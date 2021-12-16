/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.gform

import cats.instances.future._
import org.slf4j.LoggerFactory
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra, Direction }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ Envelope, EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.{ GformBackEndAlgebra, GformConnector }
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.http.{ BadRequestException, HeaderCarrier }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class DeclarationController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  renderer: SectionRenderingService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future],
  gformBackEnd: GformBackEndAlgebra[Future],
  submissionService: SubmissionService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  private def getDeclarationPage(formModelOptics: FormModelOptics[DataOrigin.Mongo]): Option[Singleton[DataExpanded]] =
    for {
      declarationPage <- formModelOptics.formModelRenderPageOptics.formModel.pages.lastOption
      singleton       <- declarationPage.fold[Option[Singleton[DataExpanded]]](Some.apply)(_ => None)(_ => None)
    } yield singleton

  def showDeclaration(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    suppressErrors: SuppressErrors
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithDeclaration](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewDeclaration
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      (cache.formTemplate.destinations, getDeclarationPage(formModelOptics)) match {
        case (DestinationList(_, _, _), Some(declarationPage)) =>
          getDeclarationPage(formModelOptics)
          import i18nSupport._
          for {
            validationResult <- validationService
                                  .validateAllSections(
                                    cache.toCacheData,
                                    formModelOptics.formModelVisibilityOptics,
                                    EnvelopeWithMapping.empty
                                  )
          } yield {
            val validationResultUpd = suppressErrors(validationResult)
            Ok(
              renderer
                .renderDeclarationSection(
                  maybeAccessCode,
                  cache.form,
                  cache.formTemplate,
                  declarationPage,
                  cache.retrievals,
                  validationResultUpd,
                  formModelOptics
                )
            )
          }

        case (_, Some(_)) =>
          Future.failed(new BadRequestException(s"Declaration Section is not defined for ${cache.formTemplateId}"))
        case (_, None) =>
          Future.failed(
            new BadRequestException(
              s"Declaration section doesn't exist: Found empty page model for ${cache.formTemplateId}"
            )
          )
      }
    }

  def submitDeclaration(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    save: Direction
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithDeclaration](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.SubmitDeclaration
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      import i18nSupport._
      processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics) {
        requestRelatedData => declarationOnlyVariadicFormData => _ =>
          val sectionsData = formModelOptics.formModelRenderPageOptics.recData.variadicFormData
            .asInstanceOf[VariadicFormData[SourceOrigin.OutOfDate]]
          val variadicFormData = sectionsData ++ declarationOnlyVariadicFormData

          val envelopeId = cache.form.envelopeId

          def processDeclaration(processData: ProcessData, envelope: EnvelopeWithMapping): Future[Result] =
            (save, cache.formTemplate.destinations) match {
              case (uk.gov.hmrc.gform.controllers.Continue, destinationList: DestinationList) =>
                continueToSubmitDeclaration[SectionSelectorType.WithDeclaration](
                  cache,
                  maybeAccessCode,
                  envelope,
                  processData
                )

              case (uk.gov.hmrc.gform.controllers.Continue, _) =>
                Future.failed(
                  new BadRequestException(s"Declaration Section is not defined for ${cache.formTemplateId}")
                )

              case _ =>
                Future.successful(BadRequest("Cannot determine action"))
            }

          val processDataF: Future[ProcessData] = processDataService
            .getProcessData[SectionSelectorType.WithDeclaration](
              variadicFormData,
              cache,
              formModelOptics,
              gformConnector.getAllTaxPeriods,
              NoSpecificAction
            )

          val envelopeF: Future[Envelope] = fileUploadService.getEnvelope(envelopeId)

          for {
            processData <- processDataF
            envelope    <- envelopeF
            res         <- processDeclaration(processData, EnvelopeWithMapping(envelope, cache.form))
          } yield res

      }
    }

  private def continueToSubmitDeclaration[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    envelope: EnvelopeWithMapping,
    processData: ProcessData
  )(implicit
    request: Request[AnyContent],
    l: LangADT,
    lise: SmartStringEvaluator
  ): Future[Result] = {

    import i18nSupport._

    for {
      valRes <- validationService
                  .validateAllSections(
                    cache.toCacheData,
                    processData.formModelOptics.formModelVisibilityOptics,
                    envelope
                  )
      response <-
        if (valRes.isFormValid) {
          processValid(cache, maybeAccessCode, envelope, processData)
        } else {
          processInvalid(maybeAccessCode, cache, valRes, processData)
        }
    } yield response
  }

  private def processValid[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    envelope: EnvelopeWithMapping,
    processData: ProcessData
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT,
    lise: SmartStringEvaluator
  ) =
    for {
      customerId <- submissionService.submitForm(cache, maybeAccessCode, envelope, processData.formModelOptics)
    } yield {
      if (customerId.isEmpty())
        logger.warn(s"DMS submission with empty customerId ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
      Redirect(
        uk.gov.hmrc.gform.gform.routes.AcknowledgementController
          .showAcknowledgement(maybeAccessCode, cache.formTemplate._id)
      )
    }

  private def processInvalid(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    validationResult: ValidationResult,
    processData: ProcessData
  )(implicit
    request: Request[_]
  ): Future[Result] = {
    val variadicFormData: VariadicFormData[SourceOrigin.Current] = processData.formModelOptics.pageOpticsData

    val form: Form = cache.form.copy(
      formData = variadicFormData.toFormData
    )

    val declarationSectionFieldIds: List[FormComponentId] = cache.formTemplate.destinations match {
      case DestinationList(_, _, ds) =>
        ds.toList.flatMap(d => d.fields.map(_.id) ++ d.fields.flatMap(_.childrenFormComponents.map(_.id)))
      case _ => Nil
    }

    for {
      _ <- gformBackEnd.updateUserData(form, maybeAccessCode)
    } yield {
      val call: Call =
        if (validationResult.errorsFieldIds.exists(declarationSectionFieldIds.contains))
          routes.DeclarationController.showDeclaration(maybeAccessCode, cache.formTemplate._id, SuppressErrors.No)
        else
          routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode)

      Redirect(call)
    }
  }
}
