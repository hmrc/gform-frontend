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
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
import uk.gov.hmrc.gform.auditing.{ AuditService, loggingHelpers }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra, Direction }
import uk.gov.hmrc.gform.fileupload.{ Attachments, Envelope, EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.{ DataExpanded, ProcessData, ProcessDataService, SectionSelector, SectionSelectorType, Singleton }
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.http.{ BadRequestException, HeaderCarrier }
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations._
import uk.gov.hmrc.gform.summary.SubmissionDetails
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class DeclarationController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  auditService: AuditService,
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  renderer: SectionRenderingService,
  gformConnector: GformConnector,
  processDataService: ProcessDataService[Future],
  gformBackEnd: GformBackEndAlgebra[Future],
  nonRepudiationHelpers: NonRepudiationHelpers,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  private def getDeclarationPage(formModelOptics: FormModelOptics[DataOrigin.Mongo]): Option[Singleton[DataExpanded]] =
    for {
      declarationPage <- formModelOptics.formModelRenderPageOptics.formModel.pages.lastOption
      singleton       <- declarationPage.fold[Option[Singleton[DataExpanded]]](Some.apply)(_ => None)
    } yield singleton

  def showDeclaration(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    suppressErrors: SuppressErrors
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithDeclaration](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewDeclaration) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      (cache.formTemplate.destinations, getDeclarationPage(formModelOptics)) match {
        case (DestinationList(_, _, _), Some(declarationPage)) =>
          getDeclarationPage(formModelOptics)
          import i18nSupport._
          for {
            validationResult <- validationService
                                 .validateDeclarationSection(
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
                  formModelOptics))
          }

        case (_, Some(_)) =>
          Future.failed(new BadRequestException(s"Declaration Section is not defined for $formTemplateId"))
        case (_, None) =>
          Future.failed(
            new BadRequestException(s"Declaration section doesn't exist: Found empty page model for $formTemplateId"))
      }
    }

  def submitDeclaration(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    save: Direction): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.WithDeclaration](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.SubmitDeclaration) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics.formModel) {
          requestRelatedData => declarationOnlyVariadicFormData =>
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
                    envelopeId,
                    envelope,
                    processData)

                case (uk.gov.hmrc.gform.controllers.Continue, _) =>
                  Future.failed(new BadRequestException(s"Declaration Section is not defined for $formTemplateId"))

                case _ =>
                  Future.successful(BadRequest("Cannot determine action"))
              }

            val processDataF: Future[ProcessData] = processDataService
              .getProcessData[SectionSelectorType.WithDeclaration](
                variadicFormData,
                cache,
                formModelOptics,
                gformConnector.getAllTaxPeriods,
                NoSpecificAction)

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
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    processData: ProcessData
  )(
    implicit
    request: Request[_],
    l: LangADT,
    lise: SmartStringEvaluator
  ): Future[Result] = {

    import i18nSupport._

    val cacheData = cache.toCacheData
    val formModelVisibilityOptics = processData.formModelOptics.formModelVisibilityOptics

    for {
      valRes <- validationService
                 .validateDeclarationSection(cacheData, formModelVisibilityOptics, envelope)
      response <- processValidation(valRes, maybeAccessCode, cache, envelopeId, envelope, processData)
    } yield response
  }

  private def cleanseEnvelope(
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    attachments: Attachments
  )(
    implicit hc: HeaderCarrier
  ): Future[Unit] = {
    val lookup: Set[FileId] =
      attachments.files.flatMap(envelope.mapping.mapping.get).toSet
    val toRemove: List[FileId] = envelope.files
      .filterNot { file =>
        lookup.contains(file.fileId)
      }
      .map(_.fileId)

    logger.warn(s"Removing ${toRemove.size} files from envelopeId $envelopeId.")

    fileUploadService.deleteFiles(envelopeId, toRemove.toSet)

  }

  private def processValidation[U <: SectionSelectorType: SectionSelector](
    validationResult: ValidationResult,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    processData: ProcessData
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): Future[Result] =
    if (validationResult.isFormValid) {
      processValid(cache, maybeAccessCode, envelopeId, envelope, processData)
    } else {
      processInvalid(maybeAccessCode, cache, validationResult, processData)
    }

  private def processInvalid(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    validationResult: ValidationResult,
    processData: ProcessData
  )(
    implicit
    request: Request[_]
  ): Future[Result] = {
    val variadicFormData: VariadicFormData[SourceOrigin.Current] = processData.formModelOptics.pageOpticsData

    val form: Form = cache.form.copy(
      formData = variadicFormData.toFormData
    )
    for {
      _ <- gformBackEnd.updateUserData(form, maybeAccessCode)
    } yield
      Redirect(routes.DeclarationController.showDeclaration(maybeAccessCode, cache.formTemplate._id, SuppressErrors.No))
  }

  private def processValid[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    envelopeId: EnvelopeId,
    envelope: EnvelopeWithMapping,
    processData: ProcessData
  )(
    implicit
    request: Request[_],
    messages: Messages,
    l: LangADT,
    lise: SmartStringEvaluator
  ): Future[Result] = {

    val formModelOptics = processData.formModelOptics

    val formModelVisibilityOptics = formModelOptics.formModelVisibilityOptics

    val attachments: Attachments = {
      val visibleFc: Set[FormComponent] = formModelVisibilityOptics.allFormComponents.toSet

      val visibleFcIds: Set[FormComponentId] = visibleFc.collect {
        case fc @ IsFileUpload() if envelope.contains(fc.modelComponentId) => fc.id
      }

      Attachments(visibleFcIds.toList)
    }

    val variadicFormData: VariadicFormData[SourceOrigin.Current] = formModelOptics.pageOpticsData

    val cacheUpd = cache.copy(form = cache.form.copy(formData = variadicFormData.toFormData))

    val submissionMark = nonRepudiationHelpers.computeHash(nonRepudiationHelpers.formDataToJson(cache.form))

    for {
      _ <- cleanseEnvelope(envelopeId, envelope, attachments)
      customerId = CustomerIdRecalculation.evaluateCustomerId(cache, formModelOptics.formModelVisibilityOptics)
      submission <- gformBackEnd.createSubmission(
                     cache.form._id,
                     cache.form.formTemplateId,
                     cache.form.envelopeId,
                     customerId.id,
                     attachments.size)
      result <- gformBackEnd
                 .submitWithUpdatedFormStatus(
                   Signed,
                   cacheUpd,
                   maybeAccessCode,
                   Some(SubmissionDetails(submission, submissionMark)),
                   customerId,
                   attachments,
                   formModelOptics)

    } yield {
      val (_, customerId) = result
      showAcknowledgement(cacheUpd, maybeAccessCode, customerId, formModelVisibilityOptics)
    }
  }

  private def showAcknowledgement(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    customerId: CustomerId,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
  )(implicit request: Request[_]) = {
    if (customerId.isEmpty)
      logger.warn(s"DMS submission with empty customerId ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    auditSubmissionEvent(cache, customerId, formModelVisibilityOptics)

    Redirect(
      uk.gov.hmrc.gform.gform.routes.AcknowledgementController
        .showAcknowledgement(maybeAccessCode, cache.form.formTemplateId))
  }

  private def auditSubmissionEvent(
    cache: AuthCacheWithForm,
    customerId: CustomerId,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser])(implicit request: Request[_]) =
    auditService.sendSubmissionEvent(cache.form, formModelVisibilityOptics, cache.retrievals, customerId)

}
