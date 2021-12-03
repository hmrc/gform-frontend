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
import cats.syntax.applicative._
import cats.syntax.flatMap._
import org.slf4j.LoggerFactory
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, OperationWithForm }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.{ EnvelopeWithMapping, FileUploadService }
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.pdf.PDFRenderService
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthConfig.hmrcSimpleModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.views.hardcoded.{ SaveAcknowledgement, SaveWithAccessCode }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ save_acknowledgement, save_with_access_code }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.pdf.model.{ PDFModel, PDFType }

import scala.concurrent.{ ExecutionContext, Future }

class SummaryController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  pdfGeneratorService: PdfGeneratorService,
  pdfRenderService: PDFRenderService,
  gformConnector: GformConnector,
  frontendAppConfig: FrontendAppConfig,
  summaryRenderingService: SummaryRenderingService,
  submissionService: SubmissionService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  import i18nSupport._

  def summaryById(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.ViewSummary) {
        implicit request => implicit l => cache => implicit sse => formModelOptics =>
          summaryRenderingService
            .getSummaryHTML(maybeAccessCode, cache, SummaryPagePurpose.ForUser, formModelOptics)
            .map(Ok(_))
      }

  def submit(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], save: Direction): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.AcceptSummary
    ) { implicit request: Request[AnyContent] => implicit l => cache => implicit sse => formModelOptics =>
      processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics) {
        requestRelatedData => variadicFormData =>
          save match {
            case Exit => handleExit(cache.formTemplateWithRedirects, maybeAccessCode, cache).pure[Future]
            case SummaryContinue =>
              handleSummaryContinue(cache.form.formTemplateId, maybeAccessCode, cache, formModelOptics)
            case _ => BadRequest("Cannot determine action").pure[Future]
          }
      }
    }

  private def handleExit(
    formTemplateWithRedirects: FormTemplateWithRedirects,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm
  )(implicit
    request: Request[AnyContent],
    l: LangADT
  ): Result = {
    val formTemplate = formTemplateWithRedirects.formTemplate
    maybeAccessCode match {
      case Some(accessCode) =>
        val saveWithAccessCode = new SaveWithAccessCode(formTemplate, accessCode)
        Ok(save_with_access_code(saveWithAccessCode, frontendAppConfig))
      case _ =>
        val config: Option[AuthConfig] =
          formTemplate.authConfig match {
            case Composite(configs) =>
              val compositeAuthDetails =
                jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
                  .get(formTemplateWithRedirects)
              AuthConfig
                .getAuthConfig(compositeAuthDetails.getOrElse(hmrcSimpleModule), configs)
            case config => Some(config)
          }

        config match {
          case Some(c) if c.isEmailAuthConfig =>
            Redirect(gform.routes.SaveAcknowledgementController.show(formTemplate._id))
          case _ =>
            val call = routes.SummaryController.summaryById(formTemplate._id, maybeAccessCode)
            val saveAcknowledgement = new SaveAcknowledgement(formTemplate, cache.form.envelopeExpiryDate)
            Ok(save_acknowledgement(saveAcknowledgement, call, frontendAppConfig))
        }
    }
  }

  private def handleSummaryContinue(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Result] = {
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

    val isFormValidF = for {
      envelope <- envelopeF
      validationResult <- validationService
                            .validateFormModel(
                              cache.toCacheData,
                              EnvelopeWithMapping(envelope, cache.form),
                              formModelOptics.formModelVisibilityOptics
                            )
    } yield validationResult.isFormValid

    def changeStateAndRedirectToDeclarationOrPrint: Future[Result] = gformConnector
      .updateUserData(
        FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
        UserData(
          cache.form.formData,
          Validated,
          cache.form.visitsIndex,
          cache.form.thirdPartyData,
          cache.form.componentIdToFileId
        )
      )
      .flatMap { _ =>
        cache.formTemplate.destinations match {
          case DestinationList(_, _, Some(declarationSection)) =>
            Redirect(
              routes.DeclarationController
                .showDeclaration(maybeAccessCode, formTemplateId, SuppressErrors.Yes)
            ).pure[Future]
          case DestinationList(_, _, None) =>
            processSubmission(maybeAccessCode, cache, formModelOptics)
          case _: DestinationPrint =>
            Redirect(
              routes.PrintSectionController
                .showPrintSection(formTemplateId, maybeAccessCode)
            ).pure[Future]
        }
      }
    val redirectToSummary: Result =
      Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode))
    for {
      result <- isFormValidF.ifM(
                  changeStateAndRedirectToDeclarationOrPrint,
                  redirectToSummary.pure[Future]
                )
    } yield result
  }

  private def processSubmission(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT,
    lise: SmartStringEvaluator
  ) =
    for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
      validationResult <- validationService
                            .validateAllSections(
                              cache.toCacheData,
                              formModelOptics.formModelVisibilityOptics,
                              EnvelopeWithMapping(envelope, cache.form)
                            )
      result <- if (validationResult.isFormValid) {
                  for {
                    customerId <- submissionService.submitForm[DataOrigin.Mongo, SectionSelectorType.Normal](
                                    cache,
                                    maybeAccessCode,
                                    EnvelopeWithMapping(envelope, cache.form),
                                    formModelOptics
                                  )
                  } yield {
                    if (customerId.isEmpty())
                      logger.warn(
                        s"DMS submission with empty customerId ${loggingHelpers.cleanHeaderCarrierHeader(hc)}"
                      )
                    Redirect(
                      uk.gov.hmrc.gform.gform.routes.AcknowledgementController
                        .showAcknowledgement(maybeAccessCode, cache.formTemplate._id)
                    )
                  }
                } else {
                  Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode)).pure[Future]
                }
    } yield result

  def downloadPDF(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      pdfRenderService
        .createPDFHtml[DataOrigin.Mongo, SectionSelectorType.Normal, PDFType.Summary](
          request.messages.messages(
            "summary.checkYourAnswers"
          ) + " - " + cache.formTemplate.formName.value + " - GOV.UK",
          Some(cache.formTemplate.summarySection.title.value()),
          cache,
          formModelOptics,
          Some(PDFModel.HeaderFooter(Some(cache.formTemplate.summarySection.header), None)),
          None,
          SummaryPagePurpose.ForUser
        )
        .flatMap(pdfGeneratorService.generatePDFLocal)
        .map { pdfSource =>
          Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfSource, None, Some("application/pdf"))
          )
        }
    }
}
