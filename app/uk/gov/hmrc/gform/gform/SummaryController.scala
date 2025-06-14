/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.syntax.eq._
import cats.instances.future._
import cats.syntax.applicative._
import org.apache.xmlgraphics.util.MimeConstants
import org.slf4j.LoggerFactory
import play.api.http.HttpEntity
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc._
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.{ Envelope, EnvelopeWithMapping, ObjectStoreService }
import uk.gov.hmrc.gform.gform
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.pdf.PDFRenderService
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, PdfContent }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.{ DestinationList, DestinationPrint }
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.{ FopService, PdfGeneratorService }
import uk.gov.hmrc.gform.validation.{ ValidationResult, ValidationService }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.file_upload_limit_exceed
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.pdf.model.{ PDFModel, PDFType }
import uk.gov.hmrc.gform.tasklist.TaskListUtils

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.models.FastForward

class SummaryController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  objectStoreService: ObjectStoreService,
  validationService: ValidationService,
  fopService: FopService,
  pdfGeneratorService: PdfGeneratorService,
  pdfRenderService: PDFRenderService,
  gformConnector: GformConnector,
  frontendAppConfig: FrontendAppConfig,
  summaryRenderingService: SummaryRenderingService,
  submissionService: SubmissionService,
  messagesControllerComponents: MessagesControllerComponents,
  appConfig: AppConfig
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger = LoggerFactory.getLogger(getClass)

  import i18nSupport._

  def summaryById(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    maybeCoordinates: Option[Coordinates],
    taskCompleted: Option[
      Boolean
    ], // to check summary page is redirected from the task list page and all tasks are completed
    reachedFormSummary: Boolean = false,
    fastForward: Option[FastForward] = None
  ): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.ViewSummary) {
        implicit request => implicit l => cache => implicit sse => formModelOptics =>
          val maybeTaskSummarySection = maybeCoordinates.flatMap { coordinates =>
            TaskListUtils.withTask(
              cache.formTemplate,
              coordinates.taskSectionNumber,
              coordinates.taskNumber
            )(task => task.summarySection)
          }

          val hasVisibleSummarySection = maybeTaskSummarySection
            .fold(false)(
              _.includeIf.fold(true)(includeIf =>
                formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
              )
            )

          lazy val formSummaryPage = summaryRenderingService
            .getSummaryHTML(
              maybeAccessCode,
              cache,
              SummaryPagePurpose.ForUser,
              formModelOptics,
              None,
              None,
              Some(true)
            )
            .map(Ok(_))

          lazy val summaryPage = summaryRenderingService
            .getSummaryHTML(
              maybeAccessCode,
              cache,
              SummaryPagePurpose.ForUser,
              formModelOptics,
              maybeCoordinates,
              maybeTaskSummarySection,
              taskCompleted
            )
            .map(Ok(_))

          lazy val landingPage = Redirect(
            uk.gov.hmrc.gform.tasklist.routes.TaskListController
              .landingPage(formTemplateId, maybeAccessCode)
          ).pure[Future]

          val formSummaryFF = fastForward.fold(false) {
            case FastForward.CYA(SectionOrSummary.FormSummary) => true
            case _                                             => false
          }
          if (reachedFormSummary || formSummaryFF) {
            for {
              isValid <- isFormValid(formTemplateId, maybeAccessCode, cache, formModelOptics)
              result  <- if (!formSummaryFF && !isValid && maybeCoordinates.isDefined) landingPage else formSummaryPage
            } yield result
          } else {
            if (!formSummaryFF && maybeCoordinates.isDefined && !hasVisibleSummarySection) landingPage else summaryPage
          }
      }

  def submit(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    save: Direction,
    formDataFingerprint: String,
    maybeCoordinates: Option[Coordinates],
    taskCompleted: Option[Boolean]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.AcceptSummary
    ) { implicit request: Request[AnyContent] => implicit l => cache => implicit sse => formModelOptics =>
      processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics) {
        requestRelatedData => variadicFormData => _ =>
          save match {
            case Exit =>
              Redirect(
                gform.routes.SaveAcknowledgementController
                  .saveAndExitFromSummary(cache.formTemplateContext.formTemplate._id, maybeAccessCode, maybeCoordinates)
              ).pure[Future]
            case SummaryContinue =>
              handleSummaryContinue(
                cache.form.formTemplateId,
                maybeAccessCode,
                cache,
                formModelOptics,
                formDataFingerprint,
                maybeCoordinates,
                taskCompleted
              )
            case _ => BadRequest("Cannot determine action").pure[Future]
          }
      }
    }

  def handleSummaryContinue(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    formDataFingerprint: String,
    maybeCoordinates: Option[Coordinates],
    taskCompleted: Option[Boolean]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Result] = {
    val envelopeF = objectStoreService.getEnvelope(cache.form.envelopeId)

    def changeStateAndRedirectToDeclarationOrPrint: Future[Result] = gformConnector
      .updateUserData(
        FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
        UserData(
          cache.form.formData,
          Validated,
          cache.form.visitsIndex,
          cache.form.thirdPartyData,
          cache.form.componentIdToFileId,
          cache.form.taskIdTaskStatus,
          cache.form.confirmationExpr
        )
      )
      .flatMap { _ =>
        maybeCoordinates match {
          case Some(coordinates) =>
            TaskListUtils.withTask(
              cache.formTemplate,
              coordinates.taskSectionNumber,
              coordinates.taskNumber
            ) { task =>
              Redirect(
                task.declarationSection.fold(
                  uk.gov.hmrc.gform.tasklist.routes.TaskListController
                    .landingPage(formTemplateId, maybeAccessCode)
                ) { taskDeclaration =>
                  val isTaskDeclarationVisible = taskDeclaration.includeIf.fold(true)(includeIf =>
                    formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
                  )

                  if (isTaskDeclarationVisible) {
                    uk.gov.hmrc.gform.tasklist.routes.TaskListController
                      .showDeclaration(
                        maybeAccessCode,
                        cache.formTemplate._id,
                        coordinates,
                        taskCompleted
                      )
                  } else {
                    uk.gov.hmrc.gform.tasklist.routes.TaskListController
                      .landingPage(formTemplateId, maybeAccessCode)
                  }
                }
              ).pure[Future]
            }

          case None =>
            cache.formTemplate.destinations match {
              case DestinationList(_, _, Some(declarationSection)) =>
                val isDeclarationSectionVisible = declarationSection.includeIf.fold(true)(includeIf =>
                  formModelOptics.formModelVisibilityOptics.evalIncludeIfExpr(includeIf, None)
                )
                if (isDeclarationSectionVisible) {
                  Redirect(
                    routes.DeclarationController
                      .showDeclaration(maybeAccessCode, formTemplateId, SuppressErrors.Yes)
                  ).pure[Future]
                } else {
                  processSubmission(maybeAccessCode, cache, formModelOptics)
                }
              case DestinationList(_, _, None) =>
                processSubmission(maybeAccessCode, cache, formModelOptics)
              case _: DestinationPrint =>
                Redirect(
                  routes.PrintSectionController
                    .showPrintSection(formTemplateId, maybeAccessCode)
                ).pure[Future]
            }
        }

      }

    val redirectToSummary: Future[Result] =
      Redirect(
        routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, maybeCoordinates, None)
      ).pure[Future]

    def redirectToFileLimitExceed(uploadedFileSize: Long): Future[Result] =
      Redirect(
        routes.SummaryController.handleFileLimitExceeded(formTemplateId, maybeAccessCode, uploadedFileSize)
      ).pure[Future]

    def calcTotalUploadedFileSizeMB(envelope: Envelope): Long =
      envelope.files.map(_.length).sum / (1024 * 1024)

    def isFileSizeExceeded(totalSizeMB: Long, maxAllowedMB: Long): Boolean =
      totalSizeMB > maxAllowedMB

    def handleResult(
      totalUploadedFileSizeMB: Long,
      validationResult: ValidationResult,
      isTokenValid: Boolean
    ): Future[Result] =
      if (isFileSizeExceeded(totalUploadedFileSizeMB, appConfig.fileMaxUploadedSizeMB)) {
        redirectToFileLimitExceed(totalUploadedFileSizeMB)
      } else if (validationResult.isFormValid && isTokenValid) {
        changeStateAndRedirectToDeclarationOrPrint
      } else {
        redirectToSummary
      }

    for {
      envelope <- envelopeF
      validationResult <- validationService.validateFormModel(
                            cache.toCacheData,
                            EnvelopeWithMapping(envelope, cache.form),
                            formModelOptics.formModelVisibilityOptics,
                            maybeCoordinates
                          )
      isTokenValid = formDataFingerprint === cache.form.formData.fingerprint
      totalUploadedFileSizeMB = calcTotalUploadedFileSizeMB(envelope)
      result <- handleResult(totalUploadedFileSizeMB, validationResult, isTokenValid)
    } yield result
  }

  def handleFileLimitExceeded(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    uploadedFileSize: Long
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewSummary
    ) { implicit request => implicit l => cache => implicit ss => _ =>
      val formAction = routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, None, None, true, None)
      Ok(
        file_upload_limit_exceed(
          cache.formTemplate,
          maybeAccessCode,
          uploadedFileSize,
          appConfig.fileMaxUploadedSizeMB,
          frontendAppConfig,
          formAction
        )
      )
        .pure[Future]
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
      envelope <- objectStoreService.getEnvelope(cache.form.envelopeId)
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
                  Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode, None, None))
                    .pure[Future]
                }
    } yield result

  def createPDFContent(cache: AuthCacheWithForm, formModelOptics: FormModelOptics[DataOrigin.Mongo])(implicit
    request: Request[_],
    l: LangADT,
    ss: SmartStringEvaluator
  ): Future[PdfContent] = {
    val draftText = cache.formTemplate.formCategory match {
      case HMRCReturnForm => Messages("summary.pdf.formCategory.return")
      case HMRCClaimForm  => Messages("summary.pdf.formCategory.claim")
      case _              => Messages("summary.pdf.formCategory.form")
    }

    val summarySection = cache.formTemplate.summarySection

    def defaultHeaderFooter = Some(PDFModel.HeaderFooter(Some(summarySection.header), None))

    val maybeHeaderFooter = summarySection.pdf.fold(defaultHeaderFooter) { pdf =>
      val maybeHeader = if (pdf.header.nonEmpty) pdf.header else Some(summarySection.header)
      Some(PDFModel.HeaderFooter(maybeHeader, pdf.footer))
    }

    val pdfOptions = summarySection.pdf.map(pdf => PDFModel.Options(pdf.tabularFormat, None))

    pdfRenderService
      .createPDFContent[DataOrigin.Mongo, SectionSelectorType.Normal, PDFType.Summary](
        request.messages.messages(
          "summary.checkYourAnswers"
        ) + " - " + cache.formTemplate.formName.value + " - GOV.UK",
        Some(summarySection.title.value()),
        cache,
        formModelOptics,
        maybeHeaderFooter,
        None,
        SummaryPagePurpose.ForUser,
        None,
        Some(draftText),
        pdfOptions,
        Some(cache.formTemplate.formName.value)
      )
  }

  def downloadPDF(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      val pdfContentF = createPDFContent(cache, formModelOptics)

      if (cache.formTemplate.accessiblePdf) {
        for {
          pdfContent <- pdfContentF
          pdfSource  <- fopService.render(pdfContent.content)
        } yield Ok(pdfSource).as(MimeConstants.MIME_PDF)
      } else {
        for {
          pdfContent <- pdfContentF
          pdfSource  <- pdfGeneratorService.generatePDF(pdfContent)
        } yield Result(
          header = ResponseHeader(200, Map.empty),
          body = HttpEntity.Streamed(pdfSource, None, Some("application/pdf"))
        )
      }
    }

  private def isFormValid(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[Boolean] = {
    val envelopeF = objectStoreService.getEnvelope(cache.form.envelopeId)

    for {
      envelope <- envelopeF
      validationResult <- validationService
                            .validateFormModel(
                              cache.toCacheData,
                              EnvelopeWithMapping(envelope, cache.form),
                              formModelOptics.formModelVisibilityOptics,
                              None
                            )
    } yield validationResult.isFormValid
  }

}
