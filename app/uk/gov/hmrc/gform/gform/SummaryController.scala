/*
 * Copyright 2019 HM Revenue & Customs
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
import org.jsoup.Jsoup
import play.api.http.HttpEntity
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, ResponseHeader, Result }
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.controllers.{ AuthenticatedRequestActionsAlgebra, ErrResponder }
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, PdfHtml }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.{ ValidationService, ValidationUtil }
import uk.gov.hmrc.gform.views.ViewHelpersAlgebra
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ save_acknowledgement, save_with_access_code }
import uk.gov.hmrc.gform.views.html.summary.snippets.pdf_header
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class SummaryController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  pdfService: PdfGeneratorService,
  gformConnector: GformConnector,
  frontendAppConfig: FrontendAppConfig,
  errResponder: ErrResponder,
  recalculation: Recalculation[Future, Throwable],
  summaryRenderingService: SummaryRenderingService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext, viewHelpers: ViewHelpersAlgebra)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def summaryById(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.ViewSummary) {
      implicit request => implicit l => cache => implicit sse =>
        summaryRenderingService
          .getSummaryHTML(formTemplateId, maybeAccessCode, cache, SummaryPagePurpose.ForUser)
          .map(Ok(_))
    }

  def submit(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.AcceptSummary) {
      implicit request: Request[AnyContent] => implicit l => cache => implicit sse =>
        processResponseDataFromBody(request, cache.formTemplate) { dataRaw =>
          val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
          val formFieldValidationResultsF = for {
            envelope <- envelopeF
            errors   <- validationService.validateForm(cache, envelope, cache.retrievals)
          } yield errors

          val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(x => ValidationUtil.isFormValid(x._2))

          lazy val redirectToDeclaration = gformConnector
            .updateUserData(
              FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
              UserData(
                cache.form.formData,
                Validated,
                cache.form.visitsIndex,
                cache.form.thirdPartyData
              )
            )
            .map { _ =>
              Redirect(
                routes.DeclarationController
                  .showDeclaration(maybeAccessCode, formTemplateId))
            }

          lazy val redirectToSummary =
            Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode))
          lazy val handleDeclaration = for {
            result <- isFormValidF.ifM(
                       redirectToDeclaration,
                       redirectToSummary.pure[Future]
                     )
          } yield result
          val envelopeExpiryDate = cache.form.envelopeExpiryDate
          lazy val handleExit = recalculation
            .recalculateFormData(
              dataRaw,
              cache.formTemplate,
              cache.retrievals,
              cache.form.thirdPartyData,
              cache.form.envelopeId)
            .map { _ =>
              maybeAccessCode match {
                case Some(accessCode) =>
                  Ok(save_with_access_code(accessCode, cache.formTemplate, frontendAppConfig))
                case _ =>
                  val call = routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode)
                  Ok(save_acknowledgement(envelopeExpiryDate, cache.formTemplate, call, frontendAppConfig))
              }
            }

          dataRaw.one(FormComponentId("save")) match {
            case Some("Exit")        => handleExit
            case Some("Declaration") => handleDeclaration
            case _                   => BadRequest("Cannot determine action").pure[Future]
          }
        }
    }

  def downloadPDF(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.DownloadSummaryPdf) {
      implicit request => implicit l => cache => implicit sse =>
        for {
          summaryHml <- summaryRenderingService
                         .getSummaryHTML(formTemplateId, maybeAccessCode, cache, SummaryPagePurpose.ForUser)
          htmlForPDF = HtmlSanitiser.sanitiseHtmlForPDF(summaryHml, submitted = false)
          withPDFHeader = pdfHeader(htmlForPDF, cache.formTemplate)
          pdfStream <- pdfService.generatePDF(PdfHtml(withPDFHeader))
        } yield
          Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
          )
    }

  private def pdfHeader(
    summaryHtml: String,
    formTemplate: FormTemplate)(implicit ec: ExecutionContext, l: LangADT, messages: Messages): String = {
    val headerHtml = pdf_header(formTemplate).toString()
    val doc = Jsoup.parse(summaryHtml)
    doc.select("article[class*=content__body]").prepend(headerHtml)
    doc.html
  }
}
