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

import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationPrint
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class PrintSectionController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  summaryRenderingService: SummaryRenderingService,
  messagesControllerComponents: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def showPrintSection(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.ViewPrintSection
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      cache.formTemplate.destinations match {
        case destinationPrint: DestinationPrint =>
          Future.successful(Ok(renderPrintSection(cache, maybeAccessCode, destinationPrint)))
        case _ =>
          Future.failed(new BadRequestException(s"Print section is not defined for ${cache.formTemplateId}"))
      }
    }

  private def renderPrintSection(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    destinationPrint: DestinationPrint
  )(implicit request: Request[_], l: LangADT, sse: SmartStringEvaluator) = {
    import i18nSupport._
    renderer.renderPrintSection(maybeAccessCode, cache.formTemplate, destinationPrint)
  }

  def downloadPDF(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadPrintSectionPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      cache.formTemplate.destinations match {
        case DestinationPrint(_, pdf, _) =>
          import i18nSupport._
          for {
            htmlForPDF <- summaryRenderingService
                            .createHtmlForPrintPdf(
                              maybeAccessCode,
                              cache,
                              SummaryPagePurpose.ForUser,
                              pdf,
                              formModelOptics
                            )
            pdfStream <- pdfService.generatePDF(htmlForPDF)
          } yield Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
          )

        case _ => Future.failed(new BadRequestException(s"Print section is not defined for ${cache.formTemplateId}"))
      }
    }

  def downloadNotificationPDF(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadPrintSectionPdf
    ) { implicit request => implicit l => cache => implicit sse => formModelOptics =>
      cache.formTemplate.destinations match {
        case DestinationPrint(_, _, Some(pdfNotification)) =>
          import i18nSupport._
          for {
            htmlForPDF <- summaryRenderingService
                            .createHtmlForNotificationPdf(
                              maybeAccessCode,
                              cache,
                              SummaryPagePurpose.ForUser,
                              pdfNotification,
                              formModelOptics
                            )
            pdfStream <- pdfService.generatePDF(htmlForPDF)
          } yield Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
          )

        case DestinationPrint(_, _, None) =>
          Future.failed(new BadRequestException(s"Pdf in print section is not defined for ${cache.formTemplateId}"))

        case _ => Future.failed(new BadRequestException(s"Print section is not defined for ${cache.formTemplateId}"))
      }
    }

}
