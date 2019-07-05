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

import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, ResponseHeader, Result }
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.summary.{ SubmissionDetails, SummaryRenderingService }

import scala.concurrent.Future

class AcknowledgementController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  summaryRenderingService: SummaryRenderingService,
  authService: AuthService,
  gformConnector: GformConnector,
  nonRepudiationHelpers: NonRepudiationHelpers
) extends FrontendController {

  def showAcknowledgement(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    eventId: String
  ): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
      import i18nSupport._
      cache.form.status match {
        case Submitted | NeedsReview =>
          renderer
            .renderAcknowledgementSection(
              maybeAccessCode,
              cache.formTemplate,
              cache.retrievals,
              eventId,
              cache.form.envelopeId)
            .map(Ok(_))
        case _ => Future.successful(BadRequest)
      }
    }

  def downloadPDF(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    eventId: String): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
      cache.form.status match {
        case Submitted =>
          // format: OFF
          val formString  =  nonRepudiationHelpers.formDataToJson(cache.form)
          val hashedValue =  nonRepudiationHelpers.computeHash(formString)
          nonRepudiationHelpers.sendAuditEvent(hashedValue, formString, eventId)

          for {
            submission <- gformConnector.submissionStatus(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
            htmlForPDF <- summaryRenderingService.createHtmlForPdf(maybeAccessCode, cache, Some(SubmissionDetails(submission, hashedValue)))
            pdfStream  <- pdfService.generatePDF(htmlForPDF)
          } yield Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
          )
      // format: ON
        case _ => Future.successful(BadRequest)
      }
    }

  def exitSurvey(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    Action.async { implicit request =>
      Future.successful(Redirect(s"/feedback/${formTemplateId.value}").withNewSession)
    }

}
