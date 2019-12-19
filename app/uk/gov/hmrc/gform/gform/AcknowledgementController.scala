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
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, ResponseHeader, Result }
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.graph.CustomerIdRecalculation
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.summary.{ SubmissionDetails, SummaryRenderingService }
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class AcknowledgementController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  summaryRenderingService: SummaryRenderingService,
  gformConnector: GformConnector,
  nonRepudiationHelpers: NonRepudiationHelpers,
  messagesControllerComponents: MessagesControllerComponents,
  customerIdRecalulation: CustomerIdRecalculation[Future],
  auditService: AuditService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def showAcknowledgement(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId
  ): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.ViewAcknowledgement) {
      implicit request => implicit l => cache => implicit sse =>
        import i18nSupport._
        renderer
          .renderAcknowledgementSection(maybeAccessCode, cache.formTemplate, cache.retrievals, cache.form.envelopeId)
          .map(Ok(_))
    }

  def downloadPDF(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.ViewAcknowledgement) {
      implicit request => implicit l => cache => implicit sse =>
        val formString = nonRepudiationHelpers.formDataToJson(cache.form)
        val hashedValue = nonRepudiationHelpers.computeHash(formString)

        for {
          customerId <- customerIdRecalulation.evaluateCustomerId(cache)
          eventId = auditService
            .calculateSubmissionEvent(cache.form, cache.formTemplate, cache.retrievals, customerId)
            .eventId
          _          <- nonRepudiationHelpers.sendAuditEvent(hashedValue, formString, eventId)
          submission <- gformConnector.submissionDetails(FormIdData(cache.retrievals, formTemplateId, maybeAccessCode))
          htmlForPDF <- summaryRenderingService
                         .createHtmlForPdf(
                           maybeAccessCode,
                           cache,
                           Some(SubmissionDetails(submission, hashedValue)),
                           SummaryPagePurpose.ForUser)
          pdfStream <- pdfService.generatePDF(htmlForPDF)
        } yield
          Result(
            header = ResponseHeader(200, Map.empty),
            body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
          )
    }

  def exitSurvey(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    Action.async { implicit request =>
      Future.successful(Redirect(s"/feedback/${formTemplateId.value}").withNewSession)
    }

}
