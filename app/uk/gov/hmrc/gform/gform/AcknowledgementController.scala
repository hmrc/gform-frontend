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

import java.time.format.DateTimeFormatter

import org.jsoup.Jsoup
import play.api.http.HttpEntity
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, ResponseHeader, Result }
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.sharedmodel.SubmissionReferenceUtil.getSubmissionReference

import scala.concurrent.Future

class AcknowledgementController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  summaryController: SummaryController, //TODO: does really one controller cannot exist without another one?
  authService: AuthService,
  gformConnector: GformConnector,
  nonRepudiationHelpers: NonRepudiationHelpers
) extends FrontendController {

  import i18nSupport._

  def showAcknowledgement(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    lang: Option[String],
    eventId: String
  ) =
    auth.asyncWithoutObligations(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      cache.form.status match {
        case Submitted =>
          renderer
            .renderAcknowledgementSection(maybeAccessCode, cache.formTemplate, cache.retrievals, lang, eventId)
            .map(Ok(_))
        case _ => Future.successful(BadRequest)
      }
    }

  def downloadPDF(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    lang: Option[String],
    eventId: String): Action[AnyContent] = auth.async(formTemplateId, lang, maybeAccessCode) {
    implicit request => cache =>
      cache.form.status match {
        case Submitted =>
          // format: OFF
        for {
          summaryHml  <- summaryController.getSummaryHTML(formTemplateId, maybeAccessCode, cache, lang)
          formString  =  nonRepudiationHelpers.formDataToJson(cache.form)
          hashedValue =  nonRepudiationHelpers.computeHash(formString)
          _           =  nonRepudiationHelpers.sendAuditEvent(hashedValue, formString, eventId)
          submission  <- gformConnector.submissionStatus(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
          cleanHtml   =  pdfService.sanitiseHtmlForPDF(summaryHml, submitted=true)
          data = FormDataHelpers.formDataMap(cache.form.formData)
          htmlForPDF  <- addExtraDataToHTML(cleanHtml, submission, cache.formTemplate.authConfig, cache.formTemplate.submissionReference, cache.retrievals, hashedValue, cache.formTemplate, data, cache.form.envelopeId)
          pdfStream   <- pdfService.generatePDF(htmlForPDF)
        } yield Result(
          header = ResponseHeader(200, Map.empty),
          body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
        )
      // format: ON
        case _ => Future.successful(BadRequest)
      }
  }

  private def addExtraDataToHTML(
    html: String,
    submissionDetails: Submission,
    authConfig: AuthConfig,
    submissionReference: Option[TextExpression],
    retrievals: MaterialisedRetrievals,
    hashedValue: String,
    formTemplate: FormTemplate,
    data: Map[FormComponentId, Seq[String]],
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier): Future[String] = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime =
      s"""${submissionDetails.submittedDate.format(dateFormat)} ${submissionDetails.submittedDate.format(timeFormat)}"""

    val referenceNumber = (authConfig, submissionReference) match {
      case (_, Some(textExpression)) =>
        authService.evaluateSubmissionReference(textExpression, retrievals, formTemplate, data)
      case (EeittModule(_), None) => Future.successful(authService.eeitReferenceNumber(retrievals))
      case (_, None)              => Future.successful(retrievals.getTaxIdValue(Some("HMRC-OBTDS-ORG"), "EtmpRegistrationNumber"))
    }

    referenceNumber.map { ref =>
      // TODO: Add Submission mark when it's implemented for the submission auditing event
      val extraData =
        s"""
           |<h2 class="h2-heading">Submission details</h2>
           |<dl class="govuk-check-your-answers cya-questions-long">
           |  <div>
           |    <dt class="cya-question">
           |      Submission date
           |    </dt>
           |    <dd class="cya-answer">$formattedTime</dd>
           |    <dd></dd>
           |  </div>
           |  <div>
           |    <dt class="cya-question">
           |      Submission reference
           |    </dt>
           |    <dd class="cya-answer">${getSubmissionReference(envelopeId)}</dd>
           |    <dd></dd>
           |  </div>
           |  <div>
           |    <dt class="cya-question">
           |      Submission mark
           |    </dt>
           |    <dd class="cya-answer">$hashedValue</dd>
           |    <dd></dd>
           |  </div>
           |</dl>
      """.stripMargin

      val doc = Jsoup.parse(html)
      doc.select("article[class*=content__body]").append(extraData)
      doc.html.replace("£", "&pound;")
    }
  }

  def exitSurvey(
    formTemplateId: FormTemplateId,
    lang: Option[String],
    maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    Action.async { implicit request =>
      Future.successful(Redirect(s"/feedback/${formTemplateId.value}").withNewSession)
    }

}
