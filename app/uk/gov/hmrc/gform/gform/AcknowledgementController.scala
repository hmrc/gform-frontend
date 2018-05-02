/*
 * Copyright 2018 HM Revenue & Customs
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
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.auth.models.Retrievals.getTaxIdValue
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

class AcknowledgementController(
    i18nSupport: I18nSupport,
    auth: AuthenticatedRequestActions,
    pdfService: PdfGeneratorService,
    renderer: SectionRenderingService,
    repeatService: RepeatingComponentService,
    summaryController: SummaryController, //TODO: does really one controller cannot exist without another one?
    authService: AuthService,
    gformConnector: GformConnector,
    nonRepudiationHelpers: NonRepudiationHelpers
) extends FrontendController {

  import i18nSupport._

  def showAcknowledgement(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String], eventId: String) = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case Submitted => renderer.renderAcknowledgementSection(cache.form, cache.formTemplate, cache.retrievals, repeatService.getCache, lang, eventId).map(Ok(_))
      case _ => Future.successful(BadRequest)
    }
  }

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String], eventId: String): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case Submitted =>
        // format: OFF
        for {
          summaryHml  <- summaryController.getSummaryHTML(formId, cache, repeatService.getCache, lang)
          formString  =  nonRepudiationHelpers.formDataToJson(cache.form)
          hashedValue =  nonRepudiationHelpers.computeHash(formString)
          _           =  nonRepudiationHelpers.sendAuditEvent(hashedValue, formString, eventId)
          submission  <- gformConnector.submissionStatus(formId)
          cleanHtml   =  pdfService.sanitiseHtmlForPDF(summaryHml)
          data = FormDataHelpers.formDataMap(cache.form.formData)
          htmlForPDF  =  addExtraDataToHTML(cleanHtml, submission, cache.formTemplate.authConfig, cache.formTemplate.submissionReference, cache.retrievals, hashedValue, data)
          pdfStream <- pdfService.generatePDF(htmlForPDF)
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
    retrievals: Retrievals,
    hashedValue: String,
    data: Map[FormComponentId, Seq[String]]
  ): String = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime = s"""${submissionDetails.submittedDate.format(dateFormat)} ${submissionDetails.submittedDate.format(timeFormat)}"""

    // format: OFF
    val referenceNumber = (authConfig, submissionReference) match {
      case (_,                  Some(textExpression)) => authService.evaluateSubmissionReference(textExpression, retrievals, data)
      case (_: EEITTAuthConfig, None)                 => authService.eeitReferenceNumber(retrievals)
      case (_,                  None)                 => getTaxIdValue(Some("HMRC-OBTDS-ORG"), "EtmpRegistrationNumber", retrievals)
    }
    // format: ON

    // TODO: Add Submission mark when it's implemented for the submission auditing event
    val extraData =
      s"""
        |<table class="table--font-reset ">
        |  <thead>
        |    <tr>
        |      <th class="grid-layout__column--1-2"> <h2 class="h2-heading">Submission details</h2> </th>
        |      <th class="text--right"> </th>
        |    </tr>
        |  </thead>
        |  <tbody>
        |    <tr>
        |      <td>Submission date</td>
        |      <td>${formattedTime}</td>
        |    </tr>
        |    <tr>
        |      <td>Submission reference</td>
        |      <td>${referenceNumber}</td>
        |    </tr>
        |    <tr>
        |      <td>Submission mark</td>
        |      <td>${hashedValue}</td>
        |    </tr>
        |  </tbody>
        |</table>
      """.stripMargin

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").append(extraData)
    doc.html
  }
}
