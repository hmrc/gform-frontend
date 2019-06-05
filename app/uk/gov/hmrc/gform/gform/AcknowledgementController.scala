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
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.{ Action, AnyContent, ResponseHeader, Result }
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.nonRepudiation.NonRepudiationHelpers
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId, NeedsReview, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.submission.SubmissionRef
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.models.helpers.Fields.flattenGroups

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

  def showAcknowledgement(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    eventId: String
  ) =
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
      import i18nSupport._
      cache.form.status match {
        case Submitted =>
          // format: OFF
        for {
          summaryHtml  <- summaryController.getSummaryHTML(formTemplateId, maybeAccessCode, cache)
          formString  =  nonRepudiationHelpers.formDataToJson(cache.form)
          hashedValue =  nonRepudiationHelpers.computeHash(formString)
          _           =  nonRepudiationHelpers.sendAuditEvent(hashedValue, formString, eventId)
          submission  <- gformConnector.submissionStatus(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
          cleanHtml   =  pdfService.sanitiseHtmlForPDF(summaryHtml, submitted=true)
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
  )(implicit hc: HeaderCarrier, messages: Messages, curLang: LangADT): Future[String] = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime =
      s"""${submissionDetails.submittedDate.format(dateFormat)} ${submissionDetails.submittedDate.format(timeFormat)}"""
    val rows = List(
      cya_row(messages("submission.date"), formattedTime),
      cya_row(messages("submission.reference"), SubmissionRef(envelopeId).toString),
      cya_row(messages("submission.mark"), hashedValue)
    )
    val extraData = cya_section(messages("submission.details"), HtmlFormat.fill(rows)).toString()
    val declaration: List[(FormComponent, Seq[String])] = for {
      formTemplateDecField <- flattenGroups(formTemplate.declarationSection.fields)
      formData             <- data.get(formTemplateDecField.id)
    } yield (formTemplateDecField, formData)

    val declarationExtraData = cya_section(
      messages("submission.declaration.details"),
      HtmlFormat.fill(declaration.map {
        case (formDecFields, formData) => cya_row(formDecFields.label.value, formData.mkString)
      })
    ).toString()

    val headerHtml = pdf_header(formTemplate).toString()

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").prepend(headerHtml)
    doc.select("article[class*=content__body]").append(extraData)
    doc.select("article[class*=content__body]").append(declarationExtraData)
    Future(doc.html.replace("£", "&pound;"))
  }

  def exitSurvey(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    Action.async { implicit request =>
      Future.successful(Redirect(s"/feedback/${formTemplateId.value}").withNewSession)
    }

}
