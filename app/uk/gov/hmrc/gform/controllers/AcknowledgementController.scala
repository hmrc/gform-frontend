/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{ Inject, Singleton }

import org.jsoup.Jsoup
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.service.SectionRenderingService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorModule
import uk.gov.hmrc.play.frontend.controller.FrontendController

@Singleton
class AcknowledgementController @Inject() (
    controllersModule: ControllersModule,
    renderer: SectionRenderingService,
    summaryController: SummaryController,
    gformBackendModule: GformBackendModule,
    pdfGeneratorModule: PdfGeneratorModule
) extends FrontendController {

  import controllersModule.i18nSupport._

  def showAcknowledgement(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    renderer.renderAcknowledgementSection(formId, cache.formTemplate, cache.retrievals, lang).map(Ok(_))
  }

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
      // format: OFF
      for {
        summaryHml <- summaryController.getSummaryHTML(formId, cache, lang)
        submission <- gformConnector.submissionStatus(formId)
        cleanHtml  = pdfService.sanitiseHtmlForPDF(summaryHml)
        htmlForPDF = addExtraDataToHTML(cleanHtml, submission)
        pdf <- pdfService.generatePDF(htmlForPDF)
      } yield Ok(pdf).as("application/pdf")
    // format: ON
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val pdfService = pdfGeneratorModule.pdfGeneratorService
  private lazy val gformConnector = gformBackendModule.gformConnector

  private def addExtraDataToHTML(html: String, submissionDetails: Submission): String = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime = s"""${submissionDetails.submittedDate.format(dateFormat)} ${submissionDetails.submittedDate.format(timeFormat)}"""

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
        |      <td>${submissionDetails.submissionRef.value}</td>
        |    </tr>
        |  </tbody>
        |</table>
      """.stripMargin

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").append(extraData)
    doc.html
  }
}
