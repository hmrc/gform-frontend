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

import java.time.format.DateTimeFormatter
import javax.inject.{ Inject, Singleton }

import org.jsoup.Jsoup
import play.api.mvc.{ Action, AnyContent }
import uk.gov.hmrc.auth.core.authorise.AffinityGroup
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.auth.models.Retrievals.getTaxIdValue
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.prepop.AuthContextPrepop
import uk.gov.hmrc.gform.service.SectionRenderingService
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, Signed, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorModule
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

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
    cache.form.status match {
      case Submitted => renderer.renderAcknowledgementSection(formId, cache.formTemplate, cache.retrievals, lang).map(Ok(_))
      case _ => Future.successful(BadRequest)
    }
  }

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case Submitted =>
        // format: OFF
        for {
          summaryHml <- summaryController.getSummaryHTML(formId, cache, lang)
          submission <- gformConnector.submissionStatus(formId)
          cleanHtml  = pdfService.sanitiseHtmlForPDF(summaryHml)
          htmlForPDF = addExtraDataToHTML(cleanHtml, submission, cache.formTemplate.authConfig, cache.formTemplate.submissionReference, cache.retrievals)
          pdf <- pdfService.generatePDF(htmlForPDF)
        } yield Ok(pdf).as("application/pdf")
      // format: ON
      case _ => Future.successful(BadRequest)
    }
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val pdfService = pdfGeneratorModule.pdfGeneratorService
  private lazy val gformConnector = gformBackendModule.gformConnector

  private def addExtraDataToHTML(
    html: String,
    submissionDetails: Submission,
    authConfig: AuthConfig,
    submissionReference: Option[TextExpression],
    retrievals: Retrievals
  ): String = {
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val formattedTime = s"""${submissionDetails.submittedDate.format(dateFormat)} ${submissionDetails.submittedDate.format(timeFormat)}"""

    // format: OFF
    val referenceNumber = (authConfig, submissionReference) match {
      case (_,                  Some(textExpression)) => evaluateSubmissionReference(textExpression, retrievals)
      case (_: EEITTAuthConfig, None)                 => eeitReferenceNumber(retrievals)
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
        |      <td></td>
        |    </tr>
        |  </tbody>
        |</table>
      """.stripMargin

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").append(extraData)
    doc.html
  }

  private def eeitReferenceNumber(retrievals: Retrievals) = retrievals.userDetails.affinityGroup match {
    case AffinityGroup.Agent => retrievals.enrolments
      .getEnrolment(AuthConfig.eeittAuth)
      .fold("")(_.getIdentifier(EEITTAuthConfig.agentIdName).fold("")(_.value))
    case _ => retrievals.enrolments
      .getEnrolment(AuthConfig.eeittAuth)
      .fold("")(_.getIdentifier(EEITTAuthConfig.nonAgentIdName).fold("")(_.value))
  }

  private def evaluateSubmissionReference(expression: TextExpression, retrievals: Retrievals): String = {

    expression.expr match {
      case AuthCtx(value) =>
        val authContextPrepop = new AuthContextPrepop()
        authContextPrepop.values(value, retrievals)

      case EeittCtx(eeitt) => eeitReferenceNumber(retrievals)
      case _ => ""
    }
  }
}
