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

import javax.inject.{ Inject, Singleton }

import cats._
import cats.implicits._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadModule }
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FieldValue, FormTemplateId }
import uk.gov.hmrc.gform.summary.Summary
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FieldValue, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.frontend.controller.FrontendController
import cats._
import cats.implicits._
import org.jsoup.Jsoup
import org.jsoup.nodes.{ Comment, Element, Node }
import org.jsoup.safety.Whitelist
import play.twirl.api.Html
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorModule
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationModule, ValidationUtil }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class SummaryController @Inject() (
  controllersModule: ControllersModule,
  repeatService: RepeatingComponentService,
  fileUploadModule: FileUploadModule,
  validationModule: ValidationModule,
  pdfGeneratorModule: PdfGeneratorModule
)(implicit ec: ExecutionContext)
    extends FrontendController {

  import controllersModule.i18nSupport._

  def summaryById(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

    for {// format: OFF
      envelope          <- envelopeF
      (v, _)            <- validateForm(cache, envelope)
      result            <- Summary(cache.formTemplate).renderSummary(v, data, formId, repeatService, envelope, lang)
      // format: ON
    } yield result
  }

  def submit(formId: FormId, formTemplateId4Ga: FormTemplateId, totalPage: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

      val formFieldValidationResultsF = for {
        envelope <- envelopeF
        errors <- validateForm(cache, envelope)
      } yield errors

      val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(x => ValidationUtil.isFormValid(x._2))

      lazy val redirectToDeclaration = Redirect(routes.DeclarationController.showDeclaration(formId, formTemplateId4Ga, lang))
      lazy val redirectToSummary = Redirect(routes.SummaryController.summaryById(formId, formTemplateId4Ga, lang))
      lazy val handleDeclaration = for {
        // format: OFF
        envelope    <- envelopeF
        (v, _)      <- formFieldValidationResultsF
        result      <- isFormValidF.ifM(
          redirectToDeclaration.pure[Future],
          redirectToSummary.pure[Future]
        )
        // format: ON
      } yield result

      get(data, FieldId("save")) match {
        // format: OFF
        case "Exit" :: Nil        => Ok(save_acknowledgement(formId, formTemplateId4Ga, totalPage, lang)).pure[Future]
        case "Declaration" :: Nil => handleDeclaration
        case _                    => BadRequest("Cannot determine action").pure[Future]
        // format: ON
      }
    }
  }

  private def validateForm(cache: AuthCacheWithForm, envelope: Envelope)(implicit hc: HeaderCarrier): Future[(ValidatedType, Map[FieldValue, FormFieldValidationResult])] = {
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)
    for {// format: OFF
      sections          <- sectionsF
      allFields         =  sections.flatMap(repeatService.atomicFields)
      v                 <- sections.map(x => validationService.validateForm(allFields, x, cache.form.envelopeId)(data)).sequenceU.map(Monoid[ValidatedType].combineAll)
      errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      // format: ON
    } yield (v, errors)
  }

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

    for {// format: OFF
      envelope <- envelopeF
      summary  <- Summary(cache.formTemplate).generateHTML(_ => None, formId, data, repeatService, envelope, lang)
      html     = sanitiseHtml(summary)
      pdf      <- pdfService.generatePDF(html)
      // format: ON
    } yield Ok(pdf).as("application/pdf")
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val validationService = validationModule.validationService
  private lazy val pdfService = pdfGeneratorModule.pdfGeneratorService

  private def sanitiseHtml(html: Html): String = {
    val doc = Jsoup.parse(html.body)
    removeComments(doc)
    doc.getElementsByTag("script").remove
    doc.getElementsByTag("a").remove
    doc.getElementsByClass("footer-wrapper").remove
    doc.getElementById("global-cookie-message").remove
    doc.getElementsByClass("print-hidden").remove

    doc.html
  }

  def removeComments(node: Node): Unit = {
    var i = 0
    while ({ i < node.childNodeSize() }) {
      val child = node.childNode(i)
      if (child.nodeName.equals("#comment")) {
        child.remove
      } else {
        removeComments(child)
        i += 1
      }
    }
  }
}
