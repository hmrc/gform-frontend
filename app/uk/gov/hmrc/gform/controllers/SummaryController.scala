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
import play.api.mvc.{ Action, AnyContent, Request }
import play.twirl.api.Html
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadModule }
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FieldValue, FormTemplateId }
import uk.gov.hmrc.gform.summary.Summary
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

  def summaryById(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    getSummaryHTML(formId, cache, lang).map(Ok(_))
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
//        envelope    <- envelopeF
//        (v, _)      <- formFieldValidationResultsF
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

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    // format: OFF
    for {
      summaryHml <- getSummaryHTML(formId, cache, lang)
      htmlForPDF = pdfService.sanitiseHtmlForPDF(summaryHml)
      pdf        <- pdfService.generatePDF(htmlForPDF)
    } yield Ok(pdf).as("application/pdf")
    // format: ON
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

  def getSummaryHTML(formId: FormId, cache: AuthCacheWithForm, lang: Option[String])(implicit request: Request[_]): Future[Html] = {
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    //    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

    // format: OFF
    for {
      envelope          <- envelopeF
      (v, _)            <- validateForm(cache, envelope)
      result            <- Summary(cache.formTemplate).renderSummary(v, data, formId, repeatService, envelope, lang)
    } yield result
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val validationService = validationModule.validationService
  private lazy val pdfService = pdfGeneratorModule.pdfGeneratorService
}
