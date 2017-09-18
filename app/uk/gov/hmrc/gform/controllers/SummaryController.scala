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
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, InProgress, UserData, Validated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
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
  pdfGeneratorModule: PdfGeneratorModule,
  gformBackendModule: GformBackendModule
)(implicit ec: ExecutionContext)
    extends FrontendController {

  import controllersModule.i18nSupport._

  def summaryById(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case InProgress => getSummaryHTML(formId, cache, lang).map(Ok(_))
      case _ => Future.successful(BadRequest)
    }
  }

  def submit(formId: FormId, formTemplateId4Ga: FormTemplateId, totalPage: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>

      val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

      val formFieldValidationResultsF = for {
        envelope <- envelopeF
        errors <- validateForm(cache, envelope)
      } yield errors

      val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(x => ValidationUtil.isFormValid(x._2))

      lazy val redirectToDeclaration = gformConnector
        .updateUserData(formId, UserData(cache.form.formData, cache.form.repeatingGroupStructure, Validated))
        .map { _ =>
          Redirect(routes.DeclarationController.showDeclaration(formId, formTemplateId4Ga, lang))
        }
      lazy val redirectToSummary = Redirect(routes.SummaryController.summaryById(formId, formTemplateId4Ga, lang))
      lazy val handleDeclaration = for {
        // format: OFF
        result <- isFormValidF.ifM(
          redirectToDeclaration,
          redirectToSummary.pure[Future]
        )
        // format: ON
      } yield result

      get(data, FormComponentId("save")) match {
        // format: OFF
        case "Exit" :: Nil        => Ok(save_acknowledgement(formId, formTemplateId4Ga, totalPage, lang)).pure[Future]
        case "Declaration" :: Nil => handleDeclaration
        case _                    => BadRequest("Cannot determine action").pure[Future]
        // format: ON
      }
    }
  }

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case InProgress =>
        // format: OFF
        for {
          summaryHml <- getSummaryHTML(formId, cache, lang)
          htmlForPDF = pdfService.sanitiseHtmlForPDF(summaryHml)
          pdf <- pdfService.generatePDF(htmlForPDF)
        } yield Ok(pdf).as("application/pdf")
      // format: ON
      case _ => Future.successful(BadRequest)
    }
  }

  private def validateForm(cache: AuthCacheWithForm, envelope: Envelope)(implicit hc: HeaderCarrier): Future[(ValidatedType, Map[FormComponent, FormFieldValidationResult])] = {
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)
    val filteredSections = sectionsF.map(_.filter(x => BooleanExpr.isTrue(x.includeIf.map(_.expr).getOrElse(IsTrue), data)))
    for {// format: OFF
      sections          <- filteredSections
      allFields         =  sections.flatMap(repeatService.atomicFields)
      v1                 <- sections.map(x => validationService.validateForm(allFields, x, cache.form.envelopeId)(data)).sequenceU.map(Monoid[ValidatedType].combineAll)
      v                 = v1
//TODO: uncomment below lines and replace v1 with them once created test only proxy to FileUpload
// otherwise we wont be able to test it
//                         Monoid.combine(
//                          v1,
//                          ValidationUtil.validateFileUploadHasScannedFiles(allFields, envelope)
//                        )
      errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      // format: ON
    } yield (v, errors)
  }

  def getSummaryHTML(formId: FormId, cache: AuthCacheWithForm, lang: Option[String])(implicit request: Request[_]): Future[Html] = {
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

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
  private lazy val gformConnector = gformBackendModule.gformConnector
}
