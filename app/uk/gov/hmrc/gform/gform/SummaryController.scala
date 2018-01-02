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

import cats._
import cats.implicits._
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, Request }
import play.api.http.HttpEntity
import play.api.mvc._
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions, ErrResponder }
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService, ValidationUtil }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class SummaryController(
    i18nSupport: I18nSupport,
    auth: AuthenticatedRequestActions,
    repeatService: RepeatingComponentService,
    fileUploadService: FileUploadService,
    validationService: ValidationService,
    pdfService: PdfGeneratorService,
    gformConnector: GformConnector,
    frontendAppConfig: FrontendAppConfig,
    errResponder: ErrResponder
) extends FrontendController {

  import i18nSupport._

  def summaryById(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case sharedmodel.form.Summary => getSummaryHTML(formId, cache, lang).map(Ok(_))
      case _ => errResponder.notFound(request, "Summary was hit before status was changed.")
    }
  }

  def submit(formId: FormId, formTemplateId4Ga: FormTemplateId, totalPage: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>

      val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

      val formFieldValidationResultsF = for {
        envelope <- envelopeF
        errors <- validateForm(cache, envelope, cache.retrievals)
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
        case "Exit" :: Nil        => Ok(save_acknowledgement(formId, cache.formTemplate, totalPage, lang, frontendAppConfig)).pure[Future]
        case "Declaration" :: Nil => handleDeclaration
        case _                    => BadRequest("Cannot determine action").pure[Future]
        // format: ON
      }
    }
  }

  def downloadPDF(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case InProgress | Summary =>
        // format: OFF
        for {
          summaryHml <- getSummaryHTML(formId, cache, lang)
          htmlForPDF = pdfService.sanitiseHtmlForPDF(summaryHml)
          pdfStream <- pdfService.generatePDF(htmlForPDF)
        } yield Result(
          header = ResponseHeader(200, Map.empty),
          body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
        )
      // format: ON
      case _ => Future.successful(BadRequest)
    }
  }

  private def validateForm(cache: AuthCacheWithForm, envelope: Envelope, retrievals: Retrievals)(implicit hc: HeaderCarrier): Future[(ValidatedType, Map[FormComponent, FormFieldValidationResult])] = {
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)
    val filteredSections = sectionsF.map(_.filter(x => BooleanExpr.isTrue(x.includeIf.map(_.expr).getOrElse(IsTrue), data, retrievals)))
    for {// format: OFF
      sections          <- filteredSections
      allFields         =  sections.flatMap(repeatService.atomicFields)
      v1                <- sections.map(x => validationService.validateForm(allFields, x, cache.form.envelopeId)(data)).sequenceU.map(Monoid[ValidatedType].combineAll)
      v                 =  Monoid.combine(
                             v1,
                             ValidationUtil.validateFileUploadHasScannedFiles(allFields, envelope)
                           )
      errors            = validationService.evaluateValidation(v, allFields, data, envelope).toMap
      // format: ON
    } yield (v, errors)
  }

  def getSummaryHTML(formId: FormId, cache: AuthCacheWithForm, lang: Option[String])(implicit request: Request[_]): Future[Html] = {
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

    // format: OFF
    for {
      envelope          <- envelopeF
      (v, _)            <- validateForm(cache, envelope, cache.retrievals)
      result            <- SummaryRenderingService.renderSummary(cache.formTemplate, v, data, cache.retrievals, formId, repeatService, envelope, lang, frontendAppConfig)
    } yield result
  }
}
