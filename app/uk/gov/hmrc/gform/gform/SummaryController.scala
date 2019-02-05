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

import cats.Monoid
import cats.instances.future._
import cats.instances.list._
import cats.instances.map._
import cats.instances.set._
import cats.instances.unit._
import cats.syntax.traverse._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, Request }
import play.api.http.HttpEntity
import play.api.mvc._
import play.twirl.api.Html
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions, ErrResponder, Origin }
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService, ValidationUtil }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ save_acknowledgement, save_with_access_code }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class SummaryController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  pdfService: PdfGeneratorService,
  gformConnector: GformConnector,
  frontendAppConfig: FrontendAppConfig,
  errResponder: ErrResponder,
  recalculation: Recalculation[Future, Throwable]
) extends FrontendController {

  import i18nSupport._

  def summaryById(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      cache.form.status match {
        case Summary | Validated | Signed =>
          getSummaryHTML(formTemplateId, maybeAccessCode, cache, lang).map(Ok(_))
        case _ => errResponder.notFound(request, "Summary was hit before status was changed.")
      }
    }

  def submit(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], lang: Option[String]) =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      processResponseDataFromBody(request) { (dataRaw: Map[FormComponentId, Seq[String]]) =>
        val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

        val formFieldValidationResultsF = for {
          envelope <- envelopeF
          errors   <- validateForm(cache, envelope, cache.retrievals)
        } yield errors

        val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(x => ValidationUtil.isFormValid(x._2))

        lazy val redirectToDeclaration = gformConnector
          .updateUserData(
            FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode),
            UserData(cache.form.formData, Validated))
          .map { _ =>
            Redirect(
              routes.DeclarationController
                .showDeclaration(maybeAccessCode, formTemplateId, lang))
          }
        lazy val redirectToSummary =
          Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, lang))
        lazy val handleDeclaration = for {
          result <- isFormValidF.ifM(
                     redirectToDeclaration,
                     redirectToSummary.pure[Future]
                   )
        } yield result
        val envelopeExpiryDate = cache.form.envelopeExpiryDate
        lazy val handleExit = recalculation.recalculateFormData(dataRaw, cache.formTemplate, cache.retrievals).map {
          data =>
            maybeAccessCode match {
              case (Some(accessCode)) =>
                Ok(save_with_access_code(accessCode, cache.formTemplate, lang, frontendAppConfig))
              case _ =>
                val call = routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode, lang)
                Ok(save_acknowledgement(envelopeExpiryDate, cache.formTemplate, call, lang, frontendAppConfig))
            }
        }

        get(dataRaw, FormComponentId("save")) match {
          case "Exit" :: Nil        => handleExit
          case "Declaration" :: Nil => handleDeclaration
          case _                    => BadRequest("Cannot determine action").pure[Future]
        }
      }
    }

  def downloadPDF(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      cache.form.status match {
        case InProgress | Summary =>
          for {
            summaryHml <- getSummaryHTML(formTemplateId, maybeAccessCode, cache, lang)
            htmlForPDF = pdfService.sanitiseHtmlForPDF(summaryHml, submitted = false)
            pdfStream <- pdfService.generatePDF(htmlForPDF)
          } yield
            Result(
              header = ResponseHeader(200, Map.empty),
              body = HttpEntity.Streamed(pdfStream, None, Some("application/pdf"))
            )
        case _ => Future.successful(BadRequest)
      }
    }

  // TODO JoVl - why validateForm is different from validate in FormController
  private def validateForm(cache: AuthCacheWithForm, envelope: Envelope, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier): Future[(ValidatedType, Map[FormComponent, FormFieldValidationResult])] = {

    val dataRaw = FormDataHelpers.formDataMap(cache.form.formData)

    def filterSection(sections: List[Section], data: FormDataRecalculated): List[Section] =
      sections.filter(data.isVisible)

    for {
      data <- recalculation.recalculateFormData(dataRaw, cache.formTemplate, retrievals)
      allSections = RepeatingComponentService.getAllSections(cache.formTemplate, data)
      sections = filterSection(allSections, data)
      allFields = submittedFCs(data, sections.flatMap(_.expandSection(data.data).allFCs))

      v1 <- sections
             .traverse(
               section =>
                 validationService
                   .validateForm(allFields, section, cache.form.envelopeId, retrievals, cache.formTemplate)(data))
             .map(Monoid[ValidatedType].combineAll)
      v = Monoid.combine(v1, ValidationUtil.validateFileUploadHasScannedFiles(allFields, envelope))
      errors = validationService.evaluateValidation(v, allFields, data, envelope).toMap
    } yield (v, errors)

  }

  def getSummaryHTML(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    lang: Option[String])(implicit request: Request[_]): Future[Html] = {
    val dataRaw = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

    for {
      data     <- recalculation.recalculateFormData(dataRaw, cache.formTemplate, cache.retrievals)
      envelope <- envelopeF
      (v, _)   <- validateForm(cache, envelope, cache.retrievals)
    } yield
      SummaryRenderingService
        .renderSummary(cache.formTemplate, v, data, maybeAccessCode, envelope, lang, frontendAppConfig)

  }
}
