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

import cats.data.Validated.{ Invalid, Valid }
import cats.instances.future._
import org.jsoup.Jsoup
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc.{ Request, Result }
import uk.gov.hmrc.gform.auditing.{ AuditService, loggingHelpers }
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals.getTaxIdValue
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ formDataMap, get, processResponseDataFromBody }
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.sharedmodel.SubmissionReferenceUtil.getSubmissionReference

import scala.concurrent.Future

class DeclarationController(
  i18nSupport: I18nSupport,
  config: FrontendAppConfig,
  auth: AuthenticatedRequestActions,
  gformConnector: GformConnector,
  auditService: AuditService,
  summaryController: SummaryController,
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  authService: AuthService,
  recalculation: Recalculation[Future, Throwable]
) extends FrontendController {

  import i18nSupport._

  def showDeclaration(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId, lang: Option[String]) =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      cache.form.status match {
        case Validated =>
          Future.successful {
            Ok {
              renderer
                .renderDeclarationSection(
                  maybeAccessCode,
                  cache.form,
                  cache.formTemplate,
                  cache.retrievals,
                  Valid(()),
                  FormDataRecalculated.empty,
                  Nil,
                  lang)
            }
          }

        case _ => Future.successful(BadRequest)
      }
    }
  //todo try and refactor the two addExtraDataToHTML into one method
  private def addExtraDataToHTML(
    html: String,
    authConfig: AuthConfig,
    submissionReference: Option[TextExpression],
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    data: FormDataRecalculated,
    envelopeId: EnvelopeId
  )(implicit hc: HeaderCarrier): String = {
    val referenceNumber = (authConfig, submissionReference) match {
      case (_, Some(textExpression)) =>
        authService.evaluateSubmissionReference(textExpression, retrievals, formTemplate, data.data)
      case (EeittModule(_), None) => authService.eeitReferenceNumber(retrievals)
      case (_, None)              => getTaxIdValue(Some("HMRC-OBTDS-ORG"), "EtmpRegistrationNumber", retrievals)
    }

    val extraData =
      s"""
         |<h2 class="h2-heading">Submission details</h2>
         |<dl class="govuk-check-your-answers cya-questions-long">
         |  <div>
         |    <dt class="cya-question">
         |      Submission reference
         |    </dt>
         |    <dd class="cya-answer">${getSubmissionReference(envelopeId)}</dd>
         |    <dd></dd>
         |  </div>
         |</dl>
      """.stripMargin

    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").append(extraData)
    doc.html
  }

  def submitDeclaration(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], lang: Option[String]) =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cacheOrig =>
      processResponseDataFromBody(request) { (dataRaw: Map[FormComponentId, Seq[String]]) =>
        val formData: Map[FormComponentId, List[String]] = cacheOrig.form.formData.fields.map {
          case FormField(id, value) => id -> (value :: Nil)
        }.toMap

        val declarationData = FormDataRecalculated(Set.empty, dataRaw)

        get(dataRaw, FormComponentId("save")) match {
          case "Continue" :: Nil =>
            for {
              data <- recalculation.recalculateFormData(
                       formData,
                       cacheOrig.formTemplate,
                       cacheOrig.retrievals,
                       cacheOrig.form.envelopeId)
              invisibleSections = cacheOrig.formTemplate.sections.filterNot(data.isVisible)

              invisibleFields: Set[FormComponentId] = invisibleSections.flatMap(_.fields).map(_.id).toSet

              visibleFields: Seq[FormField] = cacheOrig.form.formData.fields.filterNot(field =>
                invisibleFields.contains(field.id))

              form = cacheOrig.form.copy(formData = cacheOrig.form.formData.copy(fields = visibleFields))

              // This cache contains form with all fields from hidden sections removed
              cache = cacheOrig.copy(form = form)

              valRes <- validationService.validateComponents(
                         getAllDeclarationFields(cache.formTemplate.declarationSection.fields),
                         declarationData,
                         cache.form.envelopeId,
                         cache.retrievals,
                         cacheOrig.formTemplate
                       )

              response <- isValid(valRes, form.formTemplateId, maybeAccessCode, cache, declarationData, lang)

            } yield response

          case _ =>
            Future.successful(BadRequest("Cannot determine action"))
        }
      }
    }

  def isValid(
    valType: ValidatedType,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    lang: Option[String]
  )(implicit request: Request[_]): Future[Result] = valType match {

    case Valid(()) =>
      val updatedForm = updateFormWithDeclaration(cache.form, cache.formTemplate, data)
      for {
        customerId <- authService.evaluateSubmissionReference(
                       cache.formTemplate.dmsSubmission.customerId,
                       cache.retrievals,
                       cache.formTemplate,
                       formDataMap(updatedForm.formData))
        _ <- gformConnector.updateUserData(cache.form._id, UserData(updatedForm.formData, Signed))
        //todo perhaps not make these calls at all if the feature flag is false?
        summaryHml <- summaryController.getSummaryHTML(formTemplateId, maybeAccessCode, cache, lang)
        cleanHtml = pdfService.sanitiseHtmlForPDF(summaryHml, submitted = true)
        htmlForPDF = addExtraDataToHTML(
          cleanHtml,
          cache.formTemplate.authConfig,
          cache.formTemplate.submissionReference,
          cache.retrievals,
          cache.formTemplate,
          data,
          cache.form.envelopeId)
        _ <- if (config.sendPdfWithSubmission)
              gformConnector.submitFormWithPdf(
                FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode),
                customerId,
                htmlForPDF,
                cache.retrievals.affinityGroup)
            else {
              gformConnector
                .submitForm(
                  FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode),
                  customerId,
                  cache.retrievals.affinityGroup)
            }
      } yield {
        if (customerId.isEmpty)
          Logger.warn(s"DMS submission with empty customerId ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
        val submissionEventId = auditService.sendSubmissionEvent(
          cache.form,
          cache.formTemplate.sections :+ cache.formTemplate.declarationSection,
          cache.retrievals,
          customerId)
        Redirect(
          uk.gov.hmrc.gform.gform.routes.AcknowledgementController
            .showAcknowledgement(maybeAccessCode, formTemplateId, lang, submissionEventId))
      }
    case validationResult @ Invalid(_) =>
      val errorMap: List[(FormComponent, FormFieldValidationResult)] =
        getErrorMap(validationResult, data, cache.formTemplate)
      val html = renderer.renderDeclarationSection(
        maybeAccessCode,
        cache.form,
        cache.formTemplate,
        cache.retrievals,
        validationResult,
        data,
        errorMap,
        lang)
      Future.successful(Ok(html))
  }

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: FormDataRecalculated) = {
    val fieldNames = data.data.keySet.map(_.value)
    val allDeclarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    val submissibleFormFields = allDeclarationFields.flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormField(FormComponentId(name), data.data(FormComponentId(name)).head))
    }
    val updatedFields = form.formData.fields ++ submissibleFormFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def getErrorMap(
    validationResult: ValidatedType,
    data: FormDataRecalculated,
    formTemplate: FormTemplate): List[(FormComponent, FormFieldValidationResult)] = {
    val declarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope(Nil))
  }

  private def getAllDeclarationFields(fields: List[FormComponent]): List[FormComponent] =
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllDeclarationFields(grp.fields)
        case _          => List(fieldValue)
      }
    }

}
