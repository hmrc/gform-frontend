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
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.validated._
import org.jsoup.Jsoup
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, Request }
import play.twirl.api.HtmlFormat
import uk.gov.hmrc.gform.auditing.{ AuditService, loggingHelpers }
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ formDataMap, get, processResponseDataFromBody }
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.graph.{ EmailParameterRecalculation, RecData, Recalculation }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.submission.SubmissionRef
import uk.gov.hmrc.gform.views.html.summary.snippets._
import uk.gov.hmrc.gform.models.helpers.Fields.flattenGroups
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationWithCustomerId, Destinations }

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
  recalculation: Recalculation[Future, Throwable],
  lookupRegistry: LookupRegistry
) extends FrontendController {

  import i18nSupport._

  def showDeclaration(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
      Future.successful(cache.form.status match {
        case Validated => Ok(renderDeclarationSection(cache, maybeAccessCode))
        case _         => BadRequest
      })
    }

  private def renderDeclarationSection(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode])(
    implicit request: Request[_],
    l: LangADT) =
    renderer
      .renderDeclarationSection(
        maybeAccessCode,
        cache.form,
        cache.formTemplate,
        cache.retrievals,
        ValidationResult.empty.valid,
        FormDataRecalculated.empty,
        Nil)

  def submitDeclaration(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cacheOrig =>
      processResponseDataFromBody(request) { dataRaw: Map[FormComponentId, Seq[String]] =>
        get(dataRaw, FormComponentId("save")) match {
          case "Continue" :: Nil => continueToSubmitDeclaration(cacheOrig, dataRaw, maybeAccessCode)
          case _                 => Future.successful(BadRequest("Cannot determine action"))
        }
      }
    }

  private def continueToSubmitDeclaration(
    cache: AuthCacheWithForm,
    dataRaw: Map[FormComponentId, Seq[String]],
    maybeAccessCode: Option[AccessCode])(implicit request: Request[_], l: LangADT) = {

    val declarationData = FormDataRecalculated(Set.empty, RecData.fromData(dataRaw))
    for {
      cacheWithHiddenSectionDataRemoved <- removeHiddenSectionData(cache)
      valRes                            <- validationService.validateComponentsWithCache(cacheWithHiddenSectionDataRemoved, declarationData)
      response                          <- processValidation(valRes, maybeAccessCode, cacheWithHiddenSectionDataRemoved, declarationData)
    } yield response
  }

  private def removeHiddenSectionData(cache: AuthCacheWithForm)(implicit hc: HeaderCarrier) =
    recalculateFormData(cache).map { data =>
      val visibleFields: Seq[FormField] = VisibleFieldCalculator(cache.formTemplate, cache.form.formData, data)
      val updatedForm = cache.form.copy(formData = cache.form.formData.copy(fields = visibleFields))
      cache.copy(form = updatedForm)
    }

  private def recalculateFormData(cache: AuthCacheWithForm)(implicit hc: HeaderCarrier) =
    recalculation.recalculateFormData(
      extractFormDataFields(cache),
      cache.formTemplate,
      cache.retrievals,
      cache.form.thirdPartyData,
      cache.form.envelopeId)

  private def extractFormDataFields(cache: AuthCacheWithForm) =
    cache.form.formData.fields.map {
      case FormField(id, value) => id -> (value :: Nil)
    }.toMap

  private def processValidation(
    valType: ValidatedType[Unit],
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated
  )(implicit request: Request[_], l: LangADT) = valType match {
    case Valid(())                     => processValid(cache, data, maybeAccessCode)
    case validationResult @ Invalid(_) => processInvalid(maybeAccessCode, cache, data, validationResult)
  }

  private def processInvalid(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    validationResult: Invalid[GformError])(implicit request: Request[_], l: LangADT) =
    Future.successful(Ok(createHtmlForInvalidSubmission(maybeAccessCode, cache, data, validationResult)))

  private def processValid(cache: AuthCacheWithForm, data: FormDataRecalculated, maybeAccessCode: Option[AccessCode])(
    implicit request: Request[_],
    l: LangADT) = {
    val updatedCache = cache.copy(form = updateFormWithDeclaration(cache.form, cache.formTemplate, data))
    for {
      _          <- updateUserData(updatedCache.form)
      customerId <- evaluateSubmissionReference(updatedCache)
      _          <- handleSubmission(maybeAccessCode, updatedCache, data, customerId)
    } yield showAcknowledgement(updatedCache, maybeAccessCode, customerId)
  }

  private def showAcknowledgement(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode], customerId: String)(
    implicit request: Request[_]) = {
    if (customerId.isEmpty)
      Logger.warn(s"DMS submission with empty customerId ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    val submissionEventId = auditSubmissionEvent(cache, customerId)

    Redirect(
      uk.gov.hmrc.gform.gform.routes.AcknowledgementController
        .showAcknowledgement(maybeAccessCode, cache.form.formTemplateId, submissionEventId))
  }

  private def auditSubmissionEvent(cache: AuthCacheWithForm, customerId: String)(implicit request: Request[_]) =
    auditService.sendSubmissionEvent(
      cache.form,
      cache.formTemplate.sections :+ cache.formTemplate.declarationSection,
      cache.retrievals,
      customerId)

  private def updateUserData(updatedForm: Form)(implicit hc: HeaderCarrier) =
    gformConnector
      .updateUserData(
        updatedForm._id,
        UserData(
          updatedForm.formData,
          Signed,
          updatedForm.visitsIndex,
          updatedForm.thirdPartyData
        )
      )

  private def evaluateSubmissionReference(cache: AuthCacheWithForm)(implicit hc: HeaderCarrier) =
    customerIds(cache.formTemplate.destinations)
      .traverse { cid =>
        authService.evaluateSubmissionReference(
          cid,
          cache.retrievals,
          cache.formTemplate,
          formDataMap(cache.form.formData),
          cache.form.envelopeId)
      }
      .map(_.filter(!_.isEmpty).headOption.getOrElse(""))

  private def customerIds(destinations: Destinations) = destinations match {
    case d: Destinations.DmsSubmission => List(d.customerId)
    case ds: Destinations.DestinationList =>
      ds.destinations.collect { case (d: DestinationWithCustomerId) => d.customerId }
  }

  private def handleSubmission(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    customerId: String)(implicit request: Request[_], l: LangADT) =
    for {
      htmlForPDF     <- createHtmlForPdf(maybeAccessCode, cache, data)
      emailParameter <- EmailParameterRecalculation(cache).recalculateEmailParameters(recalculation)
      _ <- GformSubmission
            .handleSubmission(
              config,
              gformConnector,
              cache.retrievals,
              cache.formTemplate,
              emailParameter,
              maybeAccessCode,
              CustomerId(customerId),
              htmlForPDF,
              StructuredFormDataBuilder(cache.form, cache.formTemplate, lookupRegistry)
            )
    } yield ()

  private def createHtmlForPdf(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated)(implicit request: Request[_], l: LangADT) =
    for {
      summaryHtml <- summaryController.getSummaryHTML(cache.form.formTemplateId, maybeAccessCode, cache)
    } yield
      addExtraDataToHTML(
        pdfService.sanitiseHtmlForPDF(summaryHtml, submitted = true),
        cache.formTemplate.authConfig,
        cache.formTemplate.submissionReference,
        cache.retrievals,
        cache.formTemplate,
        data,
        cache.form.envelopeId
      )

  private def createHtmlForInvalidSubmission(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    validationResult: Invalid[GformError])(implicit request: Request[_], l: LangADT) =
    renderer.renderDeclarationSection(
      maybeAccessCode,
      cache.form,
      cache.formTemplate,
      cache.retrievals,
      validationResult,
      data,
      getErrorMap(validationResult, data, cache.formTemplate))

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: FormDataRecalculated) = {
    val fieldNames = data.data.keySet.map(_.value)
    val allDeclarationFields = Fields.flattenGroups(formTemplate.declarationSection.fields)
    val submissibleFormFields = allDeclarationFields.flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormField(FormComponentId(name), data.data(FormComponentId(name)).head))
    }
    val updatedFields = form.formData.fields ++ submissibleFormFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def getErrorMap(
    validationResult: ValidatedType[ValidationResult],
    data: FormDataRecalculated,
    formTemplate: FormTemplate): List[(FormComponent, FormFieldValidationResult)] = {
    val declarationFields = Fields.flattenGroups(formTemplate.declarationSection.fields)
    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope(Nil))
  }

  //todo try and refactor the two addExtraDataToHTML into one method
  private def addExtraDataToHTML(
    html: String,
    authConfig: AuthConfig,
    submissionReference: Option[TextExpression],
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    data: FormDataRecalculated,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier, l: LangADT): String = {
    val rows = cya_row("Submission reference", SubmissionRef(envelopeId).toString)
    val extraData = cya_section("Submission details", rows).toString()
    val declaration: List[(FormComponent, Seq[String])] = for {
      formTemplateDecField <- flattenGroups(formTemplate.declarationSection.fields)
      formData             <- data.data.get(formTemplateDecField.id)
    } yield (formTemplateDecField, formData)
    val declarationExtraData = cya_section("Declaration details", HtmlFormat.fill(declaration.map {
      case (formDecFields, formData) => cya_row(formDecFields.label.value, formData.mkString)
    })).toString()
    val doc = Jsoup.parse(html)
    doc.select("article[class*=content__body]").append(extraData)
    doc.select("article[class*=content__body]").append(declarationExtraData)
    doc.html
  }
}
