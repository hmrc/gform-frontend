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
import cats.syntax.validated._
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, Request, Result }
import uk.gov.hmrc.gform.auditing.{ AuditService, loggingHelpers }
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation }
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, VariadicFormData }
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.Future

class DeclarationController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  auditService: AuditService,
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  authService: AuthService,
  recalculation: Recalculation[Future, Throwable],
  gformBackEnd: GformBackEndAlgebra[Future]
) extends FrontendController {

  def showDeclaration(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.ViewDeclaration) {
      implicit request => implicit l => cache =>
        Future.successful(Ok(renderDeclarationSection(cache, maybeAccessCode)))
    }

  private def renderDeclarationSection(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode])(
    implicit request: Request[_],
    l: LangADT) = {
    import i18nSupport._

    renderer
      .renderDeclarationSection(
        maybeAccessCode,
        cache.form,
        cache.formTemplate,
        cache.retrievals,
        ValidationResult.empty.valid,
        FormDataRecalculated.empty,
        Nil)
  }

  def submitDeclaration(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.SubmitDeclaration) {
      implicit request => implicit l => cacheOrig =>
        processResponseDataFromBody(request, cacheOrig.formTemplate) { dataRaw =>
          dataRaw.one(FormComponentId("save")) match {
            case Some("Continue") => continueToSubmitDeclaration(cacheOrig, dataRaw, maybeAccessCode)
            case _                => Future.successful(BadRequest("Cannot determine action"))
          }
        }
    }

  private def continueToSubmitDeclaration(
    cache: AuthCacheWithForm,
    dataRaw: VariadicFormData,
    maybeAccessCode: Option[AccessCode])(implicit request: Request[_], l: LangADT) = {

    import i18nSupport._

    val declarationData = FormDataRecalculated(Set.empty, RecData.fromData(dataRaw))
    for {
      cacheWithHiddenSectionDataRemoved <- removeHiddenSectionData(cache)
      valRes <- validationService
                 .validateComponentsWithCache(cacheWithHiddenSectionDataRemoved, declarationData, Envelope.empty)
      response <- processValidation(valRes, maybeAccessCode, cacheWithHiddenSectionDataRemoved, declarationData)
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

  private def extractFormDataFields(cache: AuthCacheWithForm) = cache.variadicFormData

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
    validationResult: Invalid[GformError])(implicit request: Request[_], l: LangADT): Future[Result] =
    Future.successful(Ok(createHtmlForInvalidSubmission(maybeAccessCode, cache, data, validationResult)))

  private def processValid(cache: AuthCacheWithForm, data: FormDataRecalculated, maybeAccessCode: Option[AccessCode])(
    implicit request: Request[_],
    l: LangADT): Future[Result] = {
    val updatedCache = cache.copy(form = updateFormWithDeclaration(cache.form, cache.formTemplate, data))
    gformBackEnd
      .submitWithUpdatedFormStatus(Signed, updatedCache, maybeAccessCode, None)
      .map {
        case (_, customerId) => showAcknowledgement(updatedCache, maybeAccessCode, customerId)
      }
  }

  private def showAcknowledgement(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    customerId: CustomerId)(implicit request: Request[_]) = {
    if (customerId.isEmpty)
      Logger.warn(s"DMS submission with empty customerId ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")

    val submissionEventId = auditSubmissionEvent(cache, customerId)

    Redirect(
      uk.gov.hmrc.gform.gform.routes.AcknowledgementController
        .showAcknowledgement(maybeAccessCode, cache.form.formTemplateId, submissionEventId))
  }

  private def auditSubmissionEvent(cache: AuthCacheWithForm, customerId: CustomerId)(implicit request: Request[_]) =
    auditService.sendSubmissionEvent(
      cache.form,
      cache.formTemplate.sections :+ cache.formTemplate.declarationSection,
      cache.retrievals,
      customerId)

  private def createHtmlForInvalidSubmission(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    validationResult: Invalid[GformError])(implicit request: Request[_], l: LangADT) = {
    import i18nSupport._

    renderer.renderDeclarationSection(
      maybeAccessCode,
      cache.form,
      cache.formTemplate,
      cache.retrievals,
      validationResult,
      data,
      getErrorMap(validationResult, data, cache.formTemplate))
  }

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: FormDataRecalculated) = {
    val fieldNames = data.data.keySet.map(_.value)
    val allDeclarationFields = Fields.flattenGroups(formTemplate.declarationSection.fields)
    val submissibleFormFields = allDeclarationFields.flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormComponentId(name))
        .map(formComponentId =>
          FormField(formComponentId, data.data.get(formComponentId).toList.flatMap(_.toSeq).mkString(",")))
    }
    val updatedFields = form.formData.fields ++ submissibleFormFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def getErrorMap(
    validationResult: ValidatedType[ValidationResult],
    data: FormDataRecalculated,
    formTemplate: FormTemplate): List[(FormComponent, FormFieldValidationResult)] = {
    val declarationFields = Fields.flattenGroups(formTemplate.declarationSection.fields)
    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope.empty)
  }
}
