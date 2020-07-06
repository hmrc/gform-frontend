/*
 * Copyright 2020 HM Revenue & Customs
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
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
import uk.gov.hmrc.gform.auditing.{ AuditService, loggingHelpers }
import uk.gov.hmrc.gform.auth.AuthService
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActionsAlgebra }
import uk.gov.hmrc.gform.fileupload.{ Attachments, Envelope, FileUploadService }
import uk.gov.hmrc.gform.graph.{ RecData, Recalculation }
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT, VariadicFormData }
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.http.{ BadRequestException, HeaderCarrier }
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations._
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class DeclarationController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActionsAlgebra[Future],
  auditService: AuditService,
  pdfService: PdfGeneratorService,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  authService: AuthService,
  recalculation: Recalculation[Future, Throwable],
  gformBackEnd: GformBackEndAlgebra[Future],
  messagesControllerComponents: MessagesControllerComponents,
  fileUploadService: FileUploadService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  def showDeclaration(maybeAccessCode: Option[AccessCode], formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.ViewDeclaration) {
      implicit request => implicit l => cache => implicit sse =>
        cache.formTemplate.destinations match {
          case destinationList: DestinationList =>
            Future.successful(Ok(renderDeclarationSection(cache, maybeAccessCode, destinationList.declarationSection)))

          case _ =>
            Future.failed(new BadRequestException(s"Declaration Section is not defined for $formTemplateId"))
        }
    }

  private def renderDeclarationSection(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    declarationSectionValue: DeclarationSection)(
    implicit request: Request[_],
    l: LangADT,
    sse: SmartStringEvaluator) = {
    import i18nSupport._

    renderer
      .renderDeclarationSection(
        maybeAccessCode,
        cache.form,
        cache.formTemplate,
        declarationSectionValue,
        cache.retrievals,
        ValidationResult.empty.valid,
        FormDataRecalculated.empty,
        Nil)
  }

  def submitDeclaration(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, maybeAccessCode, OperationWithForm.SubmitDeclaration) {
      implicit request => implicit l => cacheOrig => implicit sse =>
        val envelopeId = cacheOrig.form.envelopeId
        fileUploadService.getEnvelope(envelopeId).flatMap { envelope =>
          processResponseDataFromBody(request, cacheOrig.formTemplate) { dataRaw =>
            (dataRaw.one(FormComponentId("save")), cacheOrig.formTemplate.destinations) match {
              case (Some("Continue"), destinationList: DestinationList) =>
                continueToSubmitDeclaration(
                  cacheOrig,
                  dataRaw,
                  maybeAccessCode,
                  envelopeId,
                  envelope,
                  destinationList.declarationSection)

              case (Some("Continue"), _) =>
                Future.failed(new BadRequestException(s"Declaration Section is not defined for $formTemplateId"))

              case _ =>
                Future.successful(BadRequest("Cannot determine action"))
            }
          }
        }
    }

  private def continueToSubmitDeclaration(
    cache: AuthCacheWithForm,
    dataRaw: VariadicFormData,
    maybeAccessCode: Option[AccessCode],
    envelopeId: EnvelopeId,
    envelope: Envelope,
    declarationSectionValue: DeclarationSection)(
    implicit
    request: Request[_],
    l: LangADT,
    lise: SmartStringEvaluator) = {

    import i18nSupport._

    val declarationData = FormDataRecalculated(Set.empty, RecData.fromData(dataRaw))
    for {
      tuple <- removeHiddenSectionDataAndCalculateAttachments(cache, maybeAccessCode, envelope)
      (cacheWithHiddenSectionDataRemoved, attachments) = tuple
      _ <- cleanseEnvelope(envelopeId, envelope, attachments)
      valRes <- validationService
                 .validateComponentsWithCache(
                   cacheWithHiddenSectionDataRemoved,
                   declarationData,
                   Envelope.empty,
                   maybeAccessCode)
      response <- processValidation(
                   valRes,
                   maybeAccessCode,
                   cacheWithHiddenSectionDataRemoved,
                   declarationData,
                   attachments,
                   declarationSectionValue)
    } yield response
  }

  private def cleanseEnvelope(envelopeId: EnvelopeId, envelope: Envelope, attachments: Attachments)(
    implicit hc: HeaderCarrier): Future[List[Unit]] = {
    val lookup = attachments.files.toSet
    val toRemove = envelope.files.filterNot { file =>
      lookup.contains(file.fileId.toFieldId)
    }

    Logger.warn(s"Removing ${toRemove.size} files from envelopeId $envelopeId.")

    Future.traverse(toRemove) { file =>
      fileUploadService.deleteFile(envelopeId, file.fileId)
    }
  }

  private def removeHiddenSectionDataAndCalculateAttachments(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    envelope: Envelope)(implicit hc: HeaderCarrier) =
    recalculateFormData(cache, maybeAccessCode).map { data =>
      val (visibleFields, attachments) = VisibleFieldCalculator(cache.formTemplate, cache.form.formData, data, envelope)
      val updatedForm = cache.form.copy(formData = cache.form.formData.copy(fields = visibleFields))
      (cache.copy(form = updatedForm), attachments)
    }

  private def recalculateFormData(cache: AuthCacheWithForm, maybeAccessCode: Option[AccessCode])(
    implicit hc: HeaderCarrier) =
    recalculation.recalculateFormData(
      extractFormDataFields(cache),
      cache.formTemplate,
      cache.retrievals,
      cache.form.thirdPartyData,
      maybeAccessCode,
      cache.form.envelopeId)

  private def extractFormDataFields(cache: AuthCacheWithForm) = cache.variadicFormData

  private def processValidation(
    valType: ValidatedType[Unit],
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    attachments: Attachments,
    declarationSectionValue: DeclarationSection
  )(implicit request: Request[_], l: LangADT, lise: SmartStringEvaluator) = valType match {
    case Valid(()) => processValid(cache, data, maybeAccessCode, attachments)
    case validationResult @ Invalid(_) =>
      processInvalid(maybeAccessCode, cache, data, validationResult, declarationSectionValue)
  }

  private def processInvalid(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    validationResult: Invalid[GformError],
    declarationSectionValue: DeclarationSection)(
    implicit request: Request[_],
    l: LangADT,
    sse: SmartStringEvaluator): Future[Result] =
    Future.successful(
      Ok(createHtmlForInvalidSubmission(maybeAccessCode, cache, declarationSectionValue, data, validationResult)))

  private def processValid(
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    maybeAccessCode: Option[AccessCode],
    attachments: Attachments)(
    implicit
    request: Request[_],
    l: LangADT,
    lise: SmartStringEvaluator): Future[Result] = {
    import i18nSupport._

    cache.formTemplate.destinations match {
      case _: DestinationList => {
        val updatedCache = cache.copy(form = updateFormWithDeclaration(cache.form, cache.formTemplate, data))
        gformBackEnd
          .submitWithUpdatedFormStatus(Signed, updatedCache, maybeAccessCode, None, attachments)
          .map {
            case (_, customerId) => showAcknowledgement(updatedCache, maybeAccessCode, customerId)
          }
      }

      case _: DestinationPrint =>
        Future.successful(
          Redirect(
            uk.gov.hmrc.gform.gform.routes.PrintSectionController
              .showPrintSection(cache.form.formTemplateId, maybeAccessCode)))
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
        .showAcknowledgement(maybeAccessCode, cache.form.formTemplateId))
  }

  private def auditSubmissionEvent(cache: AuthCacheWithForm, customerId: CustomerId)(implicit request: Request[_]) =
    cache.formTemplate.destinations match {
      case destinationList: DestinationList =>
        auditService.sendSubmissionEvent(
          cache.form,
          cache.formTemplate.sections :+ destinationList.declarationSection,
          cache.retrievals,
          customerId)

      case _ =>
        auditService.sendSubmissionEvent(cache.form, cache.formTemplate.sections, cache.retrievals, customerId)
    }

  private def createHtmlForInvalidSubmission(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    declarationSectionValue: DeclarationSection,
    data: FormDataRecalculated,
    validationResult: Invalid[GformError])(implicit request: Request[_], l: LangADT, sse: SmartStringEvaluator) = {
    import i18nSupport._

    renderer.renderDeclarationSection(
      maybeAccessCode,
      cache.form,
      cache.formTemplate,
      declarationSectionValue,
      cache.retrievals,
      validationResult,
      data,
      getErrorMap(validationResult, data, cache.formTemplate)
    )
  }

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: FormDataRecalculated) = {
    val fieldNames = data.data.keySet.map(_.value)

    val allDeclarationFields = formTemplate.destinations match {
      case destinationList: DestinationList => Fields.flattenGroups(destinationList.declarationSection.fields)
      case _                                => Nil
    }

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

    val declarationFields = formTemplate.destinations match {
      case destinationList: DestinationList => Fields.flattenGroups(destinationList.declarationSection.fields)
      case _                                => Nil
    }

    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope.empty)
  }
}
