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

package uk.gov.hmrc.gform.gform.handlers

import cats.syntax.validated._
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.auth.models.{ IsAgent, MaterialisedRetrievals }
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.ProcessData
import uk.gov.hmrc.gform.models.gform.{ FormComponentValidation, FormValidationOutcome }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormAccessCodeForAgents, FormComponent, FormTemplate, FormTemplateId, SeNo, SeYes, Section, SectionNumber, SectionTitle4Ga, SuppressErrors, UserId => _ }
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormControllerRequestHandler(formValidator: FormValidator)(implicit ec: ExecutionContext) {

  def handleDashboard(
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    maybeForm: Option[Form]): Redirection[Unit] =
    (formTemplate.draftRetrievalMethod, retrievals, maybeForm) match {
      case (Some(FormAccessCodeForAgents), IsAgent(), None) => NotToBeRedirected(())
      case _                                                => ToBeRedirected
    }

  def handleAccessCode(accessCode: Option[String]): Redirection[String] =
    accessCode.fold[Redirection[String]](ToBeRedirected)(NotToBeRedirected(_))

  def handleSuppressErrors(
    sectionNumber: SectionNumber,
    suppressErrors: SuppressErrors,
    cache: AuthCacheWithForm,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    data: FormDataRecalculated,
    sections: List[Section],
    envelopeF: EnvelopeId => Future[Envelope],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(implicit hc: HeaderCarrier, request: Request[AnyContent])
    : Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)] =
    suppressErrors match {
      case SeYes =>
        envelopeF(envelopeId).map((List.empty, ValidationResult.empty.valid, _))
      case SeNo =>
        handleValidate(
          data,
          sections,
          sectionNumber,
          envelopeId,
          retrievals,
          cache.form.thirdPartyData,
          cache.formTemplate,
          envelopeF,
          validateFormComponents,
          evaluateValidation)
    }

  def handleForm(
    sectionNumber: SectionNumber,
    suppressErrors: SuppressErrors,
    cache: AuthCacheWithForm,
    recalculateDataAndSections: RecalculateDataAndSections[Future],
    envelopeF: EnvelopeId => Future[Envelope],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[FormHandlerResult] = {
    val envelopeId = cache.form.envelopeId
    val retrievals = cache.retrievals

    for {
      (data, sections) <- recalculateDataAndSections(FormDataHelpers.formDataMap(cache.form.formData), cache)
      (errors, validate, envelope) <- handleSuppressErrors(
                                       sectionNumber,
                                       suppressErrors,
                                       cache,
                                       envelopeId,
                                       retrievals,
                                       data,
                                       sections,
                                       envelopeF,
                                       validateFormComponents,
                                       evaluateValidation
                                     )
    } yield FormHandlerResult(data, errors, envelope, validate, sections)
  }

  def handleFormValidation(
    data: FormDataRecalculated,
    sections: List[Section],
    sn: SectionNumber,
    cache: AuthCacheWithForm,
    extractedValidateFormHelper: (
      List[FormComponentValidation],
      ValidatedType[ValidationResult]) => FormValidationOutcome,
    envelopeF: EnvelopeId => Future[Envelope],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation
  )(
    implicit request: Request[AnyContent]
  ): Future[FormValidationOutcome] =
    formValidator.validateForm(
      data,
      sections,
      sn,
      cache,
      extractedValidateFormHelper,
      envelopeF,
      validateFormComponents,
      evaluateValidation)

  def handleFastForwardValidate(
    processData: ProcessData,
    cache: AuthCacheWithForm,
    extractedValidateFormHelper: (
      List[FormComponentValidation],
      ValidatedType[ValidationResult]) => FormValidationOutcome,
    envelopeF: EnvelopeId => Future[Envelope],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation)(
    implicit request: Request[AnyContent]
  ): Future[Option[SectionNumber]] =
    formValidator.fastForwardValidate(
      processData,
      cache,
      extractedValidateFormHelper,
      envelopeF,
      validateFormComponents,
      evaluateValidation)

  def handleValidate(
    formDataRecalculated: FormDataRecalculated,
    sections: List[Section],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    envelopeF: EnvelopeId => Future[Envelope],
    validateFormComponents: ValidateFormComponents[Future],
    evaluateValidation: EvaluateValidation)(
    implicit request: Request[AnyContent]
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)] =
    formValidator.validate(
      formDataRecalculated,
      sections,
      sectionNumber,
      envelopeId,
      retrievals,
      thirdPartyData,
      formTemplate,
      envelopeF,
      validateFormComponents,
      evaluateValidation)
}

case class FormHandlerResult(
  data: FormDataRecalculated,
  result: List[(FormComponent, FormFieldValidationResult)],
  envelope: Envelope,
  validatedType: ValidatedType[ValidationResult],
  sections: List[Section])
