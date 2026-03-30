/*
 * Copyright 2023 HM Revenue & Customs
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

import play.api.i18n.Messages
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.objectStore.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ EnteredVariadicFormData, FastForward, ProcessData }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, SectionNumber, SectionOrSummary, SectionTitle4Ga, SuppressErrors }
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormControllerRequestHandler(
  formValidator: FormValidator,
  auditService: AuditService
)(implicit ec: ExecutionContext) {

  def handleSuppressErrors(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumbers: List[SectionNumber],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Mongo],
    suppressErrors: SuppressErrors
  ): Future[FormHandlerResult] =
    formValidator
      .validatePageModelBySectionNumbers(
        formModelOptics,
        sectionNumbers,
        cache,
        envelope,
        validatePageModel
      )
      .map(suppressErrors.apply)

  def handleFormValidation(
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    sectionNumber: SectionNumber,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    enteredVariadicFormData: EnteredVariadicFormData,
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit
    hc: HeaderCarrier,
    lang: LangADT,
    sse: SmartStringEvaluator,
    messages: Messages
  ): Future[FormValidationOutcome] =
    for {
      formHandlerResult <- formValidator.validatePageModelBySectionNumber(
                             formModelOptics,
                             sectionNumber,
                             cache,
                             envelope,
                             validatePageModel
                           )

      outcome = formValidator.toFormValidationOutcome(formHandlerResult, enteredVariadicFormData)

      _ <- if (!outcome.isValid) {
             val pageModel = formModelOptics.formModelVisibilityOptics.formModel(sectionNumber)
             val sectionTitle = SectionTitle4Ga.sectionTitle4GaFactory(pageModel, sectionNumber).value
             val submissionRef =
               SubmissionRef(cache.formTemplate, form.envelopeId, formModelOptics.formModelVisibilityOptics)
             auditFormValidationErrors(formHandlerResult, form, retrievals, sectionTitle, submissionRef)
           } else {
             Future.successful(())
           }

    } yield outcome

  private def auditFormValidationErrors(
    formHandlerResult: FormHandlerResult,
    form: Form,
    retrievals: MaterialisedRetrievals,
    sectionTitle: String,
    submissionRef: SubmissionRef
  )(implicit hc: HeaderCarrier, lang: LangADT): Future[Unit] = {
    val validationErrors: Map[FormComponentId, List[String]] =
      formHandlerResult.validationResult.lookup.collect {
        case (fcId, ffvr) if ffvr.fieldErrors.nonEmpty =>
          fcId -> ffvr.fieldErrors.toList
      }

    if (validationErrors.nonEmpty) {
      Future {
        auditService.sendFormValidationErrorEvent(
          form,
          Map(sectionTitle -> validationErrors),
          retrievals,
          submissionRef
        )
      }
    } else {
      Future.successful(())
    }
  }

  def handleFastForwardValidate(
    processData: ProcessData,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    fastForward: List[FastForward],
    maybeSectionNumber: Option[SectionNumber]
  ): Future[SectionOrSummary] =
    formValidator.fastForwardValidate(
      processData,
      cache,
      envelope,
      validatePageModel,
      fastForward,
      maybeSectionNumber
    )
  def handleMaybeGetInvalidSectionNumber(
    processData: ProcessData,
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    maybeSectionNumber: Option[SectionNumber]
  ): Future[Option[SectionNumber]] =
    formValidator.mustBeVisitedSectionNumber(
      processData,
      cache,
      envelope,
      validatePageModel,
      maybeSectionNumber
    )
}

case class FormHandlerResult(
  validationResult: ValidationResult,
  envelope: EnvelopeWithMapping
)
