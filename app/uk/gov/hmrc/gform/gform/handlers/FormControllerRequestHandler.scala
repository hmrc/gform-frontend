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

import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ EnteredVariadicFormData, FastForward, ProcessData }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ SectionNumber, SuppressErrors }
import uk.gov.hmrc.gform.validation.ValidationResult

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionOrSummary

class FormControllerRequestHandler(formValidator: FormValidator)(implicit ec: ExecutionContext) {

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
    enteredVariadicFormData: EnteredVariadicFormData
  ): Future[FormValidationOutcome] =
    formValidator
      .validatePageModelBySectionNumber(
        formModelOptics,
        sectionNumber,
        cache,
        envelope,
        validatePageModel
      )
      .map(fhr => formValidator.toFormValidationOutcome(fhr, enteredVariadicFormData))

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
