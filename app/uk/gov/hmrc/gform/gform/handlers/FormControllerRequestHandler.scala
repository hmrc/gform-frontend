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

package uk.gov.hmrc.gform.gform.handlers

import cats.syntax.validated._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FastForward, FormModel, ProcessData }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormTemplate, Section, SectionNumber, SuppressErrors }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationResult }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

class FormControllerRequestHandler(formValidator: FormValidator)(implicit ec: ExecutionContext) {

  def handleSuppressErrors(
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    sectionNumber: SectionNumber,
    cache: CacheData,
    envelope: Envelope,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Mongo],
    suppressErrors: SuppressErrors
  )(
    implicit hc: HeaderCarrier
  ): Future[FormHandlerResult] =
    formValidator
      .validatePageModelBySectionNumber(
        formModelOptics,
        sectionNumber,
        cache,
        envelope,
        validatePageModel
      )
      .map(suppressErrors.apply)

  def handleFormValidation(
    formModelOptics: FormModelOptics[DataOrigin.Browser],
    sectionNumber: SectionNumber,
    cache: CacheData,
    envelope: Envelope,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser]
  )(
    implicit hc: HeaderCarrier
  ): Future[FormValidationOutcome] =
    formValidator
      .validatePageModelBySectionNumber(
        formModelOptics,
        sectionNumber,
        cache,
        envelope,
        validatePageModel
      )
      .map(formValidator.toFormValidationOutcome)

  def handleFastForwardValidate(
    processData: ProcessData,
    cache: CacheData,
    envelope: Envelope,
    validatePageModel: ValidatePageModel[Future, DataOrigin.Browser],
    fastForward: FastForward
  )(
    implicit hc: HeaderCarrier
  ): Future[Option[SectionNumber]] =
    formValidator.fastForwardValidate(
      processData,
      cache,
      envelope,
      validatePageModel,
      fastForward
    )
}

case class FormHandlerResult(
  validationResult: ValidationResult,
  envelope: Envelope
)
