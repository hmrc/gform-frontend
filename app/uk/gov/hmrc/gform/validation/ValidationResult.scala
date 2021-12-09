/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.validation

import cats.Monoid
import uk.gov.hmrc.gform.models.{ DataExpanded, Singleton }
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.ops.FormComponentOps
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, ValidatorsResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId }

case class ValidationResult(
  lookup: Map[FormComponentId, FormFieldValidationResult],
  validatorsResult: Option[ValidatorsResult]
) {

  val isFormValid: Boolean = lookup.values.view.forall(_.isOk)

  val errorsFieldIds: List[FormComponentId] =
    lookup.collect {
      case (fcId, ffvr) if ffvr.fieldErrors.nonEmpty => fcId
    }.toList

  def forgetErrors: ValidationResult = new ValidationResult(lookup.mapValues(_.forgetErrors), validatorsResult)

  def apply(formComponent: FormComponent): FormFieldValidationResult =
    lookup.getOrElse(formComponent.id, FieldOk(formComponent, ""))

  def toError(formComponentId: FormComponentId, error: String): ValidationResult =
    lookup
      .get(formComponentId)
      .fold(this) { case FieldOk(formComponent, currentValue) =>
        val fieldError = FieldError(formComponent, currentValue, Set(error))
        new ValidationResult(lookup + (formComponentId -> fieldError), validatorsResult)
      }

  def formFieldValidationResults: List[FormFieldValidationResult] = lookup.values.toList

  def formFieldValidationResults(singleton: Singleton[DataExpanded]): List[FormFieldValidationResult] =
    singleton.allFormComponents.map(apply)

  private def cleanCurrentValues: List[FormFieldValidationResult] =
    lookup.map {
      case (_, FieldOk(formComponent, cv))
          if formComponent.isSterling || formComponent.isPositiveNumber || formComponent.isNumber =>
        val poundOrComma = "[£,]".r
        val cvUpd: String = poundOrComma.replaceAllIn(cv, "")
        FieldOk(formComponent, cvUpd)
      case (_, FieldOk(formComponent, cv)) if formComponent.isReferenceNumber =>
        val cvUpd: String = cv.replace(" ", "")
        FieldOk(formComponent, cvUpd)
      case (_, FieldOk(formComponent, cv)) if formComponent.isSortCode =>
        val cvUpd: String = cv
          .replaceAll("[^0-9]", "")
          .grouped(2)
          .mkString("-")
        FieldOk(formComponent, cvUpd)
      case (formComponent, ffvr) => ffvr
    }.toList

  def toFormValidationOutcome: FormValidationOutcome = {

    val formComponentValidations =
      if (isFormValid) cleanCurrentValues else formFieldValidationResults

    FormValidationOutcome(
      isFormValid,
      FormData(
        formComponentValidations.flatMap(_.convertToFormField)
      ),
      validatorsResult
    )
  }
}

object ValidationResult {
  val empty: ValidationResult = ValidationResult(Map.empty, None)

  implicit val monoid: Monoid[ValidationResult] = new Monoid[ValidationResult] {
    override def empty: ValidationResult = ValidationResult.empty

    override def combine(x: ValidationResult, y: ValidationResult): ValidationResult = ValidationResult(
      x.lookup ++ y.lookup,
      Monoid[Option[ValidatorsResult]].combine(x.validatorsResult, y.validatorsResult)
    )
  }
}
