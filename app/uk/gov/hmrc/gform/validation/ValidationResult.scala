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

package uk.gov.hmrc.gform.validation

import cats.Monoid
import uk.gov.hmrc.gform.models.EnteredVariadicFormData
import uk.gov.hmrc.gform.models.Singleton
import uk.gov.hmrc.gform.models.gform.FormValidationOutcome
import uk.gov.hmrc.gform.sharedmodel.form.FormData
import uk.gov.hmrc.gform.sharedmodel.form.ValidatorsResult
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IsChoice, IsRevealingChoice }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import scala.annotation.nowarn
import scala.collection.mutable.LinkedHashSet

case class ValidationResult(
  lookup: Map[FormComponentId, FormFieldValidationResult],
  validatorsResult: Option[ValidatorsResult],
  pageErrors: List[PageError]
) {

  val isFormValid: Boolean = lookup.values.view.forall(_.isOk) && pageErrors.isEmpty

  val errorsFieldIds: List[FormComponentId] =
    lookup.collect {
      case (fcId, ffvr) if ffvr.fieldErrors.nonEmpty => fcId
    }.toList ++ pageErrors.flatMap(_.fields.toList.map(_.id))

  def forgetErrors: ValidationResult =
    new ValidationResult(lookup.view.mapValues(_.forgetErrors).toMap, validatorsResult, Nil)

  def apply(formComponent: FormComponent): FormFieldValidationResult =
    lookup.getOrElse(formComponent.id, FieldOk(formComponent, ""))

  @nowarn
  def toError(formComponentId: FormComponentId, error: String): ValidationResult =
    lookup
      .get(formComponentId)
      .fold(this) { case FieldOk(formComponent, currentValue) =>
        val fieldError = FieldError(formComponent, currentValue, LinkedHashSet(error))
        new ValidationResult(lookup + (formComponentId -> fieldError), validatorsResult, pageErrors)
      }

  def formFieldValidationResults: List[FormFieldValidationResult] = lookup.values.toList

  def formFieldValidationResults(singleton: Singleton): List[FormFieldValidationResult] =
    singleton.allFormComponents.map(apply)

  def pageLevelValidationResults(singleton: Singleton): List[PageError] =
    pageErrors.filter(error => singleton.allFormComponentIds.contains(error.fields.head.id))

  def forSummary(formComponent: FormComponent): FormFieldValidationResult =
    pageErrors.find(_.fields.head.id == formComponent.id).fold(apply(formComponent)) { error =>
      val result = apply(formComponent)
      result match {
        case ComponentField(component, data) =>
          val firstFieldId = component.multiValueId.fold(_ => HtmlFieldId.indexed(component.id, "0"))(multiple =>
            HtmlFieldId.pure(multiple.atoms.head)
          )
          val value = data.get(firstFieldId).flatMap(_.getCurrentValue).getOrElse("")
          ComponentField(
            component,
            data.updated(firstFieldId, FieldError(component, value, LinkedHashSet(error.error)))
          )
        case _ => FieldError(formComponent, result.getCurrentValue.getOrElse(""), LinkedHashSet(error.error))
      }
    }

  def toFormValidationOutcome(
    enteredVariadicFormData: EnteredVariadicFormData
  ): FormValidationOutcome = {

    val enteredFormFieldValidationResults: List[FormFieldValidationResult] = formFieldValidationResults.collect {
      case fe @ FieldError(fc, cv, _) =>
        fc match {
          case IsChoice(_) | IsRevealingChoice(_) => fe.copy(currentValue = cv)
          case _                                  => fe.copy(currentValue = enteredVariadicFormData.userData.one(fc.modelComponentId).getOrElse(cv))
        }
    }

    val formComponentValidations =
      if (isFormValid) formFieldValidationResults else enteredFormFieldValidationResults

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
  val empty: ValidationResult = ValidationResult(Map.empty, None, Nil)

  implicit val monoid: Monoid[ValidationResult] = new Monoid[ValidationResult] {
    override def empty: ValidationResult = ValidationResult.empty

    override def combine(x: ValidationResult, y: ValidationResult): ValidationResult = ValidationResult(
      x.lookup ++ y.lookup,
      Monoid[Option[ValidatorsResult]].combine(x.validatorsResult, y.validatorsResult),
      x.pageErrors ++ y.pageErrors
    )
  }

}

case class PageValidationResult(
  componentResult: ValidatedType[Unit],
  pageErrors: List[PageError],
  validatorsResult: Option[ValidatorsResult]
)
