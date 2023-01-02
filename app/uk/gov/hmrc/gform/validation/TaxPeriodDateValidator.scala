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

import cats.implicits.{ catsSyntaxValidatedId, _ }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, TaxPeriodDate }
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.{ errors, fieldDescriptor }
import uk.gov.hmrc.gform.validation.DateValidationLogic.{ hasMaximumLength, isNumeric }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

class TaxPeriodDateValidator[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequired(formComponent)
      .andThen(_ =>
        validateMonthYear(formComponent)
          .andThen(validateMonthYearCombo(formComponent, _))
      )

  def validateMonthYear(formComponent: FormComponent): ValidatedType[(Int, Int)] =
    formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
      case Some(month) :: Some(year) :: Nil =>
        val label = fieldDescriptor(formComponent, "")

        val yearValidated = hasMaximumLength(year, 4, label + " " + messages("date.year"))
          .andThen(_ => isNumeric(year, label + " " + messages("date.year"), label))
          .leftMap(error => Map(errorGranularity(formComponent)(TaxPeriodDate.year) -> Set(error)))

        val monthValidated = hasMaximumLength(month, 2, label + " " + messages("date.month"))
          .andThen(_ => isNumeric(month, label + " " + messages("date.month"), label))
          .leftMap(error => Map(errorGranularity(formComponent)(TaxPeriodDate.month) -> Set(error)))

        (monthValidated, yearValidated).mapN((month, year) => (month, year))
      case _ =>
        validationFailure(formComponent.firstAtomModelComponentId, formComponent, "date.isMissing", None, "")
    }

  private def validateMonthYearCombo(formComponent: FormComponent, monthYear: (Int, Int)): ValidatedType[Unit] = {
    val label = fieldDescriptor(formComponent, "")
    val monthLabel = label + " " + messages("date.month")
    val yearLabel = label + " " + messages("date.year")
    val (month, year) = monthYear
    validationSuccess
      .ensure(
        Map(
          errorGranularity(formComponent)(TaxPeriodDate.month) -> Set(
            messages("generic.error.mustBeBetween", monthLabel, 1, 12)
          )
        )
      )(_ => month >= 1 && month <= 12)
      .ensure(
        Map(
          errorGranularity(formComponent)(TaxPeriodDate.year) -> Set(
            messages("generic.error.mustBeBetween", yearLabel, 1900, 2099)
          )
        )
      )(_ => year >= 1900 && year <= 2099)
  }

  private def validateRequired(
    formComponent: FormComponent
  ): ValidatedType[Unit] = {

    case class ModelComponentIdValue(modelComponentId: ModelComponentId, value: Option[String])

    val atomsWithValues: List[ModelComponentIdValue] = formComponent.multiValueId.atomsModelComponentIds.map(m =>
      ModelComponentIdValue(m, formModelVisibilityOptics.data.one(m))
    )

    if (formComponent.mandatory) {
      atomsWithValues.foldMap { mcv =>
        mcv.value
          .filter(_.nonEmpty)
          .fold(requiredError(formComponent, mcv.modelComponentId))(_ => validationSuccess)
      }
    } else {
      validationSuccess
    }
  }

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(formComponent, "field.error.required", None)
    ).invalid

  def errorGranularity(formComponent: FormComponent)(suffix: Atom): ModelComponentId =
    formComponent.atomicFormComponentId(suffix)

}
