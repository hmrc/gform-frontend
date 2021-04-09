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
import cats.implicits.{ catsSyntaxValidatedId, _ }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CalendarDate, FormComponent }
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.{ errors, fieldDescriptor }
import uk.gov.hmrc.gform.validation.DateValidationLogic.{ hasMaximumLength, isNumeric }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import java.time.Month

class CalendarDateValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequired(formComponent)
      .andThen(_ => validateDayMonth(formComponent))
      .andThen(validateDayMonthCombo(formComponent, _))

  private def validateDayMonth(formComponent: FormComponent): ValidatedType[(Int, Int)] = {

    def errorGranularity(suffix: Atom): ModelComponentId =
      formComponent.atomicFormComponentId(suffix)

    formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
      case Some(day) :: Some(month) :: Nil =>
        val label = fieldDescriptor(formComponent, "")
        val dayValidated = hasMaximumLength(day, 2, label + " " + messages("date.day"))
          .andThen(_ => isNumeric(day, label + " " + messages("date.day"), label))
          .leftMap(error => Map(errorGranularity(CalendarDate.day) -> Set(error)))
        val monthValidated = hasMaximumLength(month, 2, label + " " + messages("date.month"))
          .andThen(_ => isNumeric(month, label + " " + messages("date.month"), label))
          .leftMap(error => Map(errorGranularity(CalendarDate.month) -> Set(error)))
        (dayValidated, monthValidated).mapN((day, month) => (day, month))
      case _ =>
        validationFailure(formComponent.firstAtomModelComponentId, formComponent, "date.isMissing", None, "")
    }
  }

  private def validateDayMonthCombo(formComponent: FormComponent, dayMonth: (Int, Int)): ValidatedType[Unit] = {
    val (day, month) = dayMonth
    if (Month.values().filter(_.getValue == month).exists(m => day >= 1 && day <= m.maxLength()))
      validationSuccess
    else
      validationFailure(formComponent.firstAtomModelComponentId, formComponent, "date.dayMonthCombo.invalid", None, "")
  }

  private def validateRequired(
    formComponent: FormComponent
  ): ValidatedType[Unit] = {

    case class ModelComponentIdValue(modelComponentId: ModelComponentId, value: Option[String])

    val atomsWithValues: List[ModelComponentIdValue] = formComponent.multiValueId.atomsModelComponentIds.map(m =>
      ModelComponentIdValue(m, formModelVisibilityOptics.data.one(m))
    )

    val validatedResult = if (formComponent.mandatory) {
      atomsWithValues.map { mcv =>
        mcv.value
          .filter(_.nonEmpty)
          .fold(requiredError(formComponent, mcv.modelComponentId))(_ => validationSuccess)
      }
    } else {
      List(validationSuccess)
    }
    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(formComponent, "field.error.required", None)
    ).invalid
}
