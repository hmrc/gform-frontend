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
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.{ errors }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import java.time.Month
import scala.util.Try

class CalendarDateValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequiredField(formComponent)
      .andThen(_ => validateDayMonthAreNumbers(formComponent))
      .andThen(validateDayMonthCombo(formComponent, _))

  private def validateDayMonthAreNumbers(formComponent: FormComponent): ValidatedType[(Int, Int)] =
    formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
      case Some(day) :: Some(month) :: Nil =>
        (validInt(formComponent, day, messages("date.day")), validInt(formComponent, month, messages("date.month")))
          .mapN((day, month) => (day, month))
      case _ =>
        validationFailure(formComponent.firstAtomModelComponentId, formComponent, "date.isMissing", None, "")
    }

  private def validInt(formComponent: FormComponent, value: String, timeUnitLabel: String): ValidatedType[Int] =
    Try(value.toInt).fold(_ => validationFailure(formComponent, "field.error.number", None, timeUnitLabel), _.valid)

  private def validateDayMonthCombo(formComponent: FormComponent, dayMonth: (Int, Int)): ValidatedType[Unit] = {
    val (day, month) = dayMonth
    if (Month.values().filter(_.getValue == month).exists(m => day >= 1 && day <= m.maxLength()))
      validationSuccess
    else
      validationFailure(formComponent, "date.dayMonthCombo.invalid", None)
  }

  private def validateRequiredField(
    formComponent: FormComponent
  ): ValidatedType[Unit] = {

    val validatedResult =
      if (formComponent.mandatory) {
        formComponent.multiValueId.atomsModelComponentIds.map { modelComponentId =>
          val answer = formModelVisibilityOptics.data.one(modelComponentId)
          answer.filterNot(_.isEmpty()).fold(requiredError(formComponent, modelComponentId))(_ => validationSuccess)
        }
      } else List(validationSuccess)

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(formComponent, "field.error.required", None)
    ).invalid
}
