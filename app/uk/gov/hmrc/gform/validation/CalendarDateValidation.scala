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
import cats.implicits.{ catsSyntaxValidatedId, _ }
import cats.data.Validated.Valid
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ CalendarDate, FormComponent }
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.eval.smartstring._

import java.time.Month

class CalendarDateValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequired(formComponent)
      .andThen(_ => validateDayMonth(formComponent))

  private def validateDayMonth(formComponent: FormComponent): ValidatedType[Unit] =
    formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
      case Some(day) :: Some(month) :: Nil =>
        val monthValidation = month.toIntOption
          .filter(m => m >= 1 && m <= 12)
          .fold(
            validationFailure[Int](
              errorGranularity(formComponent)(CalendarDate.month),
              formComponent,
              "generic.error.calendarDate.month.real",
              None,
              ""
            )
          )(m => Valid(m))
        val maxLenght = monthValidation.fold(_ => 31, m => Month.of(m).maxLength())
        val dayValidation = day.toIntOption
          .filter(d => d >= 1 && d <= maxLenght)
          .fold(
            validationFailure[Int](
              errorGranularity(formComponent)(CalendarDate.day),
              formComponent,
              "generic.error.calendarDate.day.real",
              None,
              ""
            )
          )(d => Valid(d))
        (monthValidation, dayValidation).mapN((_, _) => ())
      case _ =>
        validationFailure(
          formComponent.firstAtomModelComponentId,
          formComponent,
          "generic.error.calendarDate.required",
          None,
          ""
        )
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
    Map[ModelComponentId, Set[String]] {
      val placeholder1 = formComponent.errorShortName
        .flatMap(_.nonBlankValue())
        .getOrElse(SmartString.blank.transform(_ => "a date", _ => "ddyddiad").value())
      val placeholder2 = formComponent.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
      modelComponentId -> errors(
        formComponent,
        "generic.error.taxPeriodDate.required",
        Some(placeholder1 :: placeholder2 :: Nil)
      )
    }.invalid

  private def errorGranularity(formComponent: FormComponent)(suffix: Atom): ModelComponentId =
    formComponent.atomicFormComponentId(suffix)
}
