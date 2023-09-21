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

import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.CalendarDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.validation.CheckerServiceHelper._
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import scala.collection.mutable.LinkedHashSet

import java.time.Month

import ComponentChecker._

class CalendarDateChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val checker = new CalendarDateCheckerHelper(context.formModelVisibilityOptics)
    checker.validate(context.formComponent)
  }

}

class CalendarDateCheckerHelper[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  implicit val stringValueForReport = new ValueForReport[String] {
    def valueForReport(): String = ""
  }
  implicit val intValueForReport = new ValueForReport[Int] {
    def valueForReport(): Int = 1
  }
  implicit val stringPairValueForReport = new ValueForReport[(String, String)] {
    def valueForReport(): (String, String) = ("1", "1")
  }

  def validate(formComponent: FormComponent): CheckProgram[Unit] =
    List(validateRequired(formComponent), validateDayMonth(formComponent)).shortCircuitProgram

  private def validateDayMonth(formComponent: FormComponent): CheckProgram[Unit] = {
    val (maybeDay, maybeMonth) =
      formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
        case (Some(day) :: Some(month) :: Nil) => (Some(day), Some(month))
        case _                                 => (None, None)
      }

    (maybeDay, maybeMonth)
      .mapN((day, month) => (day, month))
      .toProgram(errorProgram =
        validationFailureTyped[(String, String)](
          formComponent.firstAtomModelComponentId,
          formComponent,
          "generic.error.calendarDate.required",
          None,
          ""
        )
      )
      .andThen { case (dayStr, monthStr) =>
        val monthProgram = monthStr.toIntOption
          .filter(m => m >= 1 && m <= 12)
          .toProgram(
            errorProgram = validationFailureTyped[Int](
              errorGranularity(formComponent)(CalendarDate.month),
              formComponent,
              "generic.error.calendarDate.month.real",
              None,
              ""
            )
          )
        def dayProgram(maxLength: Int) = dayStr.toIntOption
          .filter(d => d >= 1 && d <= maxLength)
          .toProgram(
            errorProgram = validationFailureTyped[Int](
              errorGranularity(formComponent)(CalendarDate.day),
              formComponent,
              "generic.error.calendarDate.day.real",
              None,
              ""
            )
          )
        List(
          monthProgram,
          monthProgram orElse dayProgram(31),
          monthProgram.andThen(m => dayProgram(Month.of(m).maxLength()))
        ).nonShortCircuitProgram.voidProgram

      }
  }

  private def validateRequired(
    formComponent: FormComponent
  ): CheckProgram[Unit] = {

    case class ModelComponentIdValue(modelComponentId: ModelComponentId, value: Option[String])

    val atomsWithValues: List[ModelComponentIdValue] = formComponent.multiValueId.atomsModelComponentIds.map(m =>
      ModelComponentIdValue(m, formModelVisibilityOptics.data.one(m))
    )
    ifProgram(
      andCond = formComponent.mandatory,
      thenProgram = atomsWithValues.map { mcv =>
        mcv.value
          .filter(_.nonEmpty)
          .toProgram(errorProgram = requiredError[String](formComponent, mcv.modelComponentId))
          .voidProgram
      }.nonShortCircuitProgram,
      elseProgram = successProgram(())
    )
  }

  private def requiredError[A](formComponent: FormComponent, modelComponentId: ModelComponentId): CheckProgram[A] =
    errorProgram[A](
      Map[ModelComponentId, LinkedHashSet[String]] {
        val placeholder1 = formComponent.errorShortName
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.transform(_ => "a date", _ => "ddyddiad").value())
        val placeholder2 = formComponent.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
        modelComponentId -> errors(
          formComponent,
          "generic.error.taxPeriodDate.required",
          Some(placeholder1 :: placeholder2 :: Nil)
        )
      }
    )

  private def errorGranularity(formComponent: FormComponent)(suffix: Atom): ModelComponentId =
    formComponent.atomicFormComponentId(suffix)
}
