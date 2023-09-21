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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.TaxPeriodDate
import uk.gov.hmrc.gform.validation.CheckerServiceHelper._
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors

import scala.collection.mutable.LinkedHashSet

import ComponentChecker._

class TaxPeriodDateChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val checker = new TaxPeriodDateCheckerHelper(context.formModelVisibilityOptics)
    checker.validate(context.formComponent)
  }

}

class TaxPeriodDateCheckerHelper[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  implicit val stringValueForReport = new ValueForReport[String] {
    def valueForReport(): String = ""
  }

  implicit val stringPairValueForReport = new ValueForReport[(String, String)] {
    def valueForReport(): (String, String) = ("1", "1970")
  }

  def validate(formComponent: FormComponent): CheckProgram[Unit] =
    validateRequired(formComponent).andThen(_ => validateMonthYear(formComponent))

  def validateMonthYear(formComponent: FormComponent): CheckProgram[Unit] = {
    val (maybeMonth, maybeYear) =
      formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
        case Some(month) :: Some(year) :: Nil => (Some(month), Some(year))
        case _                                => (None, None)
      }

    (maybeMonth, maybeYear)
      .mapN((month, year) => (month, year))
      .toProgram(
        errorProgram = validationFailureTyped[(String, String)](
          formComponent.firstAtomModelComponentId,
          formComponent,
          "generic.error.taxPeriodDate.required",
          None,
          ""
        )
      )
      .andThen { case (monthStr, yearStr) =>
        val monthProgram = ifProgram(
          cond = monthStr.toIntOption.map(m => m >= 1 && m <= 12).getOrElse(false),
          thenProgram = successProgram(()),
          elseProgram = errorProgram(
            Map(
              errorGranularity(formComponent)(TaxPeriodDate.month) -> LinkedHashSet(
                messages("generic.error.taxPeriodDate.month.real")
              )
            )
          )
        )
        val yearProgram = ifProgram(
          cond = yearStr.toIntOption.map(y => y >= 1900 && y <= 2099).getOrElse(false),
          thenProgram = successProgram(()),
          elseProgram = errorProgram(
            Map(
              errorGranularity(formComponent)(TaxPeriodDate.year) -> LinkedHashSet(
                messages("generic.error.taxPeriodDate.year.real")
              )
            )
          )
        )
        List(monthProgram, yearProgram).nonShortCircuitProgram
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

  def errorGranularity(formComponent: FormComponent)(suffix: Atom): ModelComponentId =
    formComponent.atomicFormComponentId(suffix)

}
