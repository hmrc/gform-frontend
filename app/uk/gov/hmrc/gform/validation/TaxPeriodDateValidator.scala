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
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.eval.smartstring._

class TaxPeriodDateValidator[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validate(formComponent: FormComponent): ValidatedType[Unit] =
    validateRequired(formComponent).andThen(_ => validateMonthYear(formComponent))

  def validateMonthYear(formComponent: FormComponent): ValidatedType[Unit] =
    formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one) match {
      case Some(month) :: Some(year) :: Nil =>
        validationSuccess
          .ensure(
            Map(
              errorGranularity(formComponent)(TaxPeriodDate.month) -> Set(
                messages("generic.error.taxPeriodDate.month.real")
              )
            )
          )(_ => month.toIntOption.map(m => m >= 1 && m <= 12).getOrElse(false))
          .ensure(
            Map(
              errorGranularity(formComponent)(TaxPeriodDate.year) -> Set(
                messages("generic.error.taxPeriodDate.year.real")
              )
            )
          )(_ => year.toIntOption.map(y => y >= 1900 && y <= 2099).getOrElse(false))
      case _ =>
        validationFailure(
          formComponent.firstAtomModelComponentId,
          formComponent,
          "generic.error.taxPeriodDate.required",
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

  def errorGranularity(formComponent: FormComponent)(suffix: Atom): ModelComponentId =
    formComponent.atomicFormComponentId(suffix)

}
