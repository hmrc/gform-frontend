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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, FormComponent, FormComponentId, FormCtx, OptionData }

object OptionDataUtils {

  def expand[D <: DataOrigin](formComponent: FormComponent, choice: Choice)(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): FormComponent = {
    val optionsUpdate = choice.options.flatMap {
      case o @ OptionData.ValueBased(_, _, _, Some(d @ OptionData.Dynamic(_)), _) =>
        OptionDataUtils.expandValueBased(o, d)
      case o @ OptionData.IndexBased(_, _, _, Some(d @ OptionData.Dynamic(_))) =>
        OptionDataUtils.expandIndexBased(o, d)
      case otherwise => NonEmptyList.one(otherwise)
    }

    val updChoice = choice.copy(options = optionsUpdate)

    formComponent.copy(`type` = updChoice)
  }

  def expandValueBased[D <: DataOrigin](valueBased: OptionData.ValueBased, dynamic: OptionData.Dynamic)(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): NonEmptyList[OptionData] =
    expandOptionData(valueBased, dynamic, valueBased.label, updateValueBased)

  def expandIndexBased[D <: DataOrigin](indexBased: OptionData.IndexBased, dynamic: OptionData.Dynamic)(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): NonEmptyList[OptionData] =
    expandOptionData(indexBased, dynamic, indexBased.label, updateIndexBased)

  private def updateIndexBased(
    index: Int,
    baseIds: List[FormComponentId],
    od: OptionData.IndexBased
  ): OptionData.IndexBased =
    od.copy(
      label = od.label.expand(index, baseIds),
      hint = od.hint.map(_.expand(index, baseIds)),
      dynamic = od.dynamic.map(
        ExpandUtils.expandOptionDataDynamic(index, _)
      ) // We need dynamic with index for StructuredFormData model
    )

  private def updateValueBased(
    index: Int,
    baseIds: List[FormComponentId],
    od: OptionData.ValueBased
  ): OptionData.ValueBased =
    od.copy(
      label = od.label.expand(index, baseIds),
      hint = od.hint.map(_.expand(index, baseIds)),
      dynamic = od.dynamic.map(ExpandUtils.expandOptionDataDynamic(index, _)),
      value = od.value + "_" + index
    )

  private def expandOptionData[A, D <: DataOrigin](
    valueBased: A,
    dynamic: OptionData.Dynamic,
    label: SmartString,
    f: (Int, List[FormComponentId], A) => A
  )(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): NonEmptyList[A] = {
    val modelComponentIds =
      fmvo.formModel.allIndexedComponentIds
        .filter(_.baseComponentId == dynamic.formComponentId.baseComponentId)

    NonEmptyList.fromList(modelComponentIds) match {
      case None => NonEmptyList.one(valueBased)
      case Some(modelComponentIdsNel) =>
        modelComponentIdsNel.map { case modelComponentId =>
          val baseIds =
            label.interpolations.flatMap(_.leafs(fmvo.formModel)).collect { case FormCtx(fcId) =>
              fcId
            }

          modelComponentId.maybeIndex match {
            case Some(index) => f(index, baseIds, valueBased)
            case None        => valueBased
          }
        }
    }
  }
}
