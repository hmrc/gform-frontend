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
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.ExpressionResultWithTypeInfo
import uk.gov.hmrc.gform.gform.ExprUpdater
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, Dynamic, FormComponent, FormComponentId, FormCtx, OptionData, OptionDataValue }

object OptionDataUtils {

  def expand[D <: DataOrigin](formComponent: FormComponent, choice: Choice)(implicit
    fmvo: FormModelVisibilityOptics[D],
    messages: Messages
  ): FormComponent = {
    val optionsUpdate = choice.options.flatMap {
      case o @ OptionData.ValueBased(_, _, _, Some(d), _, _) =>
        OptionDataUtils.expandValueBased(o, d)
      case o @ OptionData.IndexBased(_, _, _, Some(d), _) =>
        OptionDataUtils.expandIndexBased(o, d)
      case otherwise => NonEmptyList.one(otherwise)
    }

    val updChoice = choice.copy(options = optionsUpdate)

    formComponent.copy(`type` = updChoice)
  }

  def determineBaseIds[D <: DataOrigin](optionData: OptionData)(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): List[FormComponentId] =
    (optionData.label :: optionData.hint.toList)
      .flatMap(_.interpolations(fmvo.booleanExprResolver.resolve(_)).flatMap(_.leafs(fmvo.formModel)).collect {
        case FormCtx(fcId) =>
          fcId
      })

  def expandValueBased[D <: DataOrigin](valueBased: OptionData.ValueBased, dynamic: Dynamic)(implicit
    fmvo: FormModelVisibilityOptics[D],
    messages: Messages
  ): NonEmptyList[OptionData] =
    dynamic match {
      case Dynamic.DataRetrieveBased(dataRetrieveCtx) =>
        val expressionResult: ExpressionResultWithTypeInfo = fmvo.evalAndApplyTypeInfoFirst(dataRetrieveCtx.ctx)
        val optionDatas: List[OptionData.ValueBased] = expressionResult.listRepresentation.zipWithIndex.map {
          case (_, index) =>
            updateDataRetrieveValueBased(index, valueBased)
        }
        NonEmptyList.fromList(optionDatas).getOrElse(NonEmptyList.one(valueBased))
      case atl @ Dynamic.ATLBased(_) =>
        val baseIds: List[FormComponentId] = determineBaseIds(valueBased)
        expandOptionData(valueBased, atl, baseIds, updateValueBased)
    }

  def expandIndexBased[D <: DataOrigin](indexBased: OptionData.IndexBased, dynamic: Dynamic)(implicit
    fmvo: FormModelVisibilityOptics[D],
    messages: Messages
  ): NonEmptyList[OptionData] =
    dynamic match {
      case Dynamic.DataRetrieveBased(dataRetrieveCtx) =>
        val expressionResult: ExpressionResultWithTypeInfo = fmvo.evalAndApplyTypeInfoFirst(dataRetrieveCtx.ctx)
        val optionDatas = expressionResult.listRepresentation.zipWithIndex.map { case (answer, index) =>
          updateDataRetrieveIndexBased(index, indexBased)
        }
        NonEmptyList.fromList(optionDatas).getOrElse(NonEmptyList.one(indexBased))

      case atl @ Dynamic.ATLBased(_) =>
        val baseIds: List[FormComponentId] = determineBaseIds(indexBased)
        expandOptionData(indexBased, atl, baseIds, updateIndexBased)
    }

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
      ), // We need dynamic with index for StructuredFormData model
      summaryValue = od.summaryValue.map(_.expand(index, baseIds))
    )

  private def updateDataRetrieveIndexBased(
    index: Int,
    od: OptionData.IndexBased
  ): OptionData.IndexBased =
    od.copy(
      label = od.label.expandDataRetrieve(index),
      hint = od.hint.map(_.expandDataRetrieve(index)),
      dynamic = od.dynamic.map(
        ExpandUtils.expandOptionDataDynamic(index, _)
      ), // We need dynamic with index for StructuredFormData model
      summaryValue = od.summaryValue.map(_.expandDataRetrieve(index))
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
      value = od.value match {
        case OptionDataValue.StringBased(value) => OptionDataValue.StringBased(value + "_" + index)
        case OptionDataValue.ExprBased(expr) =>
          OptionDataValue.ExprBased(new ExprUpdater(index, baseIds).expandExpr(expr))
      },
      summaryValue = od.summaryValue.map(_.expand(index, baseIds))
    )

  private def updateDataRetrieveValueBased(
    index: Int,
    od: OptionData.ValueBased
  ): OptionData.ValueBased =
    od.copy(
      label = od.label.expandDataRetrieve(index),
      hint = od.hint.map(_.expandDataRetrieve(index)),
      dynamic = od.dynamic.map(ExpandUtils.expandOptionDataDynamic(index, _)),
      value = od.value match {
        case OptionDataValue.StringBased(value) => OptionDataValue.StringBased(value + "_" + index)
        case OptionDataValue.ExprBased(expr)    => OptionDataValue.ExprBased(expr)
      },
      summaryValue = od.summaryValue.map(_.expandDataRetrieve(index))
    )

  private def expandOptionData[A, D <: DataOrigin](
    valueBased: A,
    dynamic: Dynamic.ATLBased,
    baseIds: List[FormComponentId],
    f: (Int, List[FormComponentId], A) => A
  )(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): NonEmptyList[A] = {
    val modelComponentIds =
      fmvo.formModel.allIndexedComponentIds.get(dynamic.formComponentId.baseComponentId).toList.flatten

    NonEmptyList.fromList(modelComponentIds) match {
      case None => NonEmptyList.one(valueBased)
      case Some(modelComponentIdsNel) =>
        modelComponentIdsNel.map { modelComponentId =>
          modelComponentId.maybeIndex match {
            case Some(index) => f(index, baseIds, valueBased)
            case None        => valueBased
          }
        }
    }
  }
}
