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

import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.ExpressionResultWithTypeInfo
import uk.gov.hmrc.gform.gform.ExprUpdater
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.recalculation.{ Calculator, EvaluationStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, Dynamic, FormComponent, FormComponentId, FormCtx, OptionData, OptionDataValue }

object OptionDataUtils {

  def expand(formComponent: FormComponent, choice: Choice, calculator: Calculator)(implicit
    messages: Messages
  ): FormComponent = {
    val optionsUpdate = choice.options.toList.flatMap {
      case o @ OptionData.ValueBased(_, _, _, Some(d), _, _, _) =>
        OptionDataUtils.expandValueBased(o, d, calculator)
      case o @ OptionData.IndexBased(_, _, _, Some(d), _) =>
        OptionDataUtils.expandIndexBased(o, d, calculator)
      case otherwise => List(otherwise)
    }

    formComponent.copy(`type` = choice.copy(options = optionsUpdate))

  }

  def determineBaseIds(optionData: OptionData, calculator: Calculator): List[FormComponentId] =
    (optionData.label :: optionData.hint.toList)
      .flatMap(
        _.interpolations(calculator.evalBooleanExpr(_))
          .collect { case FormCtx(fcId) =>
            fcId
          }
      )

  def expandValueBased(valueBased: OptionData.ValueBased, dynamic: Dynamic, calculator: Calculator)(implicit
    messages: Messages
  ): List[OptionData] =
    dynamic match {
      case Dynamic.DataRetrieveBased(dataRetrieveCtx) =>
        val expressionResult: ExpressionResultWithTypeInfo = calculator.evalExpr(dataRetrieveCtx.ctx)
        expressionResult.listRepresentation.zipWithIndex.map { case (_, index) =>
          updateDataRetrieveValueBased(index, valueBased)
        }
      case atl @ Dynamic.ATLBased(_) =>
        val baseIds: List[FormComponentId] = determineBaseIds(valueBased, calculator)
        expandOptionData(valueBased, atl, baseIds, calculator, updateValueBased)
    }

  def expandIndexBased(indexBased: OptionData.IndexBased, dynamic: Dynamic, calculator: Calculator)(implicit
    messages: Messages
  ): List[OptionData] =
    dynamic match {
      case Dynamic.DataRetrieveBased(dataRetrieveCtx) =>
        val expressionResult: ExpressionResultWithTypeInfo = calculator.evalExpr(dataRetrieveCtx.ctx)
        expressionResult.listRepresentation.zipWithIndex.map { case (answer, index) =>
          updateDataRetrieveIndexBased(index, indexBased)
        }
      case atl @ Dynamic.ATLBased(_) =>
        val baseIds: List[FormComponentId] = determineBaseIds(indexBased, calculator)
        expandOptionData(indexBased, atl, baseIds, calculator, updateIndexBased)
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

  private def expandOptionData[A](
    valueBased: A,
    dynamic: Dynamic.ATLBased,
    baseIds: List[FormComponentId],
    calculator: Calculator,
    f: (Int, List[FormComponentId], A) => A
  ): List[A] = {
    val modelComponentIds: List[ModelComponentId] =
      calculator.allModelComponentIds(dynamic.formComponentId.modelComponentId).collect {
        case (modelComponentId, evaluationStatus) if evaluationStatus != EvaluationStatus.Hidden => modelComponentId
      }

    if (modelComponentIds.isEmpty) {
      List.empty[A]
    } else {
      val indices: List[Int] = modelComponentIds.flatMap(_.maybeIndex).distinct
      if (indices.isEmpty) List(valueBased)
      else {
        indices.map(index => f(index, baseIds, valueBased))
      }
    }
  }
}
