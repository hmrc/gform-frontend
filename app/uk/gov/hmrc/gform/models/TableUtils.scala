/*
 * Copyright 2026 HM Revenue & Customs
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
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.recalculation.{ EvaluationStatus, FreeCalculator }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Dynamic, FormComponent, FormComponentId, IndexOfDataRetrieveCtx, TableComp, TableValue, TableValueRow }

object TableUtils {

  def expand(formComponent: FormComponent, table: TableComp, freeCalculator: FreeCalculator): FormComponent = {
    val rows: List[TableValueRow] = table.rows.flatMap { row =>
      row.dynamic.fold(List(row)) {
        case d @ Dynamic.ATLBased(_) =>
          val addToListComponentBaseIds: Set[BaseComponentId] = freeCalculator.metadata.addToListComponentIds
          val baseIds =
            determineBaseIds(row, freeCalculator).filter(baseId => addToListComponentBaseIds(baseId.baseComponentId))
          expandTable(row, d, baseIds, freeCalculator).toList
        case Dynamic.DataRetrieveBased(indexOfDataRetrieveCtx) =>
          expandDataRetrieveTable(row, indexOfDataRetrieveCtx, freeCalculator)
      }
    }
    val updTable = table.copy(rows = rows)
    formComponent.copy(`type` = updTable)
  }

  def determineBaseIds(tableValueRow: TableValueRow, freeCalculator: FreeCalculator): List[FormComponentId] =
    tableValueRow.values.flatMap { tableValue =>
      tableValue.value.interpolations(freeCalculator.evalBooleanExpr(_)).flatMap(_.allFormComponentIds())
    }

  private def expandTableValue(index: Int, baseIds: List[FormComponentId], tableValue: TableValue): TableValue =
    tableValue.copy(
      value = tableValue.value.expand(index, baseIds)
    )

  private def updateTableValueRow(
    index: Int,
    baseIds: List[FormComponentId],
    tableValueRow: TableValueRow
  ): TableValueRow =
    tableValueRow.copy(
      values = tableValueRow.values.map(expandTableValue(index, baseIds, _)),
      dynamic = tableValueRow.dynamic.map(ExpandUtils.expandOptionDataDynamic(index, _))
    )

  private def expandTable(
    tableValueRow: TableValueRow,
    dynamic: Dynamic.ATLBased,
    baseIds: List[FormComponentId],
    freeCalculator: FreeCalculator
  ): NonEmptyList[TableValueRow] = {

    val modelComponentIds =
      freeCalculator.answerMapWithFallback.allModelComponentIds(dynamic.formComponentId.modelComponentId).collect {
        case (modelComponentId, evaluationStatus) if evaluationStatus != EvaluationStatus.Hidden => modelComponentId
      }

    NonEmptyList.fromList(modelComponentIds) match {
      case None => NonEmptyList.one(tableValueRow)
      case Some(modelComponentIdsNel) =>
        modelComponentIdsNel.map { modelComponentId =>
          modelComponentId.maybeIndex match {
            case Some(index) =>
              updateTableValueRow(index, baseIds, tableValueRow)
            case None => tableValueRow
          }
        }
    }
  }

  private def expandDataRetrieveTableValue(index: Int, tableValue: TableValue): TableValue =
    tableValue.copy(
      value = tableValue.value.expandDataRetrieve(index)
    )

  private def updateDataRetrieveTableValueRow(
    index: Int,
    tableValueRow: TableValueRow
  ): TableValueRow =
    tableValueRow.copy(
      values = tableValueRow.values.map(expandDataRetrieveTableValue(index, _)),
      dynamic = tableValueRow.dynamic.map(ExpandUtils.expandOptionDataDynamic(index, _))
    )

  private def expandDataRetrieveTable(
    tableValueRow: TableValueRow,
    indexOfDataRetrieveCtx: IndexOfDataRetrieveCtx,
    freeCalculator: FreeCalculator
  ): List[TableValueRow] = {
    val evaluationStatus = freeCalculator.evalExpr(indexOfDataRetrieveCtx.ctx).evaluationStatus
    evaluationStatus match {
      case EvaluationStatus.ListResult(statuses) =>
        statuses.zipWithIndex.map { case (_, index) => updateDataRetrieveTableValueRow(index, tableValueRow) }
      case _ => List(tableValueRow)
    }
  }
}
