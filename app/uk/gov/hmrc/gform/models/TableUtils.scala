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
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Dynamic, FormComponent, FormComponentId, FormCtx, IndexOfDataRetrieveCtx, TableComp, TableValue, TableValueRow }

object TableUtils {

  def expand[D <: DataOrigin](formComponent: FormComponent, table: TableComp)(implicit
    fmvo: FormModelVisibilityOptics[D],
    messages: Messages
  ): FormComponent = {
    val rows: List[TableValueRow] = table.rows.flatMap { row =>
      row.dynamic.fold(List(row)) {
        case d @ Dynamic.ATLBased(_) =>
          val allAddToListBrackets: List[Bracket.AddToList[Visibility]] = fmvo.formModel.brackets.addToListBrackets

          val addToListComponentBaseIds: Set[BaseComponentId] =
            allAddToListBrackets
              .flatMap(
                _.iterations.toList.flatMap(_.toPageModel.toList.flatMap(_.allFormComponentIds.map(_.baseComponentId)))
              )
              .toSet
          // Expand only addToList's components
          val baseIds = determineBaseIds(row).filter(baseId => addToListComponentBaseIds(baseId.baseComponentId))
          expandTable[D](row, d, baseIds).toList
        case Dynamic.DataRetrieveBased(indexOfDataRetrieveCtx) =>
          expandDataRetrieveTable(row, indexOfDataRetrieveCtx)
      }
    }
    val updTable = table.copy(rows = rows)
    formComponent.copy(`type` = updTable)
  }

  def determineBaseIds[D <: DataOrigin](tableValueRow: TableValueRow)(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): List[FormComponentId] =
    tableValueRow.values.flatMap { tableValue =>
      tableValue.value.interpolations(fmvo.booleanExprResolver.resolve(_)).flatMap(_.leafs(fmvo.formModel)).collect {
        case FormCtx(fcId) =>
          fcId
      }
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

  private def expandTable[D <: DataOrigin](
    tableValueRow: TableValueRow,
    dynamic: Dynamic.ATLBased,
    baseIds: List[FormComponentId]
  )(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): NonEmptyList[TableValueRow] = {
    val modelComponentIds =
      fmvo.formModel.allIndexedComponentIds.get(dynamic.formComponentId.baseComponentId).toList.flatten

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

  private def expandDataRetrieveTable[D <: DataOrigin](
    tableValueRow: TableValueRow,
    indexOfDataRetrieveCtx: IndexOfDataRetrieveCtx
  )(implicit
    fmvo: FormModelVisibilityOptics[D],
    messages: Messages
  ): List[TableValueRow] = {
    val expressionResult = fmvo.evalAndApplyTypeInfoFirst(indexOfDataRetrieveCtx.ctx)
    val results = expressionResult.listRepresentation
    if (results.isEmpty) {
      List(tableValueRow)
    } else {
      results.zipWithIndex.map { case (_, index) => updateDataRetrieveTableValueRow(index, tableValueRow) }
    }
  }
}
