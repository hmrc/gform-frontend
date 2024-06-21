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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Dynamic, FormComponent, FormComponentId, FormCtx, TableComp, TableValue, TableValueRow }

object TableUtils {

  def expand[D <: DataOrigin](formComponent: FormComponent, table: TableComp)(implicit
    fmvo: FormModelVisibilityOptics[D]
  ): FormComponent = {
    val rows: List[TableValueRow] = table.rows.flatMap { row =>
      row.dynamic.fold(List(row)) {
        case d @ Dynamic.ATLBased(formComponentId) =>
          val baseIds = determineBaseIds(row)
          expandTable[D](row, d, baseIds).toList
        case Dynamic.DataRetrieveBased(_) => List(row)
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
      fmvo.formModel.allIndexedComponentIds
        .filter(_.baseComponentId == dynamic.formComponentId.baseComponentId)

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
}
