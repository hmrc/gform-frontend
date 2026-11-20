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

package uk.gov.hmrc.gform.bulkupload

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import java.time.LocalDate
import play.api.i18n.Messages
import uk.gov.hmrc.gform.commons.BigDecimalUtil
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, ColumnType }

object BulkUploadValidator {
  def validate(bulkUpload: BulkUpload, data: List[List[String]])(implicit messages: Messages): List[BulkUploadError] = {
    val validationResults: List[Validated[BulkUploadError, Unit]] =
      data.zipWithIndex.flatMap { case (row, index) =>
        val validationResults: List[Validated[(ColumnIndex, String), Unit]] =
          row.zip(bulkUpload.spreadsheet).zipWithIndex.map { case ((cellValue, column), columnIdx) =>
            if (!column.mandatory && cellValue.trim.isEmpty) {
              Valid(()): Validated[(ColumnIndex, String), Unit]
            } else {
              val columnIndex = ColumnIndex(columnIdx)
              column.`type` match {
                case ColumnType.Text =>
                  if (cellValue.trim.isEmpty) {
                    Invalid(columnIndex -> messages("bulk.upload.cell.mandatory", column.title))
                  } else {
                    Valid(()): Validated[(ColumnIndex, String), Unit]
                  }
                case ColumnType.Date =>
                  Validated
                    .catchNonFatal(LocalDate.parse(cellValue, BulkUploadConstant.bulkUploadDateFormatter))
                    .leftMap(_ => columnIndex -> messages("bulk.upload.cell.not.date", column.title, cellValue))
                    .map(_ => ())
                case ColumnType.Number =>
                  BigDecimalUtil
                    .toBigDecimalSafe(cellValue)
                    .fold[Validated[(ColumnIndex, String), Unit]](
                      Invalid(columnIndex -> messages("bulk.upload.cell.not.number", column.title, cellValue))
                    )(_ => Valid(()))
              }
            }
          }

        validationResults.map { validated =>
          validated.leftMap { case (columnIndex, error) =>
            BulkUploadError(columnIndex, RowIndex(index), error, row)
          }
        }
      }

    val validationErrors: List[BulkUploadError] = validationResults.collect { case Validated.Invalid(x) =>
      x
    }

    validationErrors
  }

}
