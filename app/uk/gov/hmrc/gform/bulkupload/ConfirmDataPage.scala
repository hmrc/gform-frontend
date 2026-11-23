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

import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.gform.{ HasErrors, PageLevelErrorHtml }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, FormComponent }
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink
import uk.gov.hmrc.govukfrontend.views.html.components.Table
import uk.gov.hmrc.govukfrontend.views.viewmodels.table.{ HeadCell, TableRow }

class ConfirmDataPage(
  formComponent: FormComponent,
  formFieldValidationResult: FormFieldValidationResult,
  data: List[List[String]],
  bulkUpload: BulkUpload
)(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  val subjects: String = bulkUpload.subjectForms.plural.value()

  val errorSummary: HasErrors =
    PageLevelErrorHtml.generatePageLevelErrorHtml(List(formFieldValidationResult), List.empty[ErrorLink])

  val hasErrors: Boolean = errorSummary.hasErrors

  private def bulkUploadTable(
    rows: List[Seq[TableRow]]
  ): Table = {
    val head =
      bulkUpload.spreadsheet.map(spreadsheetColumn =>
        HeadCell(
          content = Text(spreadsheetColumn.title)
        )
      )
    Table(
      rows = rows.toList,
      head = Some(head),
      firstCellIsHeader = false
    )
  }

  val rows: List[Seq[TableRow]] = data.map(row =>
    row.map(cell =>
      TableRow(
        content = Text(cell)
      )
    )
  )

  val table: Table = bulkUploadTable(rows)

  val render: Html = html.bulkupload.confirm_data_page(table)
}
