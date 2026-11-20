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
import uk.gov.hmrc.gform.models.FastForward
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormTemplateId, SectionNumber }
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.gform.sharedmodel.formtemplate.BulkUpload

class BulkUploadErrorsPage(
  errors: List[BulkUploadError],
  totalNumberOfRows: Int,
  bulkUpload: BulkUpload,
  formTemplateId: FormTemplateId,
  maybeAccessCode: Option[AccessCode],
  formComponentId: FormComponentId,
  sectionNumber: SectionNumber,
  fastForward: List[FastForward]
)(implicit messages: Messages, sse: SmartStringEvaluator) {

  val subjects = bulkUpload.subjectForms.plural.value()

  val affectedRowsCount = errors.map(_.rowIndex).distinct.size

  val bulkPageErrors: List[BulkPageError] = {
    val primaryIndex = bulkUpload.spreadsheet.indexWhere(_.isPrimary())
    val secondaryIndex = bulkUpload.spreadsheet.indexWhere(_.isSecondary())

    errors.map { bulkUploadError =>
      val primaryData = bulkUploadError.data(primaryIndex)
      val secondaryData = bulkUploadError.data(secondaryIndex)

      BulkPageError.create(
        bulkUpload,
        primaryData,
        secondaryData,
        bulkUploadError,
        formTemplateId,
        maybeAccessCode,
        formComponentId,
        sectionNumber,
        fastForward
      )
    }
  }

  val render: Html =
    html.bulkupload.confirm_data_errors_page(
      bulkUpload.subjectForms,
      totalNumberOfRows,
      affectedRowsCount,
      bulkPageErrors
    )
}

case class BulkPageError(
  primaryData: String,
  secondaryData: String,
  errorMessage: String,
  changeLink: Html,
  removeLink: Html
)

object BulkPageError {
  def create(
    bulkUpload: BulkUpload,
    primaryData: String,
    secondaryData: String,
    bulkUploadError: BulkUploadError,
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    sectionNumber: SectionNumber,
    fastForward: List[FastForward]
  )(implicit messages: Messages, sse: SmartStringEvaluator): BulkPageError = {
    val prim = if (primaryData.trim.isEmpty) {
      messages("bulk.upload.primary.empty", bulkUpload.subjectForms.singular.value())
    } else {
      primaryData
    }

    val changeLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      messages("bulk.upload.row.change"),
      uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController
        .changeRowPage(
          formTemplateId,
          maybeAccessCode,
          formComponentId,
          sectionNumber,
          bulkUploadError.columnIndex,
          bulkUploadError.rowIndex,
          fastForward
        )
    )

    val removeLink = uk.gov.hmrc.gform.views.html.hardcoded.pages.link(
      messages("bulk.upload.row.remove"),
      uk.gov.hmrc.gform.bulkupload.routes.BulkUploadController
        .askConfirmationToRemoveRow(
          formTemplateId,
          maybeAccessCode,
          formComponentId,
          sectionNumber,
          bulkUploadError.rowIndex,
          fastForward
        )
    )

    BulkPageError(
      prim,
      secondaryData,
      bulkUploadError.error,
      changeLink,
      removeLink
    )
  }
}
