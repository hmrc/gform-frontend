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

import cats.data.NonEmptyList
import cats.syntax.eq._
import play.api.data.Form
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.bulkupload.BulkUploadConstant._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BulkUpload, ColumnType, Date, FormComponent, SpreadsheetColumn }
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.html.components.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.html.helpers.{ GovukFormGroup, GovukHintAndErrorMessage }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.dateinput.{ DateInput, InputItem }
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.govukfrontend.views.viewmodels.fieldset.{ Fieldset, Legend }
import uk.gov.hmrc.govukfrontend.views.viewmodels.input.Input
import uk.gov.hmrc.govukfrontend.views.viewmodels.label.Label

class EditPage(
  formComponent: FormComponent,
  bulkUpload: BulkUpload,
  value: String,
  spreadsheatColumn: SpreadsheetColumn,
  editTextForm: Form[String],
  editBigDecimalForm: Form[BigDecimal],
  editDateForm: Form[BulkUploadDate]
)(implicit
  messages: Messages
) {

  private val govukErrorMessage: components.GovukErrorMessage = new components.GovukErrorMessage()
  private val govukFieldset: components.GovukFieldset = new components.GovukFieldset()
  private val govukHint: components.GovukHint = new components.GovukHint()
  private val govukLabel: components.GovukLabel = new components.GovukLabel()
  private val govukFormGroup: GovukFormGroup = new GovukFormGroup
  private val govukHintAndErrorMessage: GovukHintAndErrorMessage =
    new GovukHintAndErrorMessage(govukHint, govukErrorMessage)
  private val govukInput = new components.GovukInput(govukLabel, govukFormGroup, govukHintAndErrorMessage)

  private def sizeForAtom(atom: Atom): String = atom match {
    case Date.year => "govuk-input--width-4"
    case _         => "govuk-input--width-2"
  }

  val title: String = messages("bulk.upload.cell.enter", spreadsheatColumn.title.toLowerCase)

  val errorSummary: ErrorSummary = {

    val errorsHtml: Option[ErrorLink] = spreadsheatColumn.`type` match {
      case ColumnType.Text =>
        editTextForm.errors.headOption.map { error =>
          ErrorLink(
            href = Some("#" + error.key),
            content = Text(messages(s"${error.key}.${error.message}", spreadsheatColumn.title.toLowerCase))
          )
        }
      case ColumnType.Date =>
        editDateForm.errors.headOption.map { error =>
          ErrorLink(
            href = Some("#" + formComponent.id.value + "-" + error.key),
            content = Text(messages(s"${error.key}.${error.message}"))
          )
        }
      case ColumnType.Number =>
        editBigDecimalForm.errors.headOption.map { error =>
          ErrorLink(
            href = Some("#" + error.key),
            content = Text(messages(s"${error.key}.${error.message}"))
          )
        }
    }

    ErrorSummary(
      errorList = errorsHtml.toSeq,
      title = Text(messages("error.summary.heading"))
    )
  }

  val hasErrors: Boolean = errorSummary.errorList.nonEmpty

  val field = spreadsheatColumn.`type` match {
    case ColumnType.Text =>
      val label = Label(
        content = Text(spreadsheatColumn.title)
      )

      val hasError = editTextForm.errors.exists(_.key === BULK_UPLOAD_TEXT)

      val errorClasses = if (hasError) "govuk-input--error" else ""

      val errorMessage = editTextForm.errors.headOption.map { error =>
        val message = messages(s"${error.key}.${error.message}", spreadsheatColumn.title.toLowerCase)
        ErrorMessage.errorMessageWithDefaultStringsTranslated(
          content = Text(message)
        )
      }

      val textInput = Input(
        id = BULK_UPLOAD_TEXT,
        name = BULK_UPLOAD_TEXT,
        label = label,
        classes = errorClasses,
        errorMessage = errorMessage,
        value = Some(value)
      )

      govukInput(textInput)
    case ColumnType.Date =>
      val fieldset = Fieldset(
        legend = Some(
          Legend(
            content = Text(spreadsheatColumn.title)
          )
        )
      )

      val atomValues = value.split("/").toList match {
        case day :: month :: year :: Nil => NonEmptyList.of(day, month, year)
        case _                           => NonEmptyList.of(value, "", "")
      }

      val items =
        Date
          .fields(formComponent.id.modelComponentId.indexedComponentId)
          .zip(atomValues)
          .map { case (modelComponentId, prepop) =>
            val atom = modelComponentId.atom

            val fieldName = "bulk.upload." + atom.value

            val hasError = editDateForm.errors.exists(_.key === fieldName)

            val errorClasses = if (hasError) "govuk-input--error" else ""

            InputItem(
              name = fieldName,
              label = Some(messages("date." + atom.value.capitalize)),
              value = Some(prepop),
              classes = s"$errorClasses ${sizeForAtom(atom)}",
              inputmode = Some("numeric")
            )
          }

      val errorMessage = editDateForm.errors.headOption.map { error =>
        val message = messages(s"${error.key}.${error.message}")
        ErrorMessage.errorMessageWithDefaultStringsTranslated(
          content = Text(message)
        )
      }

      val dateInput = DateInput(
        id = formComponent.id.value,
        items = items.toList,
        errorMessage = errorMessage,
        fieldset = Some(fieldset)
      )

      new components.GovukDateInput(govukFieldset, govukInput, govukFormGroup, govukHintAndErrorMessage)(dateInput)
    case ColumnType.Number =>
      val label = Label(
        content = Text(spreadsheatColumn.title)
      )

      val hasError = editBigDecimalForm.errors.exists(_.key === BULK_UPLOAD_NUMBER)

      val errorClasses = if (hasError) "govuk-input--error" else ""

      val errorMessage = editBigDecimalForm.errors.headOption.map { error =>
        val message = messages(s"${error.key}.${error.message}", spreadsheatColumn.title.toLowerCase)
        ErrorMessage.errorMessageWithDefaultStringsTranslated(
          content = Text(message)
        )
      }

      val textInput = Input(
        id = BULK_UPLOAD_NUMBER,
        name = BULK_UPLOAD_NUMBER,
        label = label,
        classes = errorClasses,
        errorMessage = errorMessage,
        value = Some(value)
      )

      govukInput(textInput)
  }

  val render: Html = field
}
