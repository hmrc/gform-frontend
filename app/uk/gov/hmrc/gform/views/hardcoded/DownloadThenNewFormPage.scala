/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.views.hardcoded

import play.api.data.Form
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.govukfrontend.views.html.components._

import java.time.format.DateTimeFormatter

class DownloadThenNewFormPage(val formTemplate: FormTemplate, form: Form[String], submission: Submission, size: Int)(
  implicit messages: Messages
) extends CommonPageProperties(formTemplate) {

  private val govukErrorMessage: GovukErrorMessage = new GovukErrorMessage()
  private val govukFieldset: GovukFieldset = new GovukFieldset()
  private val govukHint: GovukHint = new GovukHint()
  private val govukLabel: GovukLabel = new GovukLabel()

  private val dateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy", messages.lang.locale)
  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm", messages.lang.locale)
  private val submittedDateTime = submission.submittedDate
  private val fileSize = (BigDecimal(size) / BigDecimal(1000)).setScale(0, BigDecimal.RoundingMode.UP)

  val downloadUrl: String =
    uk.gov.hmrc.gform.gform.routes.AcknowledgementController.downloadPDF(Option.empty[AccessCode], formTemplate._id).url

  val errorSummary: ErrorSummary = {

    val errorsHtml: Seq[ErrorLink] = (form.errors ++ form.globalErrors).map { error =>
      ErrorLink(
        href = Some("#" + error.key),
        content = Text(messages(s"${error.key}.${error.message}", formCategory))
      )
    }

    ErrorSummary(
      errorList = errorsHtml,
      title = Text(messages("error.summary.heading"))
    )
  }

  val hasErrors: Boolean = errorSummary.errorList.nonEmpty

  val table = Table(
    rows = Seq(
      Seq(
        TableRow(
          content = Text(messages("downloadThenNew.table.caption.submissionRef"))
        ),
        TableRow(
          content = Text(submission.submissionRef.value)
        )
      ),
      Seq(
        TableRow(
          content = Text(messages("downloadThenNew.table.caption.submittedOn"))
        ),
        TableRow(
          content = Text(
            messages(
              "downloadThenNew.table.value.submittedOn",
              submittedDateTime.format(dateFormat),
              submittedDateTime.format(timeFormat)
            )
          )
        )
      ),
      Seq(
        TableRow(
          content = Text(messages("downloadThenNew.table.caption.fileType"))
        ),
        TableRow(
          content = Text("PDF")
        )
      ),
      Seq(
        TableRow(
          content = Text(messages("downloadThenNew.table.caption.fileSize"))
        ),
        TableRow(
          content = Text(s"$fileSize KB")
        )
      )
    ),
    head = None,
    caption = Some(messages("downloadThenNew.title")),
    captionClasses = "govuk-table__caption--l",
    firstCellIsHeader = false
  )

  val render: Html = {
    val errorMessage = form.errors.headOption.map { error =>
      val message = messages(s"${error.key}.${error.message}", formCategory)
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = Text(message)
      )
    }

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = Text(messages("downloadThenNew.whatNext.title")),
            isPageHeading = true,
            classes = "govuk-fieldset__legend--m"
          )
        )
      )
    )

    val startNew = RadioItem(
      value = Some("startNew"),
      content = Text(messages("downloadThenNew.whatNext.startNew"))
    )

    val signOut = RadioItem(
      value = Some("signOut"),
      content = Text(messages("downloadThenNew.whatNext.signOut"))
    )

    val radios = Radios(
      fieldset = fieldset,
      errorMessage = errorMessage,
      name = "downloadThenNew",
      items = List(startNew, signOut)
    )

    new GovukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
