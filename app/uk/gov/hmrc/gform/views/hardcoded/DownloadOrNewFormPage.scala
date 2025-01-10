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

import cats.implicits.catsSyntaxEq
import play.api.data.Form
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.form.SubmittedDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, SuppressErrors }
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

class DownloadOrNewFormPage(
  val formTemplate: FormTemplate,
  form: Form[String],
  submittedDate: Option[SubmittedDate],
  se: SuppressErrors
)(implicit
  messages: Messages
) extends CommonPageProperties(formTemplate) {

  private val govukErrorMessage: GovukErrorMessage = new GovukErrorMessage()
  private val govukFieldset: GovukFieldset = new GovukFieldset()
  private val govukHint: GovukHint = new GovukHint()
  private val govukLabel: GovukLabel = new GovukLabel()

  private val dateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy", messages.lang.locale)
  private val timeFormat = DateTimeFormatter.ofPattern("HH:mm", messages.lang.locale)
  private val submittedDateTime = submittedDate.getOrElse(SubmittedDate(LocalDateTime.MIN)).submittedAt

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

  val hasErrors: Boolean =
    if (se === SuppressErrors.No)
      errorSummary.errorList.nonEmpty
    else false

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
            content = Text(messages("downloadOrNew.title")),
            isPageHeading = true,
            classes = "govuk-fieldset__legend--l"
          )
        )
      )
    )

    val download = RadioItem(
      value = Some("download"),
      content = Text(messages("downloadOrNew.download.text")),
      hint = Some(
        Hint(content =
          Text(
            messages(
              "downloadOrNew.download.helpText",
              submittedDateTime.format(dateFormat),
              submittedDateTime.format(timeFormat)
            )
          )
        )
      )
    )

    val startNew = RadioItem(
      value = Some("startNew"),
      content = Text(messages("downloadOrNew.startNew.text")),
      hint = Some(Hint(content = Text(messages("downloadOrNew.startNew.helpText"))))
    )

    val radios = Radios(
      fieldset = fieldset,
      hint = Some(Hint(content = Text(messages("downloadOrNew.helpText")))),
      errorMessage = if (hasErrors) errorMessage else None,
      name = "downloadOrNew",
      items = List(download, startNew)
    )

    new GovukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
