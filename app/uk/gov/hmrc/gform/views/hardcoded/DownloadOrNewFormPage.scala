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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.html.components.{ ErrorLink, ErrorMessage, ErrorSummary, Fieldset, GovukErrorMessage, GovukFieldset, GovukHint, GovukLabel, GovukRadios, Legend, RadioItem, Radios, Text }
import uk.gov.hmrc.govukfrontend.views.viewmodels.hint.Hint

class DownloadOrNewFormPage(val formTemplate: FormTemplate, form: Form[String])(implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {

  private val govukErrorMessage: GovukErrorMessage = new GovukErrorMessage()
  private val govukFieldset: GovukFieldset = new GovukFieldset()
  private val govukHint: GovukHint = new GovukHint()
  private val govukLabel: GovukLabel = new GovukLabel()

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
      hint = Some(Hint(content = Text(messages("downloadOrNew.download.helpText"))))
    )

    val startNew = RadioItem(
      value = Some("startNew"),
      content = Text(messages("downloadOrNew.startNew.text")),
      hint = Some(Hint(content = Text(messages("downloadOrNew.startNew.helpText"))))
    )

    val radios = Radios(
      fieldset = fieldset,
      hint = Some(Hint(content = Text(messages("downloadOrNew.helpText")))),
      errorMessage = errorMessage,
      name = "downloadOrNew",
      items = List(download, startNew)
    )

    new GovukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
