/*
 * Copyright 2021 HM Revenue & Customs
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

import play.api.i18n.Messages
import play.twirl.api.Html
import play.api.data.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.html.components._

class ContinueFormPage(val formTemplate: FormTemplate, form: Form[String])(implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {

  private val govukErrorMessage: govukErrorMessage = new govukErrorMessage()
  private val govukFieldset: govukFieldset = new govukFieldset()
  private val govukHint: govukHint = new govukHint()
  private val govukLabel: govukLabel = new govukLabel()

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
      ErrorMessage(
        content = Text(message)
      )
    }

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = Text(messages("continueDelete.title", formCat)),
            isPageHeading = true,
            classes = "govuk-label--l"
          ))
      ))

    val continue = RadioItem(
      value = Some("continue"),
      content = Text(messages("continueDelete.continue", formCat))
    )

    val delete = RadioItem(
      value = Some("delete"),
      content = Text(messages("continueDelete.delete"))
    )

    val radios = Radios(
      fieldset = fieldset,
      errorMessage = errorMessage,
      name = "decision",
      items = List(continue, delete)
    )

    new govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
