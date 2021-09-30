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

import cats.implicits._
import play.api.data.Form
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, FormTemplate, SuppressErrors }
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.views.html.hardcoded.pages.p

class CompositeAuthFormPage(
  val formTemplate: FormTemplate,
  form: Form[String],
  ggId: Option[String],
  se: SuppressErrors
)(implicit messages: Messages)
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

  val hasErrors: Boolean =
    if (se === SuppressErrors.No)
      errorSummary.errorList.nonEmpty
    else false

  val render: Html = {

    val errorMessage = form.errors.headOption.map { error =>
      val message = messages(s"${error.key}.${error.message}", formCategory)
      ErrorMessage(
        content = Text(message)
      )
    }

    val configNames: List[String] = formTemplate.authConfig match {
      case Composite(configs) => configs.map(_.authConfigName).toList
      case _                  => Nil
    }

    val items = configNames flatMap {
      case configName @ AuthConfig.hmrcSimpleModule =>
        ggId match {
          case Some(id) =>
            List(
              RadioItem(
                value = Some(id),
                content = Text(messages("compositeAuth.ggContinueContent"))
              ),
              RadioItem(
                value = Some(configName),
                content = Text(messages("compositeAuth.ggDifferentContent")),
                conditionalHtml = Some(
                  p(
                    messages("compositeAuth.ggDifferentConditional"),
                    "govuk-body"
                  )
                )
              )
            )

          case None =>
            List(
              RadioItem(
                value = Some(configName),
                content = Text(messages("compositeAuth.ggContent"))
              )
            )
        }

      case configName @ AuthConfig.email =>
        List(
          RadioItem(
            value = Some(configName),
            content = Text(messages("compositeAuth.emailContent"))
          )
        )
    }

    val radios = Radios(
      errorMessage = if (se === SuppressErrors.No) errorMessage else None,
      name = "compositeAuthSelection",
      items = items
    )

    new GovukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
