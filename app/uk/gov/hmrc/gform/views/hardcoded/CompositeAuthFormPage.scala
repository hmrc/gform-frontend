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

import play.api.data.Form
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, FormTemplate }
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.views.html.hardcoded.pages.pWithStrong
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers

class CompositeAuthFormPage(
  val formTemplate: FormTemplate,
  form: Form[String],
  ggId: Option[String]
)(implicit messages: Messages)
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
                  pWithStrong(
                    messages("compositeAuth.ggDifferentConditional"),
                    FormDataHelpers.addSpacesInGovermentGatewayId(id),
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
      errorMessage = errorMessage,
      name = "compositeAuthSelection",
      items = items
    )

    new govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
