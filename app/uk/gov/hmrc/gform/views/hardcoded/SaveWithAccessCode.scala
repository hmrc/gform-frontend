/*
 * Copyright 2023 HM Revenue & Customs
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
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ p, strong }

class SaveWithAccessCode(val formTemplate: FormTemplate, accessCode: AccessCode, frontendAppConfig: FrontendAppConfig)(
  implicit messages: Messages
) extends CommonAgentPageProperties(formTemplate, accessCode) {

  val heading = messages("accessCode.saved.title", formCat)

  private val panel = Panel(
    title = Text(heading),
    content = HtmlContent(
      HtmlFormat.fill(
        List(
          p(messages("accessCode.saved.p1", accessCodeName) + ":"),
          strong(accessCode.value)
        )
      )
    )
  )

  val panelHtml: Html = new GovukPanel()(panel)

  private val insetText =
    InsetText(
      content = HtmlContent(
        HtmlFormat.fill(
          List(
            p(messages("accessCode.new.keepNote", accessCodeName, formCat) + ":"),
            p(accessCode.value, "govuk-body-l govuk-!-font-weight-bold govuk-!-margin-bottom-1")
          )
        )
      ),
      classes = "gforms-inset-text--important"
    )

  override val insetHtml: Html = new GovukInsetText()(insetText)

  private val warningText = WarningText(content = Text(messages("accessCode.mustSubmit", formCat)))

  val warningHtml = new GovukWarningText()(warningText)

}
