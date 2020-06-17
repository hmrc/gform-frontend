/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BySubmissionReference, FormTemplate }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ p, strong }

class SaveWithAccessCode(val formTemplate: FormTemplate, accessCode: AccessCode)(implicit messages: Messages)
    extends CommonAgentPageProperties(formTemplate, accessCode) {

  val heading = messages("accessCode.saved.title", formCat)

  private val panel = Panel(
    title = Text(heading),
    content = HtmlContent(
      HtmlFormat.fill(
        List(
          p(messages("accessCode.new.keepNote", accessCodeName)),
          strong(accessCode.value)
        )
      )
    )
  )

  val panelHtml: Html = new govukPanel()(panel)

  val paragraph = p(
    messages("accessCode.new.validFor", accessCodeName)
      + "." +
      messages("accessCode.new.willExpire", formCat, accessCodeName),
    "govuk-body")

  private val warningText = WarningText(content = Text(messages("accessCode.mustSubmit", formCat)))

  val warningHtml = new govukWarningText()(warningText)

}
