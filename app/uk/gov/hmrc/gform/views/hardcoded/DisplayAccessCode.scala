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
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.html.components._

class DisplayAccessCode(val formTemplate: FormTemplate, val accessCode: AccessCode)(implicit messages: Messages)
    extends CommonAgentPageProperties(formTemplate, accessCode) {

  val heading = messages("accessCode.new.title", accessCodeName)

  private val panel = Panel(
    title = Text(heading),
    content = HtmlContent(acStrong)
  )

  val panelHtml: Html = new GovukPanel()(panel)

}
