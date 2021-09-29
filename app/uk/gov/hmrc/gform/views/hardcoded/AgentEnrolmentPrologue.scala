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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.html.components._

class AgentEnrolmentProlog(val formTemplate: FormTemplate, url: String)(implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {

  val inset = new GovukInsetText()(
    InsetText(
      content = Text(messages("agentsPrologue.p2") + ".")
    )
  )

  val button = new GovukButton()(
    Button(
      href = Some(url),
      isStartButton = true,
      content = Text(messages("agentsPrologue.linkText"))
    )
  )
}
