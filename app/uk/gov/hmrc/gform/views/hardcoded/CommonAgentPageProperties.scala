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
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ p, strong }

class CommonAgentPageProperties(formTemplate: FormTemplate, accessCode: AccessCode)(implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {
  val accessCodeName = messages(s"accessCode.$draftRetrievalMethod")

  protected val acStrong = strong(accessCode.value)

  private val insetText =
    InsetText(
      content = HtmlContent(
        HtmlFormat.fill(
          List(
            p(accessCodeName.capitalize + ":"),
            p(accessCode.value, "govuk-body-l govuk-!-font-weight-bold govuk-!-margin-bottom-1")
          )
        )
      ),
      classes = "gforms-inset-text--important"
    )

  val insetHtml: Html = new GovukInsetText()(insetText)

}
