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
import uk.gov.hmrc.gform.models.AccessCodePage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BySubmissionReference, FormTemplate }
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ p, strong }

class DisplayAccessCode(val formTemplate: FormTemplate, val accessCode: AccessCodePage)(implicit messages: Messages) {
  private val formCategory = uk.gov.hmrc.gform.views.hardcoded.pages.formCategory(formTemplate)

  val formCat = messages(s"formCategory.$formCategory")

  private val draftRetrievalMethod = formTemplate.draftRetrievalMethod match {
    case BySubmissionReference => "submissionReference"
    case _                     => "formAccessCodeForAgents"
  }

  val accessCodeName = messages(s"accessCode.$draftRetrievalMethod")

  val heading = messages("accessCode.new.title", accessCodeName) + ":"

  private val acStrong = strong(accessCode.value)

  private val panel = Panel(
    title = Text(heading),
    content = HtmlContent(acStrong)
  )

  val panelHtml: Html = new govukPanel()(panel)

  private val insetText =
    InsetText(
      content = HtmlContent(
        HtmlFormat.fill(
          List(
            p(messages("accessCode.new.keepNote", accessCodeName)),
            acStrong,
            p(messages("accessCode.new.youWillBeAsked", formCat) + ".")
          ))
      )
    )

  val insetHtml: Html = new govukInsetText()(insetText)

}
