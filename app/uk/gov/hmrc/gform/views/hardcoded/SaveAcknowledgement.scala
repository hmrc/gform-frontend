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
import java.time.format.DateTimeFormatter
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeExpiryDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.views.html.localisedDateString
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ p, pWithBreak }

class SaveAcknowledgement(val formTemplate: FormTemplate, envelopeExpiryDate: Option[EnvelopeExpiryDate])(
  implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {

  private val formatter = DateTimeFormatter.ofPattern("dd MMMM yyyy")
  private val expiryDate = envelopeExpiryDate.fold(messages("save4later.for30days"))(exD =>
    messages("save4later.until") + " " + localisedDateString(exD.ldt.format(formatter)))

  val heading = messages("save4later.saved.title", formCat)

  private val panel = Panel(
    title = Text(heading),
    content = HtmlContent(
      pWithBreak(messages("save4later.infoHeld"), expiryDate + ".")
    )
  )

  val panelHtml: Html = new govukPanel()(panel)

  private val insetText =
    InsetText(
      content = HtmlContent(p(messages("save4later.mustSubmit", formCat), "govuk-!-font-weight-bold"))
    )

  val insetHtml: Html = new govukInsetText()(insetText)
}
