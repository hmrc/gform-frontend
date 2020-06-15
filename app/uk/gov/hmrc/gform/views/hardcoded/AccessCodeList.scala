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
import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.FormOverview
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.views.html.formatInstant
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.gform.routes.NewFormController
import uk.gov.hmrc.gform.views.html.hardcoded.pages.link

class AccessCodeList(val formTemplate: FormTemplate, formOverviews: List[FormOverview])(implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {

  private def accessLink(submissionRef: SubmissionRef): Html =
    link(submissionRef.value, NewFormController.continue(formTemplate._id, submissionRef))

  val render: Html = {

    val tableRows: List[List[TableRow]] = formOverviews.map { formOverview =>
      List(
        TableRow(
          content = HtmlContent(formOverview.submissionRef.fold(Html(""))(submissionRef => accessLink(submissionRef)))
        ),
        TableRow(
          content = Text(formatInstant(formOverview.createdAt))
        ),
        TableRow(
          content = Text(formatInstant(formOverview.updatedAt))
        )
      )
    }

    val header: List[HeadCell] = List(
      HeadCell(
        content = Text(messages("accessCodeLinks.label"))
      ),
      HeadCell(
        content = Text(messages("accessCodeLinks.created"))
      ),
      HeadCell(
        content = Text(messages("accessCodeLinks.modified"))
      )
    )

    val table = Table(
      head = Some(header),
      rows = tableRows,
      firstCellIsHeader = true
    )

    new govukTable()(table)
  }

}
