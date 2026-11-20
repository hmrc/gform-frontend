/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.bulkupload

import play.api.data.Form
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.bulkupload.BulkUploadConstant.BULK_UPLOAD_REMOVE_ROW
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.html.components.{ ErrorMessage, GovukRadios }
import uk.gov.hmrc.govukfrontend.views.html.helpers.{ GovukFormGroup, GovukHintAndErrorMessage }
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.{ RadioItem, Radios }

class ConfirmRemovePage(
  form: Form[String]
)(implicit
  messages: Messages
) {
  private val govukErrorMessage: components.GovukErrorMessage = new components.GovukErrorMessage()
  private val govukFieldset: components.GovukFieldset = new components.GovukFieldset()
  private val govukHint: components.GovukHint = new components.GovukHint()
  private val govukLabel: components.GovukLabel = new components.GovukLabel()
  private val govukHintAndErrorMessage: GovukHintAndErrorMessage =
    new GovukHintAndErrorMessage(govukHint, govukErrorMessage)
  private val govukFormGroup: GovukFormGroup = new GovukFormGroup

  val errorSummary: ErrorSummary = {

    val errorsHtml: Seq[ErrorLink] = form.errors.map { error =>
      ErrorLink(
        href = Some("#" + error.key),
        content = Text(messages(s"${error.key}.${error.message}"))
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
      val message = messages(s"${error.key}.${error.message}")
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = Text(message)
      )
    }

    val items = List(
      ("yes" -> s"$BULK_UPLOAD_REMOVE_ROW.yes"),
      ("no"  -> s"$BULK_UPLOAD_REMOVE_ROW.no")
    ).map { case (value, label) =>
      RadioItem(
        value = Some(value),
        content = Text(messages(label))
      )
    }

    val radios = Radios(
      errorMessage = errorMessage,
      name = BULK_UPLOAD_REMOVE_ROW,
      items = items.toList
    )

    new GovukRadios(
      govukFieldset = govukFieldset,
      govukHint = govukHint,
      govukLabel = govukLabel,
      govukFormGroup = govukFormGroup,
      govukHintAndErrorMessage = govukHintAndErrorMessage
    )(radios)
  }
}
