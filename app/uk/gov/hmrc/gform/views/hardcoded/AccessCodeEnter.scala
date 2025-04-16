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

import cats.implicits.catsSyntaxEq
import play.api.data.{ Form, FormError }
import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.gform.AccessCodeForm
import uk.gov.hmrc.gform.models.AccessCodePage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, SuppressErrors }
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.govukfrontend.views.html.helpers.{ GovukFormGroup, GovukHintAndErrorMessage }
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }

class AccessCodeEnter(
  val formTemplate: FormTemplate,
  form: Form[AccessCodeForm],
  val isContinue: Boolean,
  se: SuppressErrors
)(implicit
  messages: Messages
) extends CommonPageProperties(formTemplate) {

  val accessCodeName = messages(s"accessCode.$draftRetrievalMethod")

  private val govukErrorMessage: GovukErrorMessage = new GovukErrorMessage()
  private val govukHint: GovukHint = new GovukHint()
  private val govukLabel: GovukLabel = new GovukLabel()
  private val govukFormGroup: GovukFormGroup = new GovukFormGroup
  private val govukHintAndErrorMessage: GovukHintAndErrorMessage =
    new GovukHintAndErrorMessage(govukHint, govukErrorMessage)

  val accessCodeValue = form(AccessCodePage.key).value.getOrElse("")

  val errorSummary: ErrorSummary = {

    val errorsHtml: Seq[ErrorLink] = (form.errors ++ form.globalErrors)
      .filterNot(error => error.key.contains(AccessCodePage.isContinueKey))
      .map { error =>
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

  val hasErrors: Boolean =
    if (se === SuppressErrors.No)
      errorSummary.errorList.nonEmpty
    else false

  val render: Html = {

    val errorMessage: Option[ErrorMessage] = form.errors
      .filterNot(error => error.key.contains(AccessCodePage.isContinueKey))
      .headOption
      .map { error =>
        val message = messages(s"${error.key}.${error.message}", formCategory)
        ErrorMessage.errorMessageWithDefaultStringsTranslated(
          content = Text(message)
        )
      }

    val accessCodeError: Option[FormError] = form.error(AccessCodePage.key)

    val label = Label(
      content = Text(messages("accessCode.enterKey", accessCodeName)),
      isPageHeading = false,
      classes = "govuk-fieldset__legend--m"
    )

    val hint: Hint = Hint(
      content = Text(messages(s"accessCode.$draftRetrievalMethod.keyHintText"))
    )

    val html: Html = {

      val input = Input(
        id = AccessCodePage.key,
        name = AccessCodePage.key,
        label = label,
        hint = Some(hint),
        value = Some(accessCodeValue),
        classes = "govuk-input--width-10",
        errorMessage = if (hasErrors) accessCodeError.flatMap(_ => errorMessage) else None
      )
      val inputHtml = new GovukInput(govukLabel, govukFormGroup, govukHintAndErrorMessage)(input)
      HtmlFormat.fill(List(inputHtml))
    }

    html
  }

}
