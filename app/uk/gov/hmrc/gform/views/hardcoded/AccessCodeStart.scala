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

import play.api.data.{ Form, FormError }
import play.api.i18n.Messages
import play.twirl.api.{ Html, HtmlFormat }
import uk.gov.hmrc.gform.gform.AccessCodeForm
import uk.gov.hmrc.gform.models.AccessCodePage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.{ RadioItem, Radios }

class AccessCodeStart(val formTemplate: FormTemplate, form: Form[AccessCodeForm])(implicit messages: Messages)
    extends CommonPageProperties(formTemplate) {

  val accessCodeName = messages(s"accessCode.$draftRetrievalMethod")

  private val govukErrorMessage: govukErrorMessage = new govukErrorMessage()
  private val govukFieldset: govukFieldset = new govukFieldset()
  private val govukHint: govukHint = new govukHint()
  private val govukLabel: govukLabel = new govukLabel()

  val errorSummary: ErrorSummary = {

    val errorsHtml: Seq[ErrorLink] = (form.errors ++ form.globalErrors).map { error =>
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

  val hasErrors: Boolean = errorSummary.errorList.nonEmpty

  val render: Html = {

    val errorMessage: Option[ErrorMessage] = form.errors.headOption.map { error =>
      val message = messages(s"${error.key}.${error.message}", formCategory)
      ErrorMessage(
        content = Text(message)
      )
    }

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = Text(messages("accessCode.p3", formCat)),
            isPageHeading = false,
            classes = "govuk-label--m"
          )
        )
      )
    )

    val accessCodeValue = form(AccessCodePage.key).value.getOrElse("")
    val optionValue = form(AccessCodePage.optionKey).value.getOrElse("")

    val optionError: Option[FormError] = form.error(AccessCodePage.optionKey)
    val accessCodeError: Option[FormError] = form.error(AccessCodePage.key)

    val label = Label(
      content = Text(messages("accessCode.enterKey", accessCodeName))
    )

    val hint: Hint = Hint(
      content = Text(messages(s"accessCode.$draftRetrievalMethod.keyHintText"))
    )

    val conditionalHtml: Html = {

      val xs = List(
        messages("accessCode.help.p1", formCat, accessCodeName) + ".",
        messages("accessCode.help.p2", formCat, accessCodeName) + ".",
        messages(s"accessCode.$draftRetrievalMethod.help.p3"),
        messages("accessCode.help.p4", formCat, accessCodeName) + "."
      ).map(x => uk.gov.hmrc.gform.views.html.hardcoded.pages.p(x, "govuk-body"))

      val details = Details(
        summary = Text(messages("accessCode.help.title", accessCodeName)),
        content = HtmlContent(HtmlFormat.fill(xs))
      )

      val input = Input(
        id = AccessCodePage.key,
        name = AccessCodePage.key,
        label = label,
        hint = Some(hint),
        value = Some(accessCodeValue),
        classes = "govuk-input--width-10 govuk-!-margin-bottom-5",
        errorMessage = accessCodeError.flatMap(_ => errorMessage)
      )
      val inputHtml = new govukInput(govukErrorMessage, govukHint, govukLabel)(input)
      val detailsHtml = new govukDetails()(details)
      HtmlFormat.fill(List(inputHtml, detailsHtml))
    }

    val startNew = RadioItem(
      value = Some(AccessCodePage.optionNew),
      content = Text(messages("accessCode.startNew", formCat))
    )

    val useExisting = RadioItem(
      value = Some(AccessCodePage.optionAccess),
      content = Text(messages("accessCode.useExisting", formCat)),
      conditionalHtml = Some(conditionalHtml),
      checked = optionValue == AccessCodePage.optionAccess
    )

    val radios = Radios(
      fieldset = fieldset,
      errorMessage = optionError.flatMap(_ => errorMessage),
      name = "accessOption",
      items = List(startNew, useExisting)
    )

    new govukRadios(govukErrorMessage, govukFieldset, govukHint, govukLabel)(radios)
  }

}
