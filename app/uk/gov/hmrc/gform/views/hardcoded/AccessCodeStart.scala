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

import play.api.data.{ Form, FormError }
import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.models.AccessCodePage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.govukfrontend.views.html.helpers.{ GovukFormGroup, GovukHintAndErrorMessage }
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.{ RadioItem, Radios }

class AccessCodeStart(val formTemplate: FormTemplate, form: Form[String], frontendAppConfig: FrontendAppConfig)(implicit
  messages: Messages
) extends CommonPageProperties(formTemplate) {

  val accessCodeName = messages(s"accessCode.$draftRetrievalMethod")

  private val govukErrorMessage: GovukErrorMessage = new GovukErrorMessage()
  private val govukFieldset: GovukFieldset = new GovukFieldset()
  private val govukHint: GovukHint = new GovukHint()
  private val govukLabel: GovukLabel = new GovukLabel()
  private val govukFormGroup: GovukFormGroup = new GovukFormGroup
  private val govukHintAndErrorMessage: GovukHintAndErrorMessage =
    new GovukHintAndErrorMessage(govukHint, govukErrorMessage)

  val errorSummary: ErrorSummary = {

    val errorsHtml: Seq[ErrorLink] = (form.errors ++ form.globalErrors).map { error =>
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

    val errorMessage: Option[ErrorMessage] = form.errors.headOption.map { error =>
      val message = messages(s"${error.key}.${error.message}", formCategory)
      ErrorMessage.errorMessageWithDefaultStringsTranslated(
        content = Text(message)
      )
    }

    val fieldset = Some(
      Fieldset(
        legend = Some(
          Legend(
            content = Text(messages("accessCode.p3")),
            isPageHeading = false,
            classes = "govuk-fieldset__legend--m"
          )
        )
      )
    )

    val optionError: Option[FormError] = form.error(AccessCodePage.optionKey)

    val startNew = RadioItem(
      value = Some(AccessCodePage.optionNew),
      content = Text(messages("accessCode.startNew", formCat))
    )

    val useExisting = RadioItem(
      value = Some(AccessCodePage.optionContinue),
      content = Text(messages("accessCode.useExisting", formCat))
    )

    val divider = RadioItem(
      divider = Some(messages("global.or"))
    )

    val downloadHint: Hint = Hint(
      content =
        Text(messages(s"accessCode.downloadSubmitted.hintText", formCat, frontendAppConfig.submittedFormExpiryDays))
    )

    val downloadSubmitted = RadioItem(
      value = Some(AccessCodePage.optionDownload),
      content = Text(messages("accessCode.downloadSubmitted", formCat)),
      hint = Some(downloadHint)
    )

    val radios = Radios(
      fieldset = fieldset,
      errorMessage = optionError.flatMap(_ => errorMessage),
      name = "accessOption",
      items = List(startNew, useExisting, divider, downloadSubmitted)
    )

    new GovukRadios(govukFieldset, govukHint, govukLabel, govukFormGroup, govukHintAndErrorMessage)(radios)
  }

}
