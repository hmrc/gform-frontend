/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.addresslookup

import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.gform.{ HasErrors, PageLevelErrorHtml }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, FormComponent }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.ErrorLink

class EnterAddressPage(
  formComponent: FormComponent,
  formFieldValidationResult: FormFieldValidationResult
)(implicit
  messages: Messages,
  l: LangADT,
  sse: SmartStringEvaluator
) {

  val errorSummary: HasErrors =
    PageLevelErrorHtml.generatePageLevelErrorHtml(List(formFieldValidationResult), List.empty[ErrorLink])

  val hasErrors: Boolean = errorSummary.hasErrors

  val render: Html =
    html.form.snippets
      .field_template_address(
        Address(false, List.empty[Address.Configurable.Mandatory]),
        formComponent,
        formFieldValidationResult,
        false,
        ""
      )

}
