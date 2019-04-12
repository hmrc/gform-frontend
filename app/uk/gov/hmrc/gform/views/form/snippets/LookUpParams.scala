/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.views.form.snippets

import play.twirl.api.Html
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.validation.FormFieldValidationResult

case class LookUpParams(
  id: String,
  value: Html,
  label: Html,
  helpText: Html,
  options: List[LookUpOption],
  fieldValue: FormComponent,
  validationResult: Option[FormFieldValidationResult],
  formLevelHeading: Boolean,
  inputClasses: String,
  lookup: String,
  showAll: Boolean
)

case class LookUpOption(
  value: String,
  label: String
)
