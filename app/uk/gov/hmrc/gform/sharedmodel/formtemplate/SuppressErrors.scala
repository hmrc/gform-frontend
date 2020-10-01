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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import uk.gov.hmrc.gform.gform.handlers.FormHandlerResult
import uk.gov.hmrc.gform.validation.ValidationResult

sealed trait SuppressErrors extends Product with Serializable {
  def asString: String = this match {
    case SuppressErrors.Yes => SuppressErrors.seYes
    case SuppressErrors.No  => SuppressErrors.seNo
  }

  def apply(formHandlerResult: FormHandlerResult): FormHandlerResult =
    formHandlerResult.copy(validationResult = apply(formHandlerResult.validationResult))

  def apply(validationResult: ValidationResult): ValidationResult = this match {
    case SuppressErrors.Yes => validationResult.forgetErrors
    case SuppressErrors.No  => validationResult
  }

}

object SuppressErrors {

  case object Yes extends SuppressErrors
  case object No extends SuppressErrors

  val seYes = "t"
  val seNo = "f"

  def apply(b: Boolean) = if (b) Yes else No

}
