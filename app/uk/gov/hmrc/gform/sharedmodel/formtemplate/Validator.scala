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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.SmartString

sealed trait Validator {
  def errorMessage: SmartString
}

case object Validator {
  implicit val format: OFormat[Validator] = derived.oformat()
}

case class HmrcRosmRegistrationCheckValidator(
  errorMessage: SmartString,
  regime: String,
  utr: FormCtx,
  postcode: FormCtx
) extends Validator {
  val utrFieldId = utr.formComponentId
  val postcodeFieldId = postcode.formComponentId
}

object HmrcRosmRegistrationCheckValidator {
  implicit val format: OFormat[HmrcRosmRegistrationCheckValidator] = derived.oformat()
}

case class BankAccountModulusCheck(
  errorMessage: SmartString,
  accountNumber: FormCtx,
  sortCode: FormCtx
) extends Validator {

  val accountNumberId = accountNumber.formComponentId
  val sortCodeId = sortCode.formComponentId
}

object BankAccountModulusCheck {
  implicit val format: OFormat[BankAccountModulusCheck] = derived.oformat()
}
