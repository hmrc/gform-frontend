/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

sealed trait SectionValidator {
  def errorMessage: String
}

case object SectionValidator {

  val reads: Reads[SectionValidator] = Reads { json =>
    (json \ "validatorName").as[String] match {
      case "hmrcUTRPostcodeCheck" => json.validate[HMRCUTRPostcodeCheckValidator]
      case "bankAccountModulusCheck" => json.validate[BankAccoutnModulusCheck]
    }
  }

  val writes: OWrites[SectionValidator] = OWrites {
    case v: HMRCUTRPostcodeCheckValidator => HMRCUTRPostcodeCheckValidator.format.writes(v)

  }

  implicit val format = OFormat(reads, writes)

}

case class HMRCUTRPostcodeCheckValidator(errorMessage: String, utr: FormCtx, postcode: FormCtx) extends SectionValidator {

  val utrFieldId = FieldId(utr.value)
  val postcodeFieldId = FieldId(postcode.value)
}

object HMRCUTRPostcodeCheckValidator {
  val basic: OFormat[HMRCUTRPostcodeCheckValidator] = Json.format[HMRCUTRPostcodeCheckValidator]
  val writes: OWrites[HMRCUTRPostcodeCheckValidator] = OWrites { o =>
    Json.obj("validatorName" -> "bankAccountModulusCheck") ++
      basic.writes(o)
  }

  val readCustom: Reads[HMRCUTRPostcodeCheckValidator] = ((JsPath \ "errorMessage").read[String] and
    (JsPath \ "parameters" \\ "utr").read[FormCtx] and
    (JsPath \ "parameters" \\ "postcode").read[FormCtx])(HMRCUTRPostcodeCheckValidator.apply _)

  val reads = readCustom | (basic: Reads[HMRCUTRPostcodeCheckValidator])
  implicit val format: OFormat[HMRCUTRPostcodeCheckValidator] = OFormat(reads, writes)
}

case class BankAccoutnModulusCheck(errorMessage: String, accountNumber: FormCtx, sortCode: FormCtx) extends SectionValidator {

  val accountNumberId = accountNumber.toFieldId
  val sortCodeId = sortCode.toFieldId
}

object BankAccoutnModulusCheck {
  val basic: OFormat[BankAccoutnModulusCheck] = Json.format[BankAccoutnModulusCheck]
  val writesCustom: OWrites[BankAccoutnModulusCheck] = OWrites { o =>
    Json.obj("validatorName" -> "bankAccountModulusCheck") ++
      basic.writes(o)
  }

  val writes: OWrites[BankAccoutnModulusCheck] = writesCustom
  val readCustom: Reads[BankAccoutnModulusCheck] = ((JsPath \ "errorMessage").read[String] and
    (JsPath \ "parameters" \\ "accountNumber").read[FormCtx] and
    (JsPath \ "parameters" \\ "sortCode").read[FormCtx])(BankAccoutnModulusCheck.apply _)

  val reads = readCustom | (basic: Reads[BankAccoutnModulusCheck])
  implicit val format: OFormat[BankAccoutnModulusCheck] = OFormat(reads, writesCustom)
}
