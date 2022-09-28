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

package uk.gov.hmrc.gform.models

import play.api.libs.json.{ OFormat, Reads, __ }
import play.api.libs.functional.syntax._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

object CompanyProfile {

  def createFullRegisteredAddress(r: RegisteredAddress) =
    s"${r.addressLine1} ${r.addressLine2.getOrElse("")} ${r.postalCode} ${r.locality} ${r.region.getOrElse("")}"

  case class RegisteredAddress(
    addressLine1: String,
    addressLine2: Option[String],
    postalCode: String,
    locality: String,
    region: Option[String]
  )

  object RegisteredAddress {
    private val templateReads: Reads[RegisteredAddress] =
      ((__ \ "address_line_1").read[String] and
        (__ \ "address_line_2").readNullable[String] and
        (__ \ "postal_code").read[String] and
        (__ \ "locality").read[String] and
        (__ \ "region").readNullable[String])(RegisteredAddress.apply _)

    implicit val format: OFormat[RegisteredAddress] = OFormatWithTemplateReadFallback(templateReads)
  }

  case class Response(
    name: String,
    status: String,
    registeredAddress: RegisteredAddress
  )

  object Response {

    private val templateReads: Reads[Response] =
      ((__ \ "company_name").read[String] and
        (__ \ "company_status").read[String] and
        (__ \ "registered_office_address").read[RegisteredAddress])(Response.apply _)

    implicit val format: OFormat[Response] = OFormatWithTemplateReadFallback(templateReads)
  }
}
