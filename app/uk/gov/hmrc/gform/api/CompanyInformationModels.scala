/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.api

import play.api.libs.json.{ JsPath, Json, OFormat, Reads }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.OFormatWithTemplateReadFallback

import java.time.LocalDate

case class Officers(items: List[Officer])

object Officers {
  implicit val format: OFormat[Officers] = Json.format[Officers]
}

case class Officer(officerRole: String, resignedOn: Option[String] = None)

object Officer {
  val reads: Reads[Officer] = for {
    officerRole <- (JsPath \ "officer_role").read[String]
    resignedOn  <- (JsPath \ "resigned_on").readNullable[String]
  } yield Officer(officerRole, resignedOn)

  implicit val format: OFormat[Officer] = OFormatWithTemplateReadFallback(reads)
}

case class Insolvency(cases: List[InsolvencyCase])

object Insolvency {
  implicit val format: OFormat[Insolvency] = Json.format[Insolvency]
}

case class InsolvencyCase(
  `type`: String,
  number: String,
  practitioners: List[InsolvencyPractitioner]
)

object InsolvencyCase {
  implicit val format: OFormat[InsolvencyCase] = Json.format[InsolvencyCase]
}

case class InsolvencyPractitioner(
  name: String,
  address: InsolvencyPractitionerAddress,
  role: String,
  appointed_on: Option[LocalDate],
  ceased_to_act_on: Option[LocalDate]
)

object InsolvencyPractitioner {
  implicit val format: OFormat[InsolvencyPractitioner] = Json.format[InsolvencyPractitioner]
}

case class InsolvencyPractitionerAddress(
  address_line_1: String,
  address_line_2: Option[String],
  country: Option[String],
  locality: Option[String],
  postal_code: Option[String],
  region: Option[String]
) {
  override def toString: String = Seq(Some(address_line_1), address_line_2, locality, region, postal_code, country)
    .filter(_.isDefined)
    .map(_.get)
    .mkString(", ")
}

object InsolvencyPractitionerAddress {
  implicit val format: OFormat[InsolvencyPractitionerAddress] = Json.format[InsolvencyPractitionerAddress]
}
