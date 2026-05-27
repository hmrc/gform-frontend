/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import julienrf.json.derived
import play.api.libs.json.{ Format, JsObject, OFormat }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ADTFormat

case class DataRetrieveDescription(
  tpe: String,
  exampleJson: JsObject,
  attributeReferences: List[String],
  documentationUrl: Option[String],
  isArrayResult: Boolean,
  urlDescriptors: List[UrlDescriptor]
)

object DataRetrieveDescription {
  implicit val format: OFormat[DataRetrieveDescription] = derived.oformat()
}

sealed trait UrlDestination extends Product with Serializable

object UrlDestination {
  case object GForm extends UrlDestination
  case object MDTP extends UrlDestination
  case object DES extends UrlDestination
  case object HIP extends UrlDestination
  case object CompaniesHouse extends UrlDestination

  val companiesHouse = "Companies House"

  implicit val format: Format[UrlDestination] =
    ADTFormat.formatEnumeration(
      "GForm"        -> GForm,
      "MDTP"         -> MDTP,
      "DES"          -> DES,
      "HIP"          -> HIP,
      companiesHouse -> CompaniesHouse
    )

  def asString(urlDestination: UrlDestination): String = urlDestination match {
    case CompaniesHouse => companiesHouse
    case other          => other.toString
  }
}

final case class UrlDescriptor(
  urlPath: String,
  pathParameters: List[DataRetrieve.Parameter],
  destination: UrlDestination
)

object UrlDescriptor {
  implicit val format: OFormat[UrlDescriptor] = derived.oformat()
}
