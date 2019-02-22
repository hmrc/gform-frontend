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

package uk.gov.hmrc.gform.auth.models

import play.api.libs.json._
import org.joda.time.LocalDate
import uk.gov.hmrc.auth.core.AffinityGroup

case class UserDetails(
  authProviderId: Option[String],
  authProviderType: Option[AuthProviderType],
  name: String,
  lastName: Option[String] = None,
  dateOfBirth: Option[LocalDate] = None,
  postCode: Option[String] = None,
  email: Option[String] = None,
  affinityGroup: AffinityGroup,
  agentCode: Option[String] = None,
  agentId: Option[String] = None,
  agentFriendlyName: Option[String] = None,
  credentialRole: Option[String] = None,
  description: Option[String] = None,
  groupIdentifier: String
)

object UserDetails {
  val pattern = "yyyy-MM-dd'T'HH:mm:ss.SSSZ"
  implicit val dateFormat = Format[LocalDate](Reads.jodaLocalDateReads(pattern), Writes.jodaLocalDateWrites(pattern))

  implicit lazy val format: OFormat[UserDetails] = Json.format[UserDetails]
}

sealed trait AuthProviderType
final case object GovernmentGateway extends AuthProviderType
final case object Verify extends AuthProviderType
final case object PrivilegedApplication extends AuthProviderType

object AuthProviderType {
  implicit lazy val format: OFormat[AuthProviderType] = {
    val writes: OWrites[AuthProviderType] = OWrites {
      case GovernmentGateway     => JsObject(Seq("authProviderType" -> JsString("GovernmentGateway")))
      case Verify                => JsObject(Seq("authProviderType" -> JsString("Verify")))
      case PrivilegedApplication => JsObject(Seq("authProviderType" -> JsString("PrivilegedApplication")))
    }

    val reads: Reads[AuthProviderType] = Reads {
      case JsString("GovernmentGateway")     => JsSuccess(GovernmentGateway)
      case JsString("Verify")                => JsSuccess(Verify)
      case JsString("PrivilegedApplication") => JsSuccess(PrivilegedApplication)
      case others                            => JsError(s"Unrecognised value in authProviderType: $others")
    }

    OFormat(reads, writes)
  }

}
