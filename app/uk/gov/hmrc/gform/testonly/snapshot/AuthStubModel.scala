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

package uk.gov.hmrc.gform.testonly.snapshot

import java.net.URLEncoder
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve._
import uk.gov.hmrc.auth.core.retrieve.{ ItmpAddress => AuthItmpAddress }
import uk.gov.hmrc.domain.Nino

case class EnrolmentData(name: String, state: String, taxIdentifier: List[TaxIdentifierData])

object EnrolmentData {
  implicit val format: OFormat[EnrolmentData] = derived.oformat()

  implicit val encoder: UrlFormEncoder[EnrolmentData] = new UrlFormEncoder[EnrolmentData] {
    def encode(value: EnrolmentData): List[(String, String)] =
      List(
        "name"  -> value.name,
        "state" -> value.state
      ) ++
        UrlFormEncoder.withKey(
          "taxIdentifier",
          UrlFormEncoder[List[TaxIdentifierData]].encode(value.taxIdentifier)
        )

  }
}

case class TaxIdentifierData(key: String, value: String)
object TaxIdentifierData {
  implicit val format: OFormat[TaxIdentifierData] = derived.oformat()

  implicit val encoder: UrlFormEncoder[TaxIdentifierData] = new UrlFormEncoder[TaxIdentifierData] {
    def encode(value: TaxIdentifierData): List[(String, String)] =
      List(
        "name"  -> value.key,
        "value" -> value.value
      )
  }
}

case class ItmpAddress(
  line1: Option[String],
  line2: Option[String],
  line3: Option[String],
  line4: Option[String],
  line5: Option[String],
  postCode: Option[String],
  countryCode: Option[String],
  countryName: Option[String]
)

object ItmpAddress {
  implicit val format: OFormat[ItmpAddress] = derived.oformat()
  def apply(authItmpAddress: AuthItmpAddress): ItmpAddress = ItmpAddress(
    line1 = authItmpAddress.line1,
    line2 = authItmpAddress.line2,
    line3 = authItmpAddress.line3,
    line4 = authItmpAddress.line4,
    line5 = authItmpAddress.line5,
    postCode = authItmpAddress.postCode,
    countryCode = authItmpAddress.countryCode,
    countryName = authItmpAddress.countryName
  )
  def apply(): ItmpAddress = ItmpAddress(None, None, None, None, None, None, None, None)

}

case class ItmpData(
  givenName: Option[String],
  middleName: Option[String],
  familyName: Option[String],
  birthdate: Option[String],
  address: ItmpAddress
)

object ItmpData {
  implicit val writes: OFormat[ItmpData] = derived.oformat()

  implicit val encoder: UrlFormEncoder[ItmpData] = new UrlFormEncoder[ItmpData] {
    def encode(value: ItmpData): List[(String, String)] =
      List(
        "itmp.givenName"           -> value.givenName.getOrElse(""),
        "itmp.middleName"          -> value.middleName.getOrElse(""),
        "itmp.familyName"          -> value.familyName.getOrElse(""),
        "itmp.dateOfBirth"         -> value.birthdate.getOrElse(""),
        "itmp.address.line1"       -> value.address.line1.getOrElse(""),
        "itmp.address.line2"       -> value.address.line2.getOrElse(""),
        "itmp.address.line3"       -> value.address.line3.getOrElse(""),
        "itmp.address.line4"       -> value.address.line4.getOrElse(""),
        "itmp.address.line5"       -> value.address.line5.getOrElse(""),
        "itmp.address.postCode"    -> value.address.postCode.getOrElse(""),
        "itmp.address.countryCode" -> value.address.countryCode.getOrElse(""),
        "itmp.address.countryName" -> value.address.countryName.getOrElse("")
      )
  }
}

case class AgentData(
  agentId: Option[String] = None,
  agentCode: Option[String] = None,
  agentFriendlyName: Option[String] = None
)

object AgentData {
  implicit val writes: OFormat[AgentData] = derived.oformat()

  implicit val encoder: UrlFormEncoder[AgentData] = new UrlFormEncoder[AgentData] {
    def encode(value: AgentData): List[(String, String)] =
      List(
        "agent.agentId"           -> value.agentId.getOrElse(""),
        "agent.agentCode"         -> value.agentCode.getOrElse(""),
        "agent.agentFriendlyName" -> value.agentFriendlyName.getOrElse("")
      )
  }

  def apply(agent: AgentInformation): AgentData =
    AgentData(
      agentId = agent.agentId,
      agentCode = agent.agentCode,
      agentFriendlyName = agent.agentFriendlyName
    )
}

case class GovernmentGatewayFormData(
  redirectionUrl: String,
  credentialStrength: String,
  confidenceLevel: String,
  credId: String,
  nino: Option[Nino],
  enrolments: List[EnrolmentData],
  affinityGroup: String,
  credentialRole: String,
  usersName: Option[String],
  email: Option[String],
  gatewayToken: Option[String],
  groupIdentifier: Option[String],
  itmpData: Option[ItmpData],
  agent: Option[AgentData]
) {
  def withRedirectionUrl(redirectionUrl: String): GovernmentGatewayFormData =
    copy(redirectionUrl = redirectionUrl)

  def withCredentials(maybeCredentials: Option[Credentials]): GovernmentGatewayFormData =
    maybeCredentials match {
      case Some(credentials) if credentials.providerType == "GovernmentGateway" =>
        copy(credId = credentials.providerId)
      case _ => throw new IllegalArgumentException("Credentials must be of type GovernmentGateway")
    }

  def withEnrolments(enrolments: Enrolments): GovernmentGatewayFormData =
    copy(enrolments = enrolments.enrolments.map { enrolment =>
      EnrolmentData(
        enrolment.key,
        enrolment.state,
        enrolment.identifiers.map { case EnrolmentIdentifier(key, value) =>
          TaxIdentifierData(key, value)
        }.toList
      )
    }.toList)

  def withAffinityGroup(affinityGroup: Option[AffinityGroup]): GovernmentGatewayFormData =
    copy(affinityGroup = affinityGroup.map(_.toString).getOrElse(""))

  def withCredentialRole(credentialRole: Option[CredentialRole]): GovernmentGatewayFormData =
    copy(credentialRole = credentialRole.map(_.toString).getOrElse(""))

  def withCredentialStrength(credentialStrength: Option[String]): GovernmentGatewayFormData =
    copy(credentialStrength = credentialStrength.getOrElse(""))

  def withConfidenceLevel(confidenceLevel: ConfidenceLevel): GovernmentGatewayFormData =
    copy(confidenceLevel = confidenceLevel.toString)

  def withNino(maybeNino: Option[String]): GovernmentGatewayFormData =
    copy(nino = maybeNino.map(Nino(_)))

  def withName(name: Option[Name]): GovernmentGatewayFormData =
    copy(usersName = name.flatMap(_.name))

  def withEmail(email: Option[String]): GovernmentGatewayFormData =
    copy(email = email)

  def withGatewayToken(gatewayInformation: Option[GatewayInformation]): GovernmentGatewayFormData =
    copy(gatewayToken = gatewayInformation.flatMap(_.gatewayToken))

  def withGroupIdentifier(groupIdentifier: Option[String]): GovernmentGatewayFormData =
    copy(groupIdentifier = groupIdentifier)
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  def withItmpData(
    itmpName: Option[ItmpName],
    itmpDateOfBirth: Option[LocalDate],
    authItmpAddress: Option[AuthItmpAddress]
  ): GovernmentGatewayFormData =
    copy(itmpData =
      Some(
        ItmpData(
          givenName = itmpName.flatMap(_.givenName),
          middleName = itmpName.flatMap(_.middleName),
          familyName = itmpName.flatMap(_.familyName),
          birthdate = itmpDateOfBirth.map(_.format(formatter)),
          address = authItmpAddress.map(ItmpAddress(_)).getOrElse(ItmpAddress())
        )
      )
    )

  def withAgent(
    agentInformation: AgentInformation
  ): GovernmentGatewayFormData =
    copy(agent = Some(AgentData(agentInformation)))

}

object GovernmentGatewayFormData {

  implicit val format: OFormat[GovernmentGatewayFormData] = derived.oformat()

  implicit val encoder: UrlFormEncoder[GovernmentGatewayFormData] = new UrlFormEncoder[GovernmentGatewayFormData] {
    def encode(value: GovernmentGatewayFormData): List[(String, String)] =
      List(
        "redirectionUrl"     -> value.redirectionUrl,
        "credentialStrength" -> value.credentialStrength,
        "confidenceLevel"    -> value.confidenceLevel,
        "authorityId"        -> value.credId,
        "affinityGroup"      -> value.affinityGroup,
        "credentialRole"     -> value.credentialRole
      ) ++
        value.gatewayToken.map("gatewayToken" -> _).toList ++
        value.nino.map("nino" -> _.value).toList ++
        value.usersName.map("usersName" -> _).toList ++
        value.email.map("email" -> _).toList ++
        value.groupIdentifier.map("groupIdentifier" -> _).toList ++
        UrlFormEncoder.withKey(
          "enrolment",
          UrlFormEncoder[List[EnrolmentData]].encode(value.enrolments)
        ) ++
        value.itmpData.toList.flatMap(UrlFormEncoder[ItmpData].encode(_)) ++
        value.agent.toList.flatMap(UrlFormEncoder[AgentData].encode(_))

  }

  private def urlEncode(s: String): String = URLEncoder.encode(s, "UTF-8")
  def toUrlEncoded(data: GovernmentGatewayFormData): String =
    encoder.encode(data).map { case (k, v) => s"${urlEncode(k)}=${urlEncode(v)}" }.mkString("&")

  // create a "blank" ggFormData
  def apply(): GovernmentGatewayFormData =
    GovernmentGatewayFormData(
      redirectionUrl = "",
      credentialStrength = "",
      confidenceLevel = "",
      credId = "",
      nino = None,
      enrolments = List.empty,
      affinityGroup = "",
      credentialRole = "",
      usersName = None,
      email = None,
      gatewayToken = None,
      groupIdentifier = None,
      itmpData = None,
      agent = None
    )

}

trait UrlFormEncoder[A] {
  def encode(value: A): List[(String, String)]
}

object UrlFormEncoder {

  def apply[A](implicit encoder: UrlFormEncoder[A]): UrlFormEncoder[A] = encoder

  implicit val stringEncoder: UrlFormEncoder[String] = new UrlFormEncoder[String] {
    def encode(value: String): List[(String, String)] = List("" -> value)
  }

  implicit def listEncoder[A](implicit enc: UrlFormEncoder[A]): UrlFormEncoder[List[A]] = new UrlFormEncoder[List[A]] {
    def encode(values: List[A]): List[(String, String)] =
      values.zipWithIndex.flatMap { case (value, index) =>
        enc.encode(value).map { case (key, v) => s"[$index].$key" -> v }
      }
  }

  def withKey(key: String, list: List[(String, String)]): List[(String, String)] =
    list.map { case (k, v) =>
      (s"$key$k", v)
    }

}
