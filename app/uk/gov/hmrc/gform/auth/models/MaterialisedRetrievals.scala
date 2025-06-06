/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.Eq
import cats.implicits._
import julienrf.json.derived
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.auth.core.retrieve.{ ItmpAddress, ItmpName }
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.models.mappings._
import uk.gov.hmrc.gform.models.userdetails.Nino
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ IdentifierName, ServiceName }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, AffinityGroupUtil }
import uk.gov.hmrc.http.SessionId

import java.security.MessageDigest
import java.time.LocalDate

sealed trait MaterialisedRetrievals extends Product with Serializable {

  def groupId = this match {
    case AnonymousRetrievals(sessionId) => sessionId.value
    case EmailRetrievals(EmailId(email)) =>
      "email-" + MessageDigest.getInstance("SHA-1").digest(email.toString.getBytes).mkString
    case AuthenticatedRetrievals(_, _, _, groupIdentifier, _, _, _, _) => groupIdentifier
    case VerifyRetrievals(verifyId, _)                                 => verifyId.id
  }

  def ggCredId = this match {
    case AuthenticatedRetrievals(GovernmentGatewayId(ggId), _, _, _, _, _, _, _) => ggId
    case _                                                                       => ""
  }

  def maybeGovermentGatewayId = this match {
    case AuthenticatedRetrievals(gg, _, _, _, _, _, _, _) => Some(gg)
    case _                                                => None
  }

  def maybeEmailId = this match {
    case EmailRetrievals(emailId) => Some(emailId)
    case _                        => None
  }

  def renderSaveAndComeBackLater = this match {
    case AnonymousRetrievals(_) => false
    case _                      => true
  }

  def continueLabelKey = this match {
    case AnonymousRetrievals(_) => "button.continue"
    case _                      => "button.saveAndContinue"
  }

  def getEmail = this match {
    case EmailRetrievals(EmailId(email))                               => email
    case AuthenticatedRetrievals(_, _, _, _, _, otherRetrievals, _, _) => otherRetrievals.email.getOrElse("")
    case _                                                             => ""
  }

  def getTaxIdValue(taxIdName: ServiceNameAndTaxId) = this match {
    case AnonymousRetrievals(_) => ""
    case EmailRetrievals(_)     => ""
    case VerifyRetrievals(_, Nino(nino)) =>
      taxIdName match {
        case NINO(_) => nino
        case _       => ""
      }
    case AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _) =>
      val maybeEnrolmentIdentifier = taxIdName match {
        case IRSA(name, id)         => valueByNameAndId(name, id, enrolments)
        case IRCT(name, id)         => valueByNameAndId(name, id, enrolments)
        case HMRCOBTDSORG(name, id) => valueByNameAndId(name, id, enrolments)
        case NINO(id)               => valueById(enrolments, id)
        case VATReg(id)             => valueById(enrolments, id)
        case VRN(name, id)          => valueByNameAndId(name, id, enrolments)
      }

      maybeEnrolmentIdentifier match {
        case Some(enrolmentId) => enrolmentId.value
        case None              => ""
      }
  }

  def getPayeRef = this match {
    case AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _) =>
      val taxOfficeNumber = getValueByNameAndId("IR-PAYE", "TaxOfficeNumber", enrolments)
      val taxOfficeReference = getValueByNameAndId("IR-PAYE", "TaxOfficeReference", enrolments)
      if (taxOfficeNumber.isEmpty && taxOfficeReference.isEmpty) {
        ""
      } else {
        s"${taxOfficeNumber.getOrElse("")}/${taxOfficeReference.getOrElse("")}"
      }
    case _ => ""
  }

  def enrolmentExists(serviceName: ServiceName, identifierName: IdentifierName, identifierValue: String): Boolean =
    this match {
      case AnonymousRetrievals(_) => false
      case EmailRetrievals(_)     => false
      case VerifyRetrievals(_, _) => false
      case AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _) =>
        val maybeEnrolment: Option[Enrolment] = enrolments.getEnrolment(serviceName.value)
        maybeEnrolment.fold(false) { enrolment =>
          val enrolmentIdentifier = EnrolmentIdentifier(identifierName.value, identifierValue)
          enrolment.identifiers.contains(enrolmentIdentifier)
        }
    }

  def getEnrolments: List[Enrolment] = this match {
    case a: AuthenticatedRetrievals => a.enrolments.enrolments.toList
    case _                          => Nil
  }

  def getAffinityGroup: Option[AffinityGroup] = this match {
    case a: AuthenticatedRetrievals => Some(a.affinityGroup)
    case _                          => None
  }

  def getCredentialRole: Option[CredentialRole] = this match {
    case a: AuthenticatedRetrievals => a.credentialRole
    case _                          => None
  }

  def maybeConfidenceLevel: Option[ConfidenceLevel] =
    this match {
      case r: AuthenticatedRetrievals => Some(r.confidenceLevel)
      case _                          => None
    }

  private def valueById(enrolments: Enrolments, id: String) =
    enrolments.enrolments.flatMap(_.identifiers).find(_.key.equalsIgnoreCase(id))

  private def getValueByNameAndId(name: String, id: String, enrolments: Enrolments) =
    enrolments.enrolments.filter(_.key === name).flatMap(_.getIdentifier(id)).map(_.value).headOption

  private def valueByNameAndId(name: String, id: String, enrolments: Enrolments) =
    enrolments.getEnrolment(name).flatMap(_.getIdentifier(id))

}

object MaterialisedRetrievals {
  implicit val format: OFormat[MaterialisedRetrievals] = derived.oformat()
}

final case class GovernmentGatewayId(ggId: String) extends AnyVal
object GovernmentGatewayId {
  implicit val format: OFormat[GovernmentGatewayId] = Json.format[GovernmentGatewayId]
}

final case class VerifyId(id: String) extends AnyVal
object VerifyId {
  implicit val format: OFormat[VerifyId] = Json.format[VerifyId]
}

final case class AnonymousRetrievals(sessionId: SessionId) extends MaterialisedRetrievals

object AnonymousRetrievals {
  implicit val sessionIdFormat: OFormat[SessionId] = Json.format[SessionId]
  implicit val format: OFormat[AnonymousRetrievals] = Json.format[AnonymousRetrievals]
}

final case class EmailRetrievals(emailId: EmailId) extends MaterialisedRetrievals

object EmailRetrievals {
  implicit val format: OFormat[EmailRetrievals] = Json.format[EmailRetrievals]
}

final case class VerifyRetrievals(verifyIde: VerifyId, nino: Nino) extends MaterialisedRetrievals

object VerifyRetrievals {
  implicit val format: OFormat[VerifyRetrievals] = Json.format[VerifyRetrievals]
}

final case class ItmpRetrievals(
  itmpName: Option[ItmpName],
  itmpDateOfBirth: Option[LocalDate],
  itmpAddress: Option[ItmpAddress]
)
object ItmpRetrievals {
  implicit val itmpNameFormat: OFormat[ItmpName] = Json.format[ItmpName]
  implicit val itmpAddressFormat: OFormat[ItmpAddress] = Json.format[ItmpAddress]
  implicit val format: OFormat[ItmpRetrievals] = Json.format[ItmpRetrievals]
  implicit val equal: Eq[ItmpRetrievals] = Eq.fromUniversalEquals
}

final case class OtherRetrievals(
  email: Option[String]
)

object OtherRetrievals {
  val empty = OtherRetrievals(None)

  implicit val format: OFormat[OtherRetrievals] = Json.format[OtherRetrievals]
}

final case class AuthenticatedRetrievals(
  governmentGatewayId: GovernmentGatewayId,
  enrolments: Enrolments,
  affinityGroup: AffinityGroup,
  groupIdentifier: String,
  maybeNino: Option[Nino],
  otherRetrievals: OtherRetrievals,
  confidenceLevel: ConfidenceLevel,
  credentialRole: Option[CredentialRole]
) extends MaterialisedRetrievals {
  val affinityGroupName: String = AffinityGroupUtil.affinityGroupName(affinityGroup)
}

object AuthenticatedRetrievals {
  implicit val enrolmentsFormat: OFormat[Enrolments] = Json.format[Enrolments]
  implicit val format: OFormat[AuthenticatedRetrievals] = Json.format[AuthenticatedRetrievals]
}

object IsAgent {
  def unapply(materialisedRetrievals: MaterialisedRetrievals): Boolean = materialisedRetrievals match {
    case a: AuthenticatedRetrievals => a.affinityGroup == AffinityGroup.Agent
    case _                          => false
  }
}
