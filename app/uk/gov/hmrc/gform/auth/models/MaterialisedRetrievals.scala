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

package uk.gov.hmrc.gform.auth.models

import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.models.mappings._
import uk.gov.hmrc.gform.models.userdetails.Nino
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ IdentifierName, ServiceName }
import java.security.MessageDigest
import uk.gov.hmrc.http.SessionId

sealed trait MaterialisedRetrievals extends Product with Serializable {
  def groupId = this match {
    case AnonymousRetrievals(sessionId) => sessionId.value
    case EmailRetrievals(EmailId(email)) =>
      "email-" + MessageDigest.getInstance("SHA-1").digest(email.toString.getBytes).mkString
    case AuthenticatedRetrievals(_, _, _, groupIdentifier, _) => groupIdentifier
    case VerifyRetrievals(verifyId, _)                        => verifyId.id
  }

  def ggCredId = this match {
    case AuthenticatedRetrievals(GovernmentGatewayId(ggId), _, _, _, _) => ggId
    case _                                                              => ""
  }

  def maybeGovermentGatewayId = this match {
    case AuthenticatedRetrievals(gg, _, _, _, _) => Some(gg)
    case _                                       => None
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
    case EmailRetrievals(EmailId(email)) => email
    case _                               => ""
  }

  def getTaxIdValue(taxIdName: ServiceNameAndTaxId) = this match {
    case AnonymousRetrievals(_) => ""
    case EmailRetrievals(_)     => ""
    case VerifyRetrievals(_, Nino(nino)) =>
      taxIdName match {
        case NINO(_) => nino
        case _       => ""
      }
    case AuthenticatedRetrievals(_, enrolments, _, _, _) =>
      val maybeEnrolmentIdentifier = taxIdName match {
        case IRSA(name, id)         => valueByNameAndId(name, id, enrolments)
        case IRCT(name, id)         => valueByNameAndId(name, id, enrolments)
        case HMRCOBTDSORG(name, id) => valueByNameAndId(name, id, enrolments)
        case NINO(id)               => valueById(enrolments, id)
        case VATReg(id)             => valueById(enrolments, id)
      }

      maybeEnrolmentIdentifier match {
        case Some(enrolmentId) => enrolmentId.value
        case None              => ""
      }
  }

  def enrolmentExists(serviceName: ServiceName, identifierName: IdentifierName, identifierValue: String): Boolean =
    this match {
      case AnonymousRetrievals(_) => false
      case EmailRetrievals(_)     => false
      case VerifyRetrievals(_, _) => false
      case AuthenticatedRetrievals(_, enrolments, _, _, _) =>
        val maybeEnrolment: Option[Enrolment] = enrolments.getEnrolment(serviceName.value)
        maybeEnrolment.fold(false) { enrolment =>
          val enrolmentIdentifier = EnrolmentIdentifier(identifierName.value, identifierValue)
          enrolment.identifiers.contains(enrolmentIdentifier)
        }
    }

  private def valueById(enrolments: Enrolments, id: String) =
    enrolments.enrolments.flatMap(_.identifiers).find(_.key.equalsIgnoreCase(id))

  private def valueByNameAndId(name: String, id: String, enrolments: Enrolments) =
    enrolments.getEnrolment(name).flatMap(_.getIdentifier(id))
}

case class GovernmentGatewayId(ggId: String) extends AnyVal
case class VerifyId(id: String) extends AnyVal

case class AnonymousRetrievals(sessionId: SessionId) extends MaterialisedRetrievals

case class EmailRetrievals(emailId: EmailId) extends MaterialisedRetrievals

case class VerifyRetrievals(verifyIde: VerifyId, nino: Nino) extends MaterialisedRetrievals

case class AuthenticatedRetrievals(
  governmentGatewayId: GovernmentGatewayId,
  enrolments: Enrolments,
  affinityGroup: AffinityGroup,
  groupIdentifier: String,
  maybeNino: Option[Nino]
) extends MaterialisedRetrievals {
  val affinityGroupName: String = AffinityGroupUtil.affinityGroupName(affinityGroup)
}

object IsAgent {
  def unapply(materialisedRetrievals: MaterialisedRetrievals): Boolean = materialisedRetrievals match {
    case a: AuthenticatedRetrievals => a.affinityGroup == AffinityGroup.Agent
    case _                          => false
  }
}
