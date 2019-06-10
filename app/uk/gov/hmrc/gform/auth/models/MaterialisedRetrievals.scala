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

import uk.gov.hmrc.auth.core.retrieve.GGCredId
import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolments }
import uk.gov.hmrc.auth.core.retrieve.LegacyCredentials
import uk.gov.hmrc.gform.models.mappings._
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil._
import uk.gov.hmrc.http.logging.SessionId

sealed trait MaterialisedRetrievals extends Product with Serializable {
  def groupId = this match {
    case AnonymousRetrievals(sessionId)                            => sessionId.value
    case AuthenticatedRetrievals(_, _, _, _, _, userDetails, _, _) => userDetails.groupIdentifier
    case AWSALBRetrievals(username)                                => username
  }

  def ggCredId = this match {
    case AuthenticatedRetrievals(GGCredId(credId), _, _, _, _, _, _, _) => credId
    case _                                                              => ""
  }

  def renderSaveAndComeBackLater = this match {
    case AnonymousRetrievals(_) => false
    case _                      => true
  }

  def continueLabelKey = this match {
    case AnonymousRetrievals(_) => "button.continue"
    case _                      => "button.saveAndContinue"
  }

  def getTaxIdValue(taxIdName: ServiceNameAndTaxId) = this match {
    case AnonymousRetrievals(_) => ""
    case AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _) =>
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

  private def valueById(enrolments: Enrolments, id: String) =
    enrolments.enrolments.flatMap(_.identifiers).find(_.key.equalsIgnoreCase(id))

  private def valueByNameAndId(name: String, id: String, enrolments: Enrolments) =
    enrolments.getEnrolment(name).flatMap(_.getIdentifier(id))
}

case class AnonymousRetrievals(sessionId: SessionId) extends MaterialisedRetrievals
case class AuthenticatedRetrievals(
  authProviderId: LegacyCredentials,
  enrolments: Enrolments,
  affinityGroup: Option[AffinityGroup],
  internalId: Option[String],
  externalId: Option[String],
  userDetails: UserDetails,
  credentialStrength: Option[String],
  agentCode: Option[String]
) extends MaterialisedRetrievals {
  val affinityGroupName: String = affinityGroupNameO(affinityGroup)
}
case class AWSALBRetrievals(username: String) extends MaterialisedRetrievals

object IsAgent {
  def unapply(materialisedRetrievals: MaterialisedRetrievals): Boolean = materialisedRetrievals match {
    case AuthenticatedRetrievals(_, _, Some(AffinityGroup.Agent), _, _, _, _, _) => true
    case _                                                                       => false
  }
}
