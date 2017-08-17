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

package uk.gov.hmrc.gform.auth.models

import uk.gov.hmrc.auth.core.authorise.{ AffinityGroup, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.auth.core.retrieve.LegacyCredentials

case class Retrievals(
  authProviderId: LegacyCredentials,
  enrolments: Enrolments,
  affinityGroup: Option[AffinityGroup],
  internalId: Option[String],
  externalId: Option[String],
  userDetails: UserDetails,
  credentialStrength: Option[String],
  agentCode: Option[String]
)

object Retrievals {
  def getTaxIdValue(maybeEnrolment: Option[String], taxIdName: String, retrievals: Retrievals) = {

    val maybeEnrolmentIdentifier = maybeEnrolment match {
      case Some(enrolment) => retrievals.enrolments.getEnrolment(enrolment).flatMap(_.getIdentifier(taxIdName))
      case None => retrievals.enrolments.enrolments.flatMap(_.identifiers).find(_.key.equalsIgnoreCase(taxIdName))
    }

    maybeEnrolmentIdentifier match {
      case Some(enrolmentId) => enrolmentId.value
      case None => ""
    }
  }
}