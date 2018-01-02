/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auth

import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class AuthService {

  def eeitReferenceNumber(retrievals: Retrievals): String = retrievals.userDetails.affinityGroup match {
    case AffinityGroup.Agent => retrievals.enrolments
      .getEnrolment(AuthConfig.eeittAuth)
      .fold("")(_.getIdentifier(EEITTAuthConfig.agentIdName).fold("")(_.value))
    case _ => retrievals.enrolments
      .getEnrolment(AuthConfig.eeittAuth)
      .fold("")(_.getIdentifier(EEITTAuthConfig.nonAgentIdName).fold("")(_.value))
  }

  def evaluateSubmissionReference(expression: TextExpression, retrievals: Retrievals): String = {

    expression.expr match {
      case AuthCtx(value) =>
        val authContextPrepop = new AuthContextPrepop()
        authContextPrepop.values(value, retrievals)

      case EeittCtx(eeitt) => eeitReferenceNumber(retrievals)
      case _ => "" //TODO change this to AuthExpr.
    }
  }
}
