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

package uk.gov.hmrc.gform.graph.processor

import uk.gov.hmrc.auth.core.Enrolment
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, EmailRetrievals, MaterialisedRetrievals, VerifyRetrievals }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil.affinityGroupNameO
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object UserCtxEvaluatorProcessor extends IdentifierExtractor {

  def processEvaluation(
    retrievals: MaterialisedRetrievals,
    userField: UserField,
    authConfig: AuthConfig
  ): String =
    (retrievals, userField) match {
      case (AuthenticatedRetrievals(_, enrolments, _, _, _), UserField.EnrolledIdentifier) =>
        authorizedEnrolmentValue(enrolments, authConfig)
      case (AuthenticatedRetrievals(_, enrolments, _, _, _), UserField.Enrolment(sn, in)) =>
        extractIdentifier(enrolments, sn, in)
      case (_, UserField.AffinityGroup) =>
        affinityGroupNameO(AffinityGroupUtil.fromRetrievals(retrievals))
      case (AnonymousRetrievals(_), _) => ""
      case (EmailRetrievals(_), _)     => ""
      case (VerifyRetrievals(_, _), _) => ""
    }

  def maybeEnrolment(retrievals: MaterialisedRetrievals, enrolment: UserField.Enrolment): Option[Enrolment] =
    retrievals match {
      case AuthenticatedRetrievals(_, enrolments, _, _, _) =>
        enrolments.enrolments.find(e =>
          ServiceName(e.key) == enrolment.serviceName && e.identifiers.exists(k =>
            IdentifierName(k.key) == enrolment.identifierName
          )
        )
      case _ => None
    }

  def evalRosm(thirdPartyData: ThirdPartyData, rosmProp: RosmProp): String = {
    val f = thirdPartyData.desRegistrationResponse.fold("") _
    rosmProp match {
      case RosmSafeId           => f(_.safeId)
      case RosmOrganisationName => f(_.orgOrInd.getOrganisationName)
      case RosmOrganisationType => f(_.orgOrInd.getOrganisationType)
      case RosmIsAGroup         => f(_.orgOrInd.getIsAGroup)
    }
  }

}
