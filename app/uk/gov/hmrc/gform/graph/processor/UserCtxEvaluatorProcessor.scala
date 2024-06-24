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

package uk.gov.hmrc.gform.graph.processor

import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil.affinityGroupNameO
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object UserCtxEvaluatorProcessor extends IdentifierExtractor {

  private val logger = LoggerFactory.getLogger(getClass)

  def processEvaluation(
    retrievals: MaterialisedRetrievals,
    userField: UserField,
    authConfig: AuthConfig
  ): String =
    (retrievals, userField) match {
      case (AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _), UserField.EnrolledIdentifier) =>
        authorizedEnrolmentValue(enrolments, authConfig)
      case (
            AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _),
            UserField.Enrolment(sn, in, maybeUserFieldFunc)
          ) =>
        enrolments.enrolments
          .find { e =>
            ServiceName(e.key) == sn && e.identifiers.exists(k => IdentifierName(k.key) == in)
          }
          .map { enrolment =>
            maybeUserFieldFunc match {
              case Some(UserFieldFunc.Count) =>
                enrolment.identifiers.count(i => IdentifierName(i.key) == in).toString
              case Some(UserFieldFunc.Index(i)) =>
                maybeValue(
                  enrolment.identifiers
                    .filter(i => IdentifierName(i.key) == in)
                    .lift(i - 1)
                )
              case None => maybeValue(enrolment.getIdentifier(in.value))
            }
          }
          .getOrElse("")
      case (_, UserField.AffinityGroup) =>
        affinityGroupNameO(AffinityGroupUtil.fromRetrievals(retrievals))
      case (_, UserField.CredentialRole) =>
        val credentialRole: String =
          retrievals.getCredentialRole.fold("")(credentialRole => credentialRole.toString.toLowerCase)
        logger.info(
          s"User credential role has been retrieved as: ${if (credentialRole == "") "<empty string>" else credentialRole}"
        )
        credentialRole
      case (AnonymousRetrievals(_), _) => ""
      case (EmailRetrievals(_), _)     => ""
      case (VerifyRetrievals(_, _), _) => ""
    }
}
