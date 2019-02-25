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

package uk.gov.hmrc.gform.graph.processor

import cats.Monad
import cats.syntax.applicative._
import uk.gov.hmrc.auth.core.Enrolments
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.graph.{ Convertible, NonConvertible, RecalculationOp }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil.affinityGroupNameO
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.language.higherKinds

class UserCtxEvaluatorProcessor[F[_]: Monad] {

  def processEvaluation(
    retrievals: MaterialisedRetrievals,
    userCtx: UserCtx,
    authConfig: AuthConfig
  ): Convertible[F] = {
    val result =
      (retrievals, userCtx) match {
        case (AnonymousRetrievals(_), _) => RecalculationOp.noChange
        case (AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _), UserCtx(EnrolledIdentifier)) =>
          getIdentifierValue(enrolments, authConfig)
        case (
            AuthenticatedRetrievals(_, enrolments, _, _, _, _, _, _),
            UserCtx(Enrolment(ServiceName(sn), IdentifierName(in)))) =>
          enrolments
            .getEnrolment(sn)
            .flatMap(_.getIdentifier(in))
            .map(a => RecalculationOp.newValue(a.value))
            .getOrElse(RecalculationOp.noChange)
        case (_, UserCtx(AffinityGroup)) =>
          RecalculationOp.newValue(affinityGroupNameO(AffinityGroupUtil.fromRetrievals(retrievals)))
      }
    NonConvertible(result.pure[F])
  }

  private def getIdentifierValue(enrolments: Enrolments, authConfig: AuthConfig) = authConfig match {
    case HmrcEnrolmentModule(auth)             => identifierValue(enrolments, auth)
    case HmrcAgentWithEnrolmentModule(_, auth) => identifierValue(enrolments, auth)
    case _                                     => RecalculationOp.noChange
  }

  private def identifierValue(enrolments: Enrolments, auth: EnrolmentAuth): RecalculationOp =
    enrolments
      .getEnrolment(auth.serviceId.value)
      .flatMap(_.identifiers.headOption)
      .map(a => RecalculationOp.newValue(a.value))
      .getOrElse(RecalculationOp.noChange)

}
