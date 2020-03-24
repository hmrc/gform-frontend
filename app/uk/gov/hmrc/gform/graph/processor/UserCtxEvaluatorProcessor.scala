/*
 * Copyright 2020 HM Revenue & Customs
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
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.graph.{ Convertible, NonConvertible, RecalculationOp }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil
import uk.gov.hmrc.gform.sharedmodel.AffinityGroupUtil.affinityGroupNameO
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.language.higherKinds

class UserCtxEvaluatorProcessor[F[_]: Monad] extends IdentifierExtractor {

  def processEvaluation(
    retrievals: MaterialisedRetrievals,
    userCtx: UserCtx,
    authConfig: AuthConfig
  ): Convertible[F] = {
    val result =
      (retrievals, userCtx) match {
        case (AnonymousRetrievals(_), _) => RecalculationOp.noChange
        case (AuthenticatedRetrievals(_, enrolments, _, _), UserCtx(EnrolledIdentifier)) =>
          RecalculationOp.newValue(authorizedEnrolmentValue(enrolments, authConfig))
        case (AuthenticatedRetrievals(_, enrolments, _, _), UserCtx(Enrolment(sn, in))) =>
          RecalculationOp.newValue(extractIdentifier(enrolments, sn, in))
        case (_, UserCtx(AffinityGroup)) =>
          RecalculationOp.newValue(affinityGroupNameO(AffinityGroupUtil.fromRetrievals(retrievals)))
      }
    NonConvertible(result.pure[F])
  }
}
