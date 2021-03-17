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

package uk.gov.hmrc.gform

import cats.Monad
import cats.syntax.applicative._

import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.{ GovernmentGatewayId, IdentifierValue }
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.sharedmodel.dblookup.CollectionName
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.DelegatedEnrolment
import uk.gov.hmrc.http.HeaderCarrier

trait GraphSpec {

  def dbLookupStatusTrue[F[_]: Monad](id: String, collectionName: CollectionName, hc: HeaderCarrier): F[Boolean] =
    true.pure[F]

  def delegatedEnrolmentCheckStatusTrue[F[_]: Monad](
    governmentGatewayId: GovernmentGatewayId,
    delegatedEnrolment: DelegatedEnrolment,
    identifierValue: IdentifierValue,
    hc: HeaderCarrier
  ): F[Boolean] = true.pure[F]

  def booleanExprEval[F[_]: Monad]: BooleanExprEval[F] = new BooleanExprEval[F]()

}
