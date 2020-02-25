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

package uk.gov.hmrc.gform.graph

import cats.Monad
import cats.syntax.functor._
import cats.syntax.applicative._
import cats.syntax.traverse._
import cats.instances.list._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.gform.{ AuthContextPrepop, CustomerId }
import uk.gov.hmrc.gform.sharedmodel.{ SubmissionRef, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DestinationWithCustomerId, Destinations }
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

class CustomerIdRecalculation[F[_]: Monad](
  eeittId: (Eeitt, MaterialisedRetrievals, FormTemplate, HeaderCarrier) => F[String])(
  implicit ec: ExecutionContext
) {

  def evaluateCustomerId(cache: AuthCacheWithForm)(implicit hc: HeaderCarrier): F[CustomerId] =
    customerIdExpressions(cache.formTemplate.destinations)
      .traverse { cid =>
        recalculateCustomerId(cid, cache.retrievals, cache.formTemplate, cache.variadicFormData, cache.form.envelopeId)
      }
      .map(_.filter(!_.isEmpty).headOption.getOrElse(CustomerId.empty))

  private def customerIdExpressions(destinations: Destinations) = destinations match {
    case ds: Destinations.DestinationList =>
      ds.destinations.collect { case d: DestinationWithCustomerId => d.customerId }
  }

  private def recalculateCustomerId(
    expression: TextExpression,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    data: VariadicFormData,
    envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): F[CustomerId] =
    (expression.expr match {
      case AuthCtx(value) => AuthContextPrepop.values(value, retrievals).pure[F]

      case EeittCtx(eeitt) => eeittId(eeitt, retrievals, formTemplate, hc)

      case id: FormCtx => data.oneOrElse(id.toFieldId, "").pure[F]

      case SubmissionReference => SubmissionRef(envelopeId).toString.pure[F]

      case Constant(value) => value.pure[F]

      case _ => "".pure[F] //TODO change this to AuthExpr.
    }).map(customerId => CustomerId(customerId.take(64)))

}
