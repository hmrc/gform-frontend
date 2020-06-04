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

package uk.gov.hmrc.gform.models

import cats.MonadError
import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.flatMap._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.models.gform.ObligationsAction
import uk.gov.hmrc.gform.models.gform.ForceReload
import uk.gov.hmrc.gform.sharedmodel._

trait TaxPeriodStateChecker[F[_], E] {

  def error: E

  def callDesIfNeeded(
    getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => F[NonEmptyList[ServiceCallResponse[TaxResponse]]],
    evaluatedTaxPeriod: Option[NonEmptyList[HmrcTaxPeriodWithEvaluatedId]],
    obligations: Obligations,
    newState: Map[RecalculatedTaxPeriodKey, IdNumberValue],
    oldState: Map[RecalculatedTaxPeriodKey, IdNumberValue],
    obligationsAction: ObligationsAction
  )(
    implicit
    me: MonadError[F, E]
  ): F[Obligations] =
    (
      evaluatedTaxPeriod,
      needRefresh(newState, oldState) || obligations.isNotChecked || forceObligationReload(obligationsAction)) match {
      case (None, _)        => obligations.pure[F]
      case (Some(_), false) => obligations.pure[F]
      case (Some(hmrcTaxPeriodWithEvaluatedIds), true) =>
        getAllTaxPeriods(hmrcTaxPeriodWithEvaluatedIds).flatMap { nel =>
          nel.sequence match {
            case CannotRetrieveResponse => me.raiseError[Obligations](error)
            case NotFound               => me.raiseError[Obligations](error) // NotFound case is handled on backend.
            case ServiceResponse(a)     => (RetrievedObligations(a): Obligations).pure[F]
          }
        }
    }

  val needRefresh
    : (Map[RecalculatedTaxPeriodKey, IdNumberValue], Map[RecalculatedTaxPeriodKey, IdNumberValue]) => Boolean =
    (newState, oldState) => (oldState.isEmpty && newState.nonEmpty) || hasDifferentValue(newState, oldState)

  private val hasDifferentValue
    : (Map[RecalculatedTaxPeriodKey, IdNumberValue], Map[RecalculatedTaxPeriodKey, IdNumberValue]) => Boolean =
    (newState, oldState) =>
      newState
        .exists {
          case (hmrcTaxPeriod, idNumberValue) =>
            oldState.get(hmrcTaxPeriod).fold(true)(_ =!= idNumberValue)
      }

  private val forceObligationReload: ObligationsAction => Boolean = {
    case ForceReload => true
    case _           => false
  }
}
