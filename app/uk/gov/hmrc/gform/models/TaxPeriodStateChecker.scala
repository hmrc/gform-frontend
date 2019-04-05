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

package uk.gov.hmrc.gform.models

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative._
import cats.syntax.eq._
import cats.syntax.functor._
import uk.gov.hmrc.gform.models.gform.UserType
import uk.gov.hmrc.gform.models.gform.ReturningUser
import uk.gov.hmrc.gform.sharedmodel._

class TaxPeriodStateChecker[F[_]: Monad] {

  def callDesIfNeeded(
    getAllTaxPeriods: NonEmptyList[HmrcTaxPeriodWithEvaluatedId] => F[NonEmptyList[TaxResponse]],
    evaluatedTaxPeriod: Option[NonEmptyList[HmrcTaxPeriodWithEvaluatedId]],
    obligations: Obligations,
    newState: Map[RecalculatedTaxPeriodKey, IdNumberValue],
    oldState: Map[RecalculatedTaxPeriodKey, IdNumberValue],
    userType: UserType
  ): F[Obligations] =
    (evaluatedTaxPeriod, needRefresh(newState, oldState) || obligations.isNotChecked || isAReturningUser(userType)) match {
      case (None, _)        => obligations.pure[F]
      case (Some(_), false) => obligations.pure[F]
      case (Some(hmrcTaxPeriodWithEvaluatedIds), true) =>
        getAllTaxPeriods(hmrcTaxPeriodWithEvaluatedIds).map(RetrievedObligations.apply)
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

  private val isAReturningUser: UserType => Boolean = {
    case ReturningUser => true
    case _             => false
  }
}
