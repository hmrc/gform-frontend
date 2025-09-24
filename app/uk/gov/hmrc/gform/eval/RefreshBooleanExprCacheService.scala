/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import cats.Monad
import cats.syntax.all._
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest
import uk.gov.hmrc.gform.auth.models.{ IdentifierValue, MaterialisedRetrievals }
import uk.gov.hmrc.gform.eval.{ DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, FormCtx, In }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.http.HeaderCarrier

class RefreshBooleanExprCacheService[F[_]: Monad](
  seissEligibilityChecker: SeissEligibilityChecker[F],
  delegatedEnrolmentCheckStatus: DelegatedEnrolmentChecker[F],
  dbLookupCheckStatus: DbLookupChecker[F]
) {

  def makeCall(retrievals: MaterialisedRetrievals, value: String, dataSource: DataSource)(implicit
    hc: HeaderCarrier
  ): F[Boolean] = dataSource match {
    case DataSource.Mongo(collectionName) => dbLookupCheckStatus(value, collectionName, hc)
    case DataSource.Enrolment(serviceName, identifierName) =>
      retrievals.enrolmentExists(serviceName, identifierName, value).pure[F]
    case dd @ DataSource.DelegatedEnrolment(_, _) =>
      retrievals.maybeGovermentGatewayId.fold(false.pure[F]) { governmentGatewayId =>
        delegatedEnrolmentCheckStatus(governmentGatewayId, dd, IdentifierValue(value), hc)
      }
    case DataSource.SeissEligible =>
      seissEligibilityChecker(UtrEligibilityRequest(value), hc)
  }

  def refreshBooleanExprCache(
    retrievals: MaterialisedRetrievals,
    variadicFormData: VariadicFormData[SourceOrigin.OutOfDate],
    formModelOptics: FormModelOptics[DataOrigin.Mongo],
    booleanExprCache: BooleanExprCache
  )(implicit hc: HeaderCarrier): F[BooleanExprCache] = {

    val formModel = formModelOptics.formModelRenderPageOptics.formModel

    val ins: List[In] =
      ((formModel.allIncludeIfsWithDependingFormComponents ++
        formModel.allComponentIncludeIfs)
        .map(_._1)
        .map(_.booleanExpr) ++ formModel.allValidIfs
        .flatMap(_._1)
        .map(_.booleanExpr))
        .collect { case in: In =>
          in
        }

    val insEvaluated: F[List[(DataSource, String, Boolean)]] = ins.flatTraverse {
      case In(FormCtx(formComponentId), dataSource) =>
        val value = variadicFormData.one(formComponentId.modelComponentId)
        (value match {
          case None => Option.empty[(DataSource, String, Boolean)].pure[F]
          case Some(value) =>
            booleanExprCache.get(dataSource, value) match {
              case None              => makeCall(retrievals, value, dataSource).map(b => Some((dataSource, value, b)))
              case Some(cachedValue) => Option.empty[(DataSource, String, Boolean)].pure[F]
            }
        }).map(_.toList)

      case _ => List.empty[(DataSource, String, Boolean)].pure[F]
    }

    insEvaluated.map { xs =>
      if (xs.isEmpty) {
        booleanExprCache
      } else {
        xs.foldLeft(booleanExprCache) { case (acc, (dataSource, value, result)) =>
          acc.add(dataSource, value, result)
        }
      }
    }
  }
}

object RefreshBooleanExprCacheService {
  def evalInExpr[S <: SourceOrigin](in: In, booleanExprCache: BooleanExprCache, recData: RecData[S]): Boolean =
    in match {
      case In(FormCtx(fcId), dataSource) =>
        recData.variadicFormData.one(fcId.modelComponentId).fold(false) { value =>
          booleanExprCache.get(dataSource, value).getOrElse(false)
        }
      case _ => false
    }
}
