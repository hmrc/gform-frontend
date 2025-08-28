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

package uk.gov.hmrc.gform.sharedmodel.graph

import cats.syntax.all._
import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest
import uk.gov.hmrc.gform.auth.models.{ IdentifierValue, MaterialisedRetrievals }
import uk.gov.hmrc.gform.eval.{ AllFormTemplateExpressions, DbLookupChecker, DelegatedEnrolmentChecker, SeissEligibilityChecker }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, FormCtx, FormTemplate, In }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.mutable
import scala.concurrent.{ ExecutionContext, Future }

case class GraphDataCache(
  graph: Graph[GraphNode, DiEdge[GraphNode]],
  inExprResolver: In => Boolean,
  booleanExprCache: BooleanExprCache
)

object GraphDataCache {
  def empty: GraphDataCache =
    GraphDataCache(Graph.empty, _ => false, BooleanExprCache.empty)
}

class GraphDataCacheService(
  seissEligibilityChecker: SeissEligibilityChecker,
  delegatedEnrolmentCheckStatus: DelegatedEnrolmentChecker,
  dbLookupCheckStatus: DbLookupChecker
) {
  def get[U <: SectionSelectorType: SectionSelector](
    retrievals: MaterialisedRetrievals,
    variadicFormData: VariadicFormData[SourceOrigin.OutOfDate],
    formTemplate: FormTemplate,
    fmb: FormModelBuilder,
    booleanExprCache: BooleanExprCache
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[GraphDataCache] = {

    val formModelInterim: FormModel[Interim] = fmb.expand(variadicFormData)
    val (graph, inExprs) = DependencyGraph.toGraph(formModelInterim, AllFormTemplateExpressions(formTemplate))
    val changedCacheValues = mutable.Map[(DataSource, String), Boolean]()

    val inExprResolverFtr = {
      val setOfFutures = inExprs.collect { case In(formCtx: FormCtx, dataSource) =>
        val modelComponentId = formCtx.formComponentId.baseComponentId
        def makeCall(value: String): Future[Boolean] = dataSource match {
          case DataSource.Mongo(collectionName) => dbLookupCheckStatus(value, collectionName, hc)
          case DataSource.Enrolment(serviceName, identifierName) =>
            retrievals.enrolmentExists(serviceName, identifierName, value).pure[Future]
          case dd @ DataSource.DelegatedEnrolment(_, _) =>
            retrievals.maybeGovermentGatewayId.fold(false.pure[Future]) { governmentGatewayId =>
              delegatedEnrolmentCheckStatus(governmentGatewayId, dd, IdentifierValue(value), hc)
            }
          case DataSource.SeissEligible =>
            seissEligibilityChecker(UtrEligibilityRequest(value), hc)
        }

        variadicFormData.forBaseComponentId(modelComponentId).map { case (modelComponentId, variadicFormValue) =>
          val value = variadicFormValue.fold(one => one.value)(many =>
            throw new RuntimeException(s"Didn't expect many data points in data $many")
          )

          val result = booleanExprCache.get(dataSource, value).map(Future.successful).getOrElse {

            makeCall(value).map { result =>
              changedCacheValues.addOne((dataSource -> value, result))
              result
            }
          }

          val newIn = In(new FormCtx(modelComponentId.toFormComponentId), dataSource)
          result.map(newIn -> _)
        }
      }.flatten

      Future
        .sequence(setOfFutures)
        .map(_.toMap)
        .map { map => (in: In) =>
          map.getOrElse(in, false)
        }
    }

    inExprResolverFtr.map { inExprResolver =>
      val finalBooleanCache = if (changedCacheValues.nonEmpty) {
        val newBooleanExprCache = booleanExprCache.mapping
          .foldLeft(mutable.Map.newBuilder[DataSource, mutable.Map[String, Boolean]]) { case (builder, (key, value)) =>
            builder.addOne(
              (key, mutable.Map.newBuilder.addAll(value).result())
            )
          }
          .result()

        changedCacheValues.foreach { case ((dataSource, name), value) =>
          val subMap = newBooleanExprCache.getOrElseUpdate(dataSource, mutable.Map())
          subMap.addOne((name, value))
        }

        BooleanExprCache(
          newBooleanExprCache
            .foldLeft(Map.newBuilder[DataSource, Map[String, Boolean]]) { case (acc, (key, value)) =>
              acc.addOne((key, Map.newBuilder.addAll(value).result()))
            }
            .result()
        )

      } else {
        booleanExprCache
      }

      println(finalBooleanCache)

      GraphDataCache(graph, inExprResolver, finalBooleanCache)
    }
  }
}
