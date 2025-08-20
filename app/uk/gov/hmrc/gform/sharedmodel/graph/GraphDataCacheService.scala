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
import uk.gov.hmrc.gform.models.{ FormModel, FormModelBuilder, Interim, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, FormTemplate, In }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class GraphDataCache(graph: Graph[GraphNode, DiEdge[GraphNode]], inExprResolver: In => Boolean)

class GraphDataCacheService(
  seissEligibilityChecker: SeissEligibilityChecker,
  delegatedEnrolmentCheckStatus: DelegatedEnrolmentChecker,
  dbLookupCheckStatus: DbLookupChecker
) {
  def get[U <: SectionSelectorType: SectionSelector](
    retrievals: MaterialisedRetrievals,
    variadicFormData: VariadicFormData[SourceOrigin.OutOfDate],
    formTemplate: FormTemplate,
    fmb: FormModelBuilder
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[GraphDataCache] = {

    val formModelInterim: FormModel[Interim] = fmb.expand(variadicFormData)
    val (graph, inExprs) = DependencyGraph.toGraph(formModelInterim, AllFormTemplateExpressions(formTemplate))

    val inExprResolverFtr = {
      val setOfFutures = inExprs.map { case in @ In(expr, dataSource) =>
        println(expr)
        val value =
          ???

        def makeCall(): Future[Boolean] = dataSource match {
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

        makeCall().map(in -> _)
      }

      Future
        .sequence(setOfFutures)
        .map(_.toMap)
        .map { map => (in: In) =>
          map.getOrElse(in, throw new RuntimeException("In expression cannot be evaluated, request is missing"))
        }
    }

    inExprResolverFtr.map {
      GraphDataCache(graph, _)
    }
  }
}
