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

package uk.gov.hmrc.gform.sharedmodel.graph

import cats.instances.either._
import cats.syntax.functor._
import scalax.collection.Graph
import scalax.collection.GraphPredef._, scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object DependencyGraph {

  val emptyGraph: Graph[GraphNode, DiEdge] = Graph.empty

  def toGraph(formTemplate: FormTemplate, data: Data): Graph[GraphNode, DiEdge] =
    graphFrom(formTemplate.expandFormTemplate(data))

  def toGraphFull(formTemplate: FormTemplate): Graph[GraphNode, DiEdge] =
    graphFrom(formTemplate.expandFormTemplateFull)

  private def graphFrom(expandedFT: ExpandedFormTemplate): Graph[GraphNode, DiEdge] = {

    val allFcIds = expandedFT.allFormComponentIds

    def edges(fc: FormComponent): List[DiEdge[GraphNode]] = {
      def fcIds(fc: FormComponent): List[FormComponentId] = fc match {
        case HasExpr(
            SingleExpr(AuthCtx(_) | EeittCtx(_) | UserCtx(_) | SubmissionReference | HmrcRosmRegistrationCheck(_))) =>
          fc.id :: Nil
        case HasExpr(SingleExpr(expr)) => eval(expr)
        case _                         => List.empty
      }
      fcIds(fc).map(fcId => SimpleGN(fc.id) ~> SimpleGN(fcId))
    }

    def eval(expr: Expr): List[FormComponentId] =
      expr match {
        case fc @ FormCtx(_)             => fc.toFieldId :: Nil
        case Sum(FormCtx(fc))            => allFcIds.filter(_.value.endsWith(fc))
        case Add(field1, field2)         => eval(field1) ++ eval(field2)
        case Subtraction(field1, field2) => eval(field1) ++ eval(field2)
        case Multiply(field1, field2)    => eval(field1) ++ eval(field2)
        case Else(field1, field2)        => eval(field1) ++ eval(field2)
        case otherwise                   => List.empty
      }

    def evalBooleanExpr(expr: BooleanExpr): List[FormComponentId] =
      expr match {
        case Equals(left, right)              => eval(left) ++ eval(right)
        case NotEquals(left, right)           => eval(left) ++ eval(right)
        case GreaterThan(left, right)         => eval(left) ++ eval(right)
        case GreaterThanOrEquals(left, right) => eval(left) ++ eval(right)
        case LessThan(left, right)            => eval(left) ++ eval(right)
        case LessThanOrEquals(left, right)    => eval(left) ++ eval(right)
        case Not(bExpr)                       => evalBooleanExpr(bExpr)
        case Or(left, right)                  => evalBooleanExpr(left) ++ evalBooleanExpr(right)
        case And(left, right)                 => evalBooleanExpr(left) ++ evalBooleanExpr(right)
        case otherwise                        => List.empty
      }

    val includeIfs: List[DiEdge[GraphNode]] = expandedFT.allIncludeIfs.flatMap {
      case (expandedFCs, includeIf, index) =>
        val includeIfFcId = FormComponentId("includeIf_" + index)

        val iign = IncludeIfGN(includeIfFcId, includeIf)
        val deps = evalBooleanExpr(includeIf.expr)

        expandedFCs.flatMap(_.allIds).map(a => SimpleGN(a) ~> iign) ++
          deps.map(a => iign ~> SimpleGN(a))

    }

    expandedFT.allFormComponents.flatMap(edges).foldLeft(emptyGraph)(_ + _) ++ includeIfs
  }

  def constructDependencyGraph(
    graph: Graph[GraphNode, DiEdge]): Either[graph.NodeT, Traversable[(Int, List[GraphNode])]] = {
    def sortedOuterNodes(items: Iterable[graph.NodeT]) =
      items.toList
        .map(_.toOuter)
        .sortBy(_.formComponentId.value)

    graph.topologicalSort
      .map(_.toLayered.map {
        case (index, items) => (index, sortedOuterNodes(items))
      })
  }
}
