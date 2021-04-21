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

package uk.gov.hmrc.gform.sharedmodel.graph

import cats.syntax.eq._
import cats.syntax.option._
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import uk.gov.hmrc.gform.eval.{ AllFormComponentExpressions, ExprMetadata, IsSelfReferring, SelfReferenceProjection, StandaloneSumInfo, SumInfo }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, PageMode }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object DependencyGraph {

  val emptyGraph: Graph[GraphNode, DiEdge] = Graph.empty

  def toGraph(formModel: FormModel[Interim], formTemplateExprs: List[ExprMetadata]): Graph[GraphNode, DiEdge] =
    graphFrom(formModel, formTemplateExprs)

  private def graphFrom[T <: PageMode](
    formModel: FormModel[T],
    formTemplateExprs: List[ExprMetadata]
  ): Graph[GraphNode, DiEdge] = {

    val isSum = new IsOneOfSum(formModel.sumInfo)
    val isStandaloneSum = new IsOneOfStandaloneSum(formModel.standaloneSumInfo)

    def edges(fc: FormComponent): List[DiEdge[GraphNode]] = {

      def fcIds(fc: FormComponent): List[DiEdge[GraphNode]] = fc match {
        case AllFormComponentExpressions(exprsMetadata) =>
          exprsMetadata.flatMap {
            case SelfReferenceProjection(
                  IsSelfReferring.No(AuthCtx(_) | UserCtx(_) | FormTemplateCtx(_) | HmrcRosmRegistrationCheck(_))
                ) =>
              (fc.id :: Nil).map(fcId => GraphNode.Simple(fc.id) ~> GraphNode.Simple(fcId))
            case SelfReferenceProjection(IsSelfReferring.No(expr)) =>
              toDiEdge(fc, expr, _ => false)
            case SelfReferenceProjection(IsSelfReferring.Yes(expr, selfReference)) =>
              toDiEdge(fc, expr, _ === selfReference)
          }
        case isSum.IsSum(values) =>
          values.toList.flatMap { value =>
            GraphNode.Expr(FormCtx(fc.id)) ~> GraphNode.Simple(fc.id) ::
              GraphNode.Simple(value) ~> GraphNode.Expr(FormCtx(fc.id)) :: Nil
          }
        case isStandaloneSum.IsSum(fcId) =>
          List(GraphNode.Expr(FormCtx(fcId)) ~> GraphNode.Simple(fcId))
        case _ => Nil
      }
      fcIds(fc)
    }

    def toFormComponentId(expr: Expr): List[FormComponentId] =
      expr match {
        case FormCtx(formComponentId) => formComponentId :: Nil
        case otherwise                => List.empty
      }

    def toDiEdge(fc: FormComponent, expr: Expr, cycleBreaker: FormComponentId => Boolean): List[DiEdge[GraphNode]] =
      expr.leafs
        .flatMap { e =>
          val fcNodes = toFormComponentId(e).map(fcId => GraphNode.Expr(e) ~> GraphNode.Simple(fcId))
          if (cycleBreaker(fc.id) && e === FormCtx(fc.id)) fcNodes
          else GraphNode.Simple(fc.id) ~> GraphNode.Expr(e) :: fcNodes
        }

    def boolenExprDeps(
      booleanExpr: BooleanExpr,
      dependingFCs: List[FormComponent],
      cycleBreaker: GraphNode.Expr => Boolean
    ): List[DiEdge[GraphNode]] = {

      val allExprGNs: List[GraphNode.Expr] =
        booleanExpr.allExpressions.flatMap(_.leafs).map(GraphNode.Expr.apply)

      val dependingFCIds = dependingFCs.map(_.id)

      val deps1: List[DiEdge[GraphNode]] =
        for {
          exprGN        <- allExprGNs.filterNot(cycleBreaker)
          dependingFCId <- dependingFCIds
        } yield GraphNode.Simple(dependingFCId) ~> exprGN

      val deps2: List[DiEdge[GraphNode]] =
        allExprGNs.flatMap(exprGN => toFormComponentId(exprGN.expr).map(fcId => exprGN ~> GraphNode.Simple(fcId)))

      deps1 ++ deps2
    }

    val validIfs: List[DiEdge[GraphNode]] = formModel.allValidIfs.flatMap { case (validIfs, fc) =>
      validIfs.flatMap(validIf => boolenExprDeps(validIf.booleanExpr, fc :: Nil, _ === GraphNode.Expr(FormCtx(fc.id))))
    }
    val componentIncludeIfs: List[DiEdge[GraphNode]] = formModel.allComponentIncludeIfs.flatMap {
      case (includeIf, fc) =>
        boolenExprDeps(includeIf.booleanExpr, fc :: Nil, _ => false)
    }

    val includeIfs: List[DiEdge[GraphNode]] = formModel.allIncludeIfsWithDependingFormComponents.flatMap {
      case (includeIf, dependingFCs) =>
        boolenExprDeps(includeIf.booleanExpr, dependingFCs, _ => false)
    }

    val sections = {
      val templateAndPageExprs: List[Expr] = (formTemplateExprs ++ formModel.exprsMetadata).map(_.expr)

      val allExprGNs: List[GraphNode.Expr] = templateAndPageExprs.flatMap(_.leafs).map(GraphNode.Expr.apply)
      val allSectionDeps: List[DiEdge[GraphNode]] =
        allExprGNs.flatMap(exprGN => toFormComponentId(exprGN.expr).map(fcId => exprGN ~> GraphNode.Simple(fcId)))

      val allRepeatExprDeps: List[DiEdge[GraphNode]] =
        formModel.fcIdRepeatsExprLookup.map { case (fcId, repeatsExpr) =>
          GraphNode.Simple(fcId) ~> GraphNode.Expr(repeatsExpr)
        }.toList

      allSectionDeps ++ allRepeatExprDeps
    }

    formModel.allFormComponents
      .flatMap(edges)
      .foldLeft(emptyGraph)(_ + _) ++ includeIfs ++ componentIncludeIfs ++ validIfs ++ sections
  }

  def constructDependencyGraph(
    graph: Graph[GraphNode, DiEdge]
  ): Either[graph.NodeT, Traversable[(Int, List[GraphNode])]] = {
    def sortedOuterNodes(items: Iterable[graph.NodeT]) =
      items.toList
        .map(_.toOuter)

    graph.topologicalSort
      .map(_.toLayered.map { case (index, items) =>
        (index, sortedOuterNodes(items))
      })
  }
}

class IsOneOfSum(sumInfo: SumInfo) {
  object IsSum {
    def unapply(formComponent: FormComponent): Option[Set[FormComponentId]] =
      sumInfo.dependees(formComponent.id)
  }
}

class IsOneOfStandaloneSum(standaloneSumInfo: StandaloneSumInfo) {
  object IsSum {
    def unapply(formComponent: FormComponent): Option[FormComponentId] =
      standaloneSumInfo.dependees(formComponent.id)
  }
}
