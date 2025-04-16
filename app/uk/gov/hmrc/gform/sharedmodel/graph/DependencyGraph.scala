/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.implicits._
import scalax.collection.edges.{ DiEdge, DiEdgeImplicits }
import scalax.collection.immutable.Graph
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.eval._
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, PageMode }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object DependencyGraph {

  def toGraph(
    formModel: FormModel[Interim],
    formTemplateExprs: Set[ExprMetadata]
  ): Graph[GraphNode, DiEdge[GraphNode]] =
    graphFrom(formModel, formTemplateExprs)

  private def graphFrom[T <: PageMode](
    formModel: FormModel[T],
    formTemplateExprs: Set[ExprMetadata]
  ): Graph[GraphNode, DiEdge[GraphNode]] = {

    val isSum = new IsOneOfSum(formModel.sumInfo)
    val isStandaloneSum = new IsOneOfStandaloneSum(formModel.standaloneSumInfo)

    def edges(fc: FormComponent): Set[DiEdge[GraphNode]] =
      fc match {
        case AllFormComponentExpressions(exprsMetadata) =>
          exprsMetadata.toSet[ExprMetadata].flatMap {
            case SelfReferenceProjection(
                  IsSelfReferring.No(AuthCtx(_) | UserCtx(_) | FormTemplateCtx(_))
                ) =>
              (fc.id :: Nil).map(fcId => GraphNode.Simple(fc.id) ~> GraphNode.Simple(fcId))
            case SelfReferenceProjection(IsSelfReferring.No(expr)) =>
              toDiEdge(fc, expr, _ => false)
            case SelfReferenceProjection(IsSelfReferring.Yes(expr, selfReference)) =>
              toDiEdge(fc, expr, _ === selfReference)
            case _ => Set.empty
          }
        case isSum.IsSum(values) =>
          values.flatMap { value =>
            GraphNode.Expr(FormCtx(fc.id)) ~> GraphNode.Simple(fc.id) ::
              GraphNode.Simple(value) ~> GraphNode.Expr(FormCtx(fc.id)) :: Nil
          }
        case isStandaloneSum.IsSum(fcId) =>
          Set(GraphNode.Expr(FormCtx(fcId)) ~> GraphNode.Simple(fcId))
        case _ => Set.empty
      }

    def toFormComponentId(expr: Expr): List[FormComponentId] =
      expr match {
        case FormCtx(formComponentId) => formComponentId :: Nil
        case otherwise                => List.empty
      }

    def eqBaseComponentId(expr: Expr, fc: FormComponent): Boolean = expr
      .cast[FormCtx]
      .exists(_.formComponentId.baseComponentId === fc.id.baseComponentId)

    def toDiEdge(fc: FormComponent, expr: Expr, cycleBreaker: FormComponentId => Boolean): Set[DiEdge[GraphNode]] =
      expr
        .leafs(formModel)
        .flatMap { e =>
          val fcNodes = toFormComponentId(e).map(fcId => GraphNode.Expr(e) ~> GraphNode.Simple(fcId))
          if (cycleBreaker(fc.id) && eqBaseComponentId(e, fc)) fcNodes
          else GraphNode.Simple(fc.id) ~> GraphNode.Expr(e) :: fcNodes
        }
        .toSet

    def boolenExprDeps(
      booleanExpr: BooleanExpr,
      dependingFCs: Set[FormComponent],
      cycleBreaker: GraphNode.Expr => Boolean
    ): Set[DiEdge[GraphNode]] = {

      val allExprGNs: Set[GraphNode.Expr] =
        booleanExpr.allExpressions.flatMap(_.leafs(formModel)).map(GraphNode.Expr.apply).toSet

      val dependingFCIds = dependingFCs.map(_.id)

      val deps1: Set[DiEdge[GraphNode]] =
        for {
          exprGN        <- allExprGNs.filterNot(cycleBreaker)
          dependingFCId <- dependingFCIds
        } yield GraphNode.Simple(dependingFCId) ~> exprGN

      val deps2: Set[DiEdge[GraphNode]] =
        allExprGNs.flatMap(exprGN => toFormComponentId(exprGN.expr).map(fcId => exprGN ~> GraphNode.Simple(fcId)))

      deps1 ++ deps2
    }
    val validIfs: Set[DiEdge[GraphNode]] = formModel.allValidIfs.flatMap { case (validIfs, fc) =>
      validIfs.flatMap(validIf =>
        boolenExprDeps(
          validIf.booleanExpr,
          Set(fc),
          graphNodeExpr => eqBaseComponentId(graphNodeExpr.expr, fc)
        )
      )
    }.toSet
    val componentIncludeIfs: Set[DiEdge[GraphNode]] = formModel.allComponentIncludeIfs.flatMap { case (includeIf, fc) =>
      boolenExprDeps(includeIf.booleanExpr, Set(fc), _ => false)
    }.toSet

    val includeIfs: Set[DiEdge[GraphNode]] = formModel.allIncludeIfsWithDependingFormComponents.flatMap {
      case (includeIf, dependingFCs) =>
        boolenExprDeps(includeIf.booleanExpr, dependingFCs.toSet, _ => false)
    }.toSet

    val sections: Set[DiEdge[GraphNode]] = {
      val templateAndPageExprs: Set[Expr] = (formTemplateExprs ++ formModel.exprsMetadata.toSet).map(_.expr)

      val allExprGNs: Set[GraphNode.Expr] = templateAndPageExprs.flatMap(_.leafs(formModel)).map(GraphNode.Expr.apply)
      val allSectionDeps: Set[DiEdge[GraphNode]] =
        allExprGNs.flatMap(exprGN => toFormComponentId(exprGN.expr).map(fcId => exprGN ~> GraphNode.Simple(fcId)))

      val allRepeatExprDeps: Set[DiEdge[GraphNode]] =
        formModel.fcIdRepeatsExprLookup.map { case (fcId, repeatsExpr) =>
          GraphNode.Simple(fcId) ~> GraphNode.Expr(repeatsExpr)
        }.toSet

      allSectionDeps ++ allRepeatExprDeps
    }

    val allEdges: Set[DiEdge[GraphNode]] =
      formModel.allFormComponents.toSet
        .flatMap(edges) ++ includeIfs ++ componentIncludeIfs ++ validIfs ++ sections

    val allFcIds: Set[ModelComponentId] = allEdges.flatMap { diEdge =>
      Seq(diEdge.source, diEdge.target).collectFirst {
        case GraphNode.Expr(FormCtx(fcId)) => fcId.modelComponentId
        case GraphNode.Simple(fcId)        => fcId.modelComponentId
      }
    }

    val repeatedSectionBaseIds: Set[BaseComponentId] =
      formModel.brackets.repeatingPageBrackets
        .flatMap(_.source.page.allFieldsNested.map(_.baseComponentId))
        .toSet

    val atlFieldsBaseIds: Set[BaseComponentId] =
      formModel.brackets.addToListBrackets
        .flatMap(_.source.pages.toList.flatMap(_.allFieldsNested.map(_.baseComponentId)))
        .toSet

    val repeatingPageLookup: Map[BaseComponentId, List[FormComponent]] = formModel.brackets.repeatingPageBrackets
      .flatMap(_.singletons.toList.flatMap(_.singleton.page.allFieldsNested))
      .groupBy(_.baseComponentId)

    val atlLookup: Map[BaseComponentId, List[FormComponent]] = formModel.brackets.addToListBrackets
      .flatMap(_.iterations.toList.flatMap(_.singletons.toList.flatMap(x => x.singleton.page.allFieldsNested)))
      .groupBy(_.baseComponentId)

    def mkOutsideToInsideEdge(
      indexedComponentIds: List[IndexedComponentId],
      k: BaseComponentId
    ): List[DiEdge[GraphNode]] =
      indexedComponentIds
        .collect { case x: IndexedComponentId.Indexed =>
          x
        }
        .flatMap { v =>
          val kFcId = ModelComponentId.pure(IndexedComponentId.pure(k)).toFormComponentId
          val vFcId = ModelComponentId.pure(v).toFormComponentId
          Set(
            GraphNode.Simple(kFcId) ~> GraphNode.Expr(FormCtx(vFcId)),
            GraphNode.Expr(FormCtx(vFcId)) ~> GraphNode.Simple(vFcId)
          )
        }

    // This represents either
    //  - references to atl fields made outside of atl
    //  - references to repeating section fields made outside of repeating section
    val repeatedStructureEdges: Iterable[DiEdge[GraphNode]] =
      allFcIds
        .groupBy(_.baseComponentId)
        .collect {
          case (k, _) if repeatedSectionBaseIds(k) =>
            val indexedComponentIds: List[IndexedComponentId] =
              repeatingPageLookup.get(k).toList.flatten.map(_.modelComponentId.indexedComponentId)
            mkOutsideToInsideEdge(indexedComponentIds, k)

          case (k, _) if atlFieldsBaseIds(k) =>
            val indexedComponentIds: List[IndexedComponentId] =
              atlLookup.get(k).toList.flatten.map(_.modelComponentId.indexedComponentId)
            mkOutsideToInsideEdge(indexedComponentIds, k)
        }
        .flatten

    val emptyGraph: Graph[GraphNode, DiEdge[GraphNode]] = Graph.empty
    emptyGraph ++ (allEdges ++ repeatedStructureEdges)
  }

  def constructDependencyGraph(
    graph: Graph[GraphNode, DiEdge[GraphNode]]
  ): Either[graph.NodeT, Iterable[(Int, List[GraphNode])]] = {
    def sortedOuterNodes(items: Iterable[graph.NodeT]) =
      items.toList
        .map(_.outer)

    graph.topologicalSort
      .map(_.toLayered.map { case (index, items) =>
        (index, sortedOuterNodes(items))
      }) match {
      case Left(value) => Left(graph.nodes.head)
      case Right(value) => Right(value)
    }
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
