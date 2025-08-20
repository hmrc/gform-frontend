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

import scala.collection.mutable

object DependencyGraph {

  def toGraph(
    formModel: FormModel[Interim],
    formTemplateExprs: Set[ExprMetadata]
  ): (Graph[GraphNode, DiEdge[GraphNode]], Set[In]) =
    graphFrom(formModel, formTemplateExprs)

  private def graphFrom[T <: PageMode](
    formModel: FormModel[T],
    formTemplateExprs: Set[ExprMetadata]
  ): (Graph[GraphNode, DiEdge[GraphNode]], Set[In]) = {

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

    val inExprs = mutable.Set[In]()

    def getAllEdges = {
      val allEdges: mutable.Set[DiEdge[GraphNode]] = mutable.Set
        .from(formModel.allFormComponents)
        .flatMap(edges)

      def boolenExprDeps(
        booleanExpr: BooleanExpr,
        dependingFCs: Set[FormComponent],
        cycleBreaker: Option[GraphNode.Expr => Boolean]
      ): Unit = {

        booleanExpr match {
          case in: In => inExprs.add(in)
          case _      => ()
        }

        val allExprGNs: Set[GraphNode.Expr] =
          booleanExpr.allExpressions.flatMap(_.leafs(formModel)).map(GraphNode.Expr.apply).toSet

        val dependingFCIds = dependingFCs.map(_.id)

        allEdges.addAll(
          for {
            exprGN        <- cycleBreaker.map(cycleBreaker => allExprGNs.filterNot(cycleBreaker)).getOrElse(allExprGNs)
            dependingFCId <- dependingFCIds
          } yield GraphNode.Simple(dependingFCId) ~> exprGN
        )

        allExprGNs.foreach(exprGN =>
          toFormComponentId(exprGN.expr).foreach(fcId => allEdges.addOne(exprGN ~> GraphNode.Simple(fcId)))
        )
      }

      def addValidIfs() = formModel.allValidIfs.foreach { case (validIfs, fc) =>
        validIfs.foreach(validIf =>
          boolenExprDeps(
            validIf.booleanExpr,
            Set(fc),
            Some(graphNodeExpr => eqBaseComponentId(graphNodeExpr.expr, fc))
          )
        )
      }

      def addComponentIncludeIfs() = formModel.allComponentIncludeIfs.foreach { case (includeIf, fc) =>
        boolenExprDeps(includeIf.booleanExpr, Set(fc), None)
      }

      def addIncludeIfs() = formModel.allIncludeIfsWithDependingFormComponents.foreach {
        case (includeIf, dependingFCs) =>
          boolenExprDeps(includeIf.booleanExpr, dependingFCs.toSet, None)
      }

      def addSections() = {
        val templateAndPageExprs: Set[Expr] = (formTemplateExprs ++ formModel.exprsMetadata.toSet).map(_.expr)

        val allExprGNs: Set[GraphNode.Expr] = templateAndPageExprs.flatMap(_.leafs(formModel)).map(GraphNode.Expr.apply)

        allExprGNs.foreach(exprGN =>
          toFormComponentId(exprGN.expr).foreach(fcId => allEdges.addOne(exprGN ~> GraphNode.Simple(fcId)))
        )

        formModel.fcIdRepeatsExprLookup.foreach { case (fcId, repeatsExpr) =>
          allEdges.addOne(GraphNode.Simple(fcId) ~> GraphNode.Expr(repeatsExpr))
        }
      }

      addValidIfs()
      addComponentIncludeIfs()
      addIncludeIfs()
      addSections()
      allEdges
    }

    val allEdges = getAllEdges

    val allFcIds: mutable.Set[ModelComponentId] = allEdges.flatMap { diEdge =>
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
    def addRepeatedStructureEdges(set: mutable.Set[DiEdge[GraphNode]]): Unit =
      allFcIds
        .groupBy(_.baseComponentId)
        .foreach {
          case (k, _) if repeatedSectionBaseIds(k) =>
            val indexedComponentIds: List[IndexedComponentId] =
              repeatingPageLookup.get(k).toList.flatten.map(_.modelComponentId.indexedComponentId)
            set.addAll(mkOutsideToInsideEdge(indexedComponentIds, k))

          case (k, _) if atlFieldsBaseIds(k) =>
            val indexedComponentIds: List[IndexedComponentId] =
              atlLookup.get(k).toList.flatten.map(_.modelComponentId.indexedComponentId)
            set.addAll(mkOutsideToInsideEdge(indexedComponentIds, k))
          case _ => ()
        }

    addRepeatedStructureEdges(allEdges)
    Graph.from(allEdges) -> inExprs.toSet
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
      })
      .leftMap { failure =>
        failure.cycle.getOrElse(throw new RuntimeException("Topological sort issue")).endNode
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
