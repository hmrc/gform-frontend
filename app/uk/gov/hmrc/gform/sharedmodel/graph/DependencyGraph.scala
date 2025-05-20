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
import uk.gov.hmrc.gform.models.{ FormModel, HasIncludeIf, Interim, PageMode, PageModel }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic.AddToListPage
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.mutable
import scala.util.Try

object DependencyGraph {

  def toGraph(
    formModel: FormModel[Interim],
    formTemplateExprs: Set[ExprMetadata],
    currentPage: Option[PageModel[_]]
  ): Graph[GraphNode, DiEdge[GraphNode]] =
    graphFrom(formModel, formTemplateExprs, currentPage)

  private def graphFrom[T <: PageMode](
    formModel: FormModel[T],
    formTemplateExprs: Set[ExprMetadata],
    currentPage: Option[PageModel[_]]
  ): Graph[GraphNode, DiEdge[GraphNode]] = {
//    formModel.brackets.map { singleton =>
//      println(singleton.title)
//      println(singleton)
//      singleton
//    }(p)(p)

    //println(currentPage)
    val isSum = new IsOneOfSum(formModel.sumInfo)
    val isStandaloneSum = new IsOneOfStandaloneSum(formModel.standaloneSumInfo)

    val pagesWithSectionNumber = formModel.brackets.toPageModelWithNumber.toList

    val orderedPagesWithSectionNumber = pagesWithSectionNumber.sortWith {
      case ((page, sectionNumber1), (page2, sectionNumber2)) =>
        Try(sectionNumber1.compare(sectionNumber2) < 0).getOrElse(false)
    }

    val pageGraph: Graph[PageModel[_], DiEdge[PageModel[_]]] =
      Graph.from(orderedPagesWithSectionNumber.zipWithIndex.tail.map { case ((page, sectionNumber), index) =>
        val pageOrigin = orderedPagesWithSectionNumber(index - 1)._1
        pageOrigin ~> page
      })

//    val formComponents = formModel.allFormComponents

    val (formComponents, pages) = currentPage
      .map { currentPage =>
        val pages = mutable.Set[PageModel[_]]()
        val formComponents = mutable.Set[FormComponent]()
        def addSetOfFormComponents(node: pageGraph.NodeT): Unit = {
          formComponents.addAll(node.allFormComponents)
          pages.add(node)
          node.outgoing.foreach { edge =>
            addSetOfFormComponents(edge.targets.head)
          }
        }
        if (pageGraph.isEmpty) {
          pages.add(currentPage)
          formComponents.addAll(currentPage.allFormComponents)
        } else {
          //TODO remove try
          Try(pageGraph.get(currentPage)).foreach {
            addSetOfFormComponents
          }
        }
        formComponents -> pages
      }
      .getOrElse(Set.empty -> Set.empty)

//    println("form components size: " + formComponents.size)
//    println("Pages size: " + pages.size)

    val formComponentsList = formComponents.toList

    val pagesAllValidIfs = pages.flatMap { page =>
      page.allValidIfs
    }

    def edges(fc: FormComponent): Set[DiEdge[GraphNode]] =
      fc match {
        case AllFormComponentExpressions(exprsMetadata) =>
          exprsMetadata.toSet[ExprMetadata].flatMap {
            case SelfReferenceProjection(
                  IsSelfReferring.No(AuthCtx(_) | UserCtx(_) | FormTemplateCtx(_))
                ) =>
              Set(
                GraphNode.Simple(fc.id) ~> GraphNode.Simple(fc.id)
              )
            case SelfReferenceProjection(IsSelfReferring.No(expr)) =>
              toDiEdge(fc, expr, _ => false)
            case SelfReferenceProjection(IsSelfReferring.Yes(expr, selfReference)) =>
              toDiEdge(fc, expr, _ === selfReference)
            case _ => Set.empty
          }
        case isSum.IsSum(values) =>
          values.flatMap { value =>
            GraphNode.Simple(value) ~> GraphNode.Expr(FormCtx(fc.id)) :: Nil
          } + GraphNode.Expr(FormCtx(fc.id)) ~> GraphNode.Simple(fc.id)
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
        .leafs(formComponentsList)
        .flatMap { e =>
          val fcNodes = toFormComponentId(e).map(fcId => GraphNode.Expr(e) ~> GraphNode.Simple(fcId))
          if (cycleBreaker(fc.id) && eqBaseComponentId(e, fc)) fcNodes
          else GraphNode.Simple(fc.id) ~> GraphNode.Expr(e) :: fcNodes
        }
        .toSet

    def getAllEdges = {
      val allEdges: mutable.Set[DiEdge[GraphNode]] = mutable.Set
        .from(formComponents)
        .flatMap(edges)

      def boolenExprDeps(
        booleanExpr: BooleanExpr,
        dependingFCs: Set[FormComponent],
        cycleBreaker: Option[GraphNode.Expr => Boolean]
      ): Unit = {

        val allExprGNs: Set[GraphNode.Expr] =
          booleanExpr.allExpressions.flatMap(_.leafs(formComponentsList)).map(GraphNode.Expr.apply).toSet

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

      def addValidIfs() = pagesAllValidIfs.foreach { case (validIfs, fc) =>
        validIfs.foreach(validIf =>
          boolenExprDeps(
            validIf.booleanExpr,
            Set(fc),
            Some(graphNodeExpr => eqBaseComponentId(graphNodeExpr.expr, fc))
          )
        )
      }

      def addComponentIncludeIfs() = pages.flatMap(_.allComponentIncludeIfs).foreach { case (includeIf, fc) =>
        boolenExprDeps(includeIf.booleanExpr, Set(fc), None)
      }

      val allChoiceIncludeIfs = pages.flatMap(_.allChoiceIncludeIfs)

      val allMiniSummaryListIncludeIfs = pages.flatMap(_.allMiniSummaryListIncludeIfs)

      val allIncludeIfsWithDependingFormComponents = pages.collect { case pm @ HasIncludeIf(includeIf) =>
        (includeIf, pm.fold(_.page.allFields)(_ => Nil)(_.addAnotherQuestion :: Nil))
      } ++ allChoiceIncludeIfs.map(i => (i._1, List(i._2))) ++ allMiniSummaryListIncludeIfs.map(i => (i._1, List(i._2)))

      def addIncludeIfs() = allIncludeIfsWithDependingFormComponents.foreach { case (includeIf, dependingFCs) =>
        boolenExprDeps(includeIf.booleanExpr, dependingFCs.toSet, None)
      }

      def addSections() = {
        val templateAndPageExprs: Set[Expr] = (formTemplateExprs ++ formModel.exprsMetadata.toSet).map(_.expr)

        val allExprGNs: Set[GraphNode.Expr] =
          templateAndPageExprs.flatMap(_.leafs(formComponents.toList)).map(GraphNode.Expr.apply)

        allExprGNs.foreach(exprGN =>
          toFormComponentId(exprGN.expr).foreach(fcId => allEdges.addOne(exprGN ~> GraphNode.Simple(fcId)))
        )

        formModel.fcIdRepeatsExprLookup.foreach { case (fcId, repeatsExpr) =>
          allEdges.addOne(GraphNode.Simple(fcId) ~> GraphNode.Expr(repeatsExpr))
        }
      }

//      println("all edges: " + allEdges.size)
      addValidIfs()
//      println("all edges after add valid ifs: " + allEdges.size)
      addComponentIncludeIfs()
//      println("all edges after add component include ifs: " + allEdges.size)
      addIncludeIfs()
//      println("all edges after add include ifs: " + allEdges.size)
      addSections()
//      println("all edges after add secions: " + allEdges.size)
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
    Graph.from(allEdges)
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
