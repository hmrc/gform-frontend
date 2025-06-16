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

package uk.gov.hmrc.gform.graph

import cats.syntax.all._
import cats.{ Monad, MonadError }
import play.api.i18n.Messages
import scalax.collection.edges.{ DiEdge, DiEdgeImplicits }
import scalax.collection.immutable.Graph
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionNumber.Classic

import scala.util.Try
//import scalax.collection.io.dot.DotRootGraph
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest
import uk.gov.hmrc.gform.auth.models.{ IdentifierValue, MaterialisedRetrievals }
import uk.gov.hmrc.gform.eval._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.{ Basic, FormModel, Interim, PageModel, Singleton }
import uk.gov.hmrc.gform.sharedmodel.LangADT.{ Cy, En }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, LangADT, SourceOrigin, VariadicFormData, VariadicValue }

import java.nio.file.{ Files, Paths }
import scala.collection.{ immutable, mutable }

sealed trait GraphException {
  def reportProblem: String = this match {
    case NoTopologicalOrder(fcId, graph) =>
      s"$graph is not possible to sort topologically. Violating node is $fcId. Does graph contains cycle: ${graph.findCycle}"
    case NoFormComponent(fcId, lookup) =>
      s"""No FormComponent found for $fcId. Available FormComponents: ${lookup.keys.mkString(",")}"""
  }
}

case class NoTopologicalOrder(fcId: GraphNode, graph: Graph[GraphNode, DiEdge[GraphNode]]) extends GraphException
case class NoFormComponent(fcId: FormComponentId, lookup: Map[FormComponentId, FormComponent]) extends GraphException

class Recalculation[F[_]: Monad, E](
  val seissEligibilityChecker: SeissEligibilityChecker[F],
  val delegatedEnrolmentCheckStatus: DelegatedEnrolmentChecker[F],
  val dbLookupCheckStatus: DbLookupChecker[F],
  val error: GraphException => E
) {

  def recalculateFormDataNew(
    data: VariadicFormData[SourceOrigin.OutOfDate],
    formModel: FormModel[Interim],
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    evaluationContext: EvaluationContext,
    messages: Messages
  )(implicit me: MonadError[F, E]): F[RecalculationResult] = {
    val formTemplateExprs: Set[ExprMetadata] = AllFormTemplateExpressions.apply(formTemplate)
    val graph: Graph[GraphNode, DiEdge[GraphNode]] =
      DependencyGraph.toGraph(formModel, formTemplateExprs, evaluationContext.currentSection, formTemplate)
    recalculateFromGraph(
      formModel,
      formTemplate,
      retrievals,
      evaluationContext,
      messages,
      graph,
      Map.empty,
      data.data,
      thirdPartyData.booleanExprCache.mapping
    )
  }

  def recalculateFromGraph(
    formModel: FormModel[Interim],
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    messages: Messages,
    graph: Graph[GraphNode, DiEdge[GraphNode]],
    exprMapStart: collection.Map[Expr, ExpressionResult],
    formDataMapStart: collection.Map[ModelComponentId, VariadicValue],
    booleanExprCacheStart: Map[DataSource, Map[String, Boolean]]
  )(implicit me: MonadError[F, E]): F[RecalculationResult] = {

    implicit val fm: FormModel[Interim] = formModel

//    val page = evaluationContext.currentPage.flatMap {
//      _.fold[Option[Singleton[_]]](Some(_))(_ => None)(_ => None)
//    }

    //    def dotRoot = DotRootGraph(
    //      true,
    //      Some(scalax.collection.io.dot.Id("ExampleGraph"))
    //    )

//    def toIndexToInts(index: SectionNumber): (Int, Int, Int) =
//      index
//        .fold[Option[(Int, Int, Int)]] {
//          case Classic.NormalPage(sectionIndex)               => Some((0, sectionIndex.index, 0))
//          case Classic.RepeatedPage(sectionIndex, pageNumber) => Some((0, sectionIndex.index, pageNumber))
//          case page: Classic.AddToListPage =>
//            Some((0, 0, 0))
//        } { taskList =>
//          Some(
//            taskList.coordinates.taskSectionNumber.value,
//            taskList.coordinates.taskNumber.value,
//            taskList.templateSectionIndex.index
//          )
//        }
//        .getOrElse(0, 0, 0)

//    formModel.addToListBrackets
//      .flatMap(_.toPageModelWithNumber.toList)

//    val list = formModel.brackets.toPageModelWithNumber.toList.map(x => x._1 -> toIndexToInts(x._2))

//    val orderedList = list.sortWith { case ((page, (a, b, c)), (page2, (a2, b2, c2))) =>
//      a < a2 && b < b2 && c < c2
//    }

    //val pageToIndex: Map[PageModel[_], (Int, Int, Int)] = list.toMap
    //val indexToPage = pageToIndex.map(x => x._2 -> x._1)

    //println(orderedList.map(_._2))

//    val pageGraph: Graph[PageModel[_], DiEdge[PageModel[_]]] = {
//      var prevPageOrigin: PageModel[Interim] = orderedList.head._1
//      Graph.from(orderedList.zipWithIndex.tail.flatMap { case ((page, sectionNumber), index) =>
//        val pageInternal = page.fold[Option[Page[Interim]]](x => Some(x.page))(_ => None)(_ => None)
//        val containsExpr = pageInternal.exists(AllFormTemplateExpressions.fromPage(_).nonEmpty)
//        if (containsExpr) {
//          val pageOrigin = prevPageOrigin
//          prevPageOrigin = page
//          Some(pageOrigin ~> page)
//        } else {
//          None
//        }
//      })
//    }

//    val pageGraph: Graph[PageModel[_], DiEdge[PageModel[_]]] =
//      Graph.from(orderedList.zipWithIndex.tail.map { case ((page, sectionNumber), index) =>
//        val pageOrigin = orderedList(index - 1)._1
//        pageOrigin ~> page
//      })

//    println("total form expressions: " + formTemplateExprs.size)
//
//    println("total form expression comp size: " + formTemplateExprs.flatMap(_.expr.allFormComponentIds()).size)

//    val formComponents = page
//      .map { page =>
//        val set = mutable.Set[FormComponent]()
//        def addSetOfFormComponents(node: PageModel[_], set: mutable.Set[FormComponent]): Unit = {
//          val pageInternal = page.fold[Option[Page[_]]](x => Some(x.page))(_ => None)(_ => None)
//          pageInternal.foreach { page =>
//            val exprs = AllFormTemplateExpressions
//              .fromPage(page)
//            val formIds = exprs.flatMap(expr => expr.expr.allFormComponentIds())
//            val formComponents = formIds.map(formModel.fcLookup)
//            set.addAll(formComponents)
//            val pages = formIds.map(formModel.pageLookup)
//            println("pages size: " + pages.size)
//            pages.foreach {
//              case page if page != node =>
//                addSetOfFormComponents(page, set)
//            }
//          }
//          //          set.addAll(AllFormTemplateExpressions.fromPage(pageInternal))
//          //          node.outgoing.foreach { edge =>
//          //            addSetOfFormComponents(edge.targets.head, set)
//          //          }
//        }
//
//        addSetOfFormComponents(page, set)
//        set
//      }
//      .getOrElse(mutable.Set())

//    println("pageGraph edge size: " + pageGraph.edges.size)
//
//    val formComponents = page
//      .map { page =>
//        val set = mutable.Set[FormComponent]()
//        def addSetOfFormComponents(node: pageGraph.NodeT, set: mutable.Set[FormComponent]): Unit = {
//          // val pageInternal = page.fold[Option[Page[_]]](x => Some(x.page))(_ => None)(_ => None)
////          pageInternal.foreach { page =>
////            set.addAll(
////              AllFormTemplateExpressions
////                .fromPage(page)
////                .flatMap(_.expr.allFormComponentIds().map(fcId => formModel.fcLookup(fcId)))
////                .map { fc =>
////                  formModel.pageLookup
////                }
////            )
////          }
//          set.addAll(node.allFormComponents)
//          node.outgoing.foreach { edge =>
//            addSetOfFormComponents(edge.targets.head, set)
//          }
//        }
//        if (pageGraph.isEmpty) {
//          set.addAll(page.allFormComponents)
//        } else {
//          addSetOfFormComponents(pageGraph.get(page), set)
//        }
//        set
//      }
//      .getOrElse(mutable.Set())

//    println("formComponents size: " + formComponents.size)

    //    val graph: Graph[GraphNode, DiEdge[GraphNode]] = page
//      .map { page =>
//        DependencyGraph.toGraph(formModel, formTemplateExprs, page)
//      }
//      .getOrElse(Graph.empty)

    //import scalax.collection.io.dot._
//    var exprPar = 0
//    def edgeTransformer(innerEdge: Graph[GraphNode, DiEdge[GraphNode]]#EdgeT): Option[(DotGraph, DotEdgeStmt)] = {
//      val edge = innerEdge.outer
//      def toId(g: GraphNode) =
//        g match {
//          case GraphNode.Simple(formComponentId) => NodeId(formComponentId.value)
//          case GraphNode.Expr(expr) =>
//            exprPar = exprPar + 1
//            NodeId(expr.toString)
//        }
//      Some(
//        dotRoot ->
//          DotEdgeStmt(
//            NodeId(edge.source.toString),
//            NodeId(edge.target.toString)
//          )
//      )
//    }
//
//    def edgeTransformerPageModel(
//      innerEdge: Graph[String, DiEdge[String]]#EdgeT
//    ): Option[(DotGraph, DotEdgeStmt)] = {
//      val edge = innerEdge.outer
//      Some(
//        dotRoot ->
//          DotEdgeStmt(
//            NodeId(edge.source),
//            NodeId(edge.target)
//          )
//      )
//    }

//    val dot = graph.toDot(dotRoot, edgeTransformer)
//    Files.write(Paths.get("graph.dot"), dot.getBytes)
//    val dotFormComponent = pageGraph.toDot(dotRoot, edgeTransformerPageModel)
//    Files.write(Paths.get("graph-page.dot"), dotFormComponent.getBytes)

    val orderedGraph: Either[GraphException, Iterable[(Int, List[GraphNode])]] = DependencyGraph
      .constructDependencyGraph(graph)
      .leftMap(node => NoTopologicalOrder(node.outer, graph))

    //println("ordered graph size: " + orderedGraph.right.get.flatMap(_._2).size)

    val exprMap = mutable.Map.newBuilder.addAll(exprMapStart).result()
    val formDataMap = mutable.Map.newBuilder.addAll(formDataMapStart).result()

    val startEvResults = EvaluationResults(
      exprMap,
      SourceOrigin.changeSource(RecData.fromData(VariadicFormData(formDataMap))),
      formTemplate.formKind.repeatedComponentsDetails
    )
    val booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]] =
      booleanExprCacheStart
        .foldLeft(
          mutable.Map.newBuilder[DataSource, mutable.Map[String, Boolean]]
        ) { case (builder, (key, value)) =>
          builder.addOne(key -> mutable.Map.newBuilder.addAll(value).result())
        }
        .result()

    val res: Either[GraphException, F[Iterable[(Int, List[GraphNode])]]] =
      for {
        graphTopologicalOrder <- orderedGraph
      } yield {
        val recalc = graphTopologicalOrder.toList.reverse.foldLeft(().pure[F]) { case (state, (_, graphLayer)) =>
//          println("graph layer fcids: " + graphLayer.flatMap {
//            case GraphNode.Simple(formComponentId) => List(formComponentId)
//            case GraphNode.Expr(expr)              => expr.allFormComponentIds()
//          })
          //println(graphLayer)
          //println(graphLayer.length)
          recalculateGraphLayer(
            graphLayer,
            state.map(_ => startEvResults),
            retrievals,
            evaluationContext,
            messages,
            exprMap,
            formDataMap,
            booleanExprCacheMap
          )
        }
        recalc.map { _ =>
          graphTopologicalOrder
        }
      }

    def immutableBooleanExprCacheMap = booleanExprCacheMap
      .foldLeft(
        immutable.Map.newBuilder[DataSource, immutable.Map[String, Boolean]]
      ) { case (builder, (key, value)) =>
        builder.addOne(key -> value.toMap)
      }
      .result()

    res match {
      case Left(graphException) => me.raiseError(error(graphException))
      case Right(fd) =>
        fd.map { graphTopologicalOrder =>
          RecalculationResult(
            startEvResults,
            GraphData(graphTopologicalOrder, graph),
            BooleanExprCache(immutableBooleanExprCacheMap),
            evaluationContext
          )

        }
    }
  }

  private def recalculateGraphLayer(
    graphLayer: List[GraphNode],
    state: F[EvaluationResults],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    messages: Messages,
    exprMap: mutable.Map[Expr, ExpressionResult],
    variadicFormDataMap: mutable.Map[ModelComponentId, VariadicValue],
    booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]]
  )(implicit formModel: FormModel[Interim]): F[Unit] =
    state.flatMap { evResult =>
      val recData = SourceOrigin.changeSourceToOutOfDate(evResult.recData)

      val booleanExprResolver = BooleanExprResolver { booleanExpr =>
        evalBooleanExprPure(booleanExpr, evResult, recData, retrievals, evaluationContext)
      }

      def evalExpr(expr: Expr) = {
        val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)
        evResult
          .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
          .applyTypeInfo(typeInfo)
          .stringRepresentation(typeInfo, messages)
      }

      val graphLayerResult = graphLayer.traverseVoid {

        case GraphNode.Simple(fcId) =>
          val fc: Option[FormComponent] = formModel.fcLookup.get(fcId)
          val isOptionHidden = fc.exists {
            case IsChoice(_) | IsRevealingChoice(_) =>
              val userResponse: Seq[String] = recData.variadicFormData.many(fcId.modelComponentId).toSeq.flatten
              val optionData: List[OptionData] = fc
                .collect {
                  case IsChoice(c) =>
                    c.options.toList.collect {
                      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _)
                          if userResponse.contains(value) =>
                        o
                      case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(expr), _)
                          if userResponse.contains(evalExpr(expr)) =>
                        o
                      case o: OptionData.IndexBased if userResponse.contains(o.toString) => o
                    }
                  case IsRevealingChoice(rc) => rc.options.map(_.choice)
                }
                .getOrElse(List.empty[OptionData])

              val includeIfs: List[IncludeIf] = optionData.collect {
                case OptionData.ValueBased(_, _, Some(includeIf), _, _, _) => includeIf
                case OptionData.IndexBased(_, _, Some(includeIf), _, _)    => includeIf
              }

              val isHidden = includeIfs
                .map(i => booleanExprResolver.resolve(i.booleanExpr))
                .reduceOption(_ && _)
                .getOrElse(true)
              !isHidden
            case _ => false
          }

          val recDataUpd: RecData[OutOfDate] =
            if (isOptionHidden) {
              variadicFormDataMap.remove(fcId.modelComponentId)
              recData
            } else {
              recData
            }

          val res = for {
            isHiddenIncludeIf <-
              isHiddenByIncludeIf(
                fcId,
                evResult,
                recData,
                retrievals,
                booleanExprResolver,
                evaluationContext,
                exprMap,
                booleanExprCacheMap
              )
            isHiddenComponentIncludeIf <-
              isHiddenByComponentIncludeIf(
                fcId,
                evResult,
                recData,
                retrievals,
                booleanExprResolver,
                evaluationContext,
                exprMap,
                booleanExprCacheMap
              )
            isHiddenRevealingChoice <- isHiddenByRevealingChoice(fcId, recData)
            isHiddenRepeatsExpr <-
              isHiddenByRepeatsExpr(fcId, evResult, recData, booleanExprResolver, evaluationContext)
          } yield
            if (isHiddenIncludeIf || isHiddenRevealingChoice || isHiddenComponentIncludeIf || isHiddenRepeatsExpr) {
              exprMap.addOne((FormCtx(fcId), ExpressionResult.Hidden))
              evResult
            } else {
              evResult
            }

          res.map(_.copy(recData = SourceOrigin.changeSource(recDataUpd)))

        case GraphNode.Expr(formCtx @ FormCtx(formComponentId)) =>
          val expr: Expr = formModel.fcLookup
            .get(formComponentId)
            .collect { case HasValueExpr(expr) =>
              expr
            }
            .getOrElse(formCtx)
          val typeInfo: TypeInfo = formModel.explicitTypedExpr(expr, formComponentId)

          val exprResult: ExpressionResult =
            evResult
              .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
              .applyTypeInfo(typeInfo)
          val newExpr = (
            formCtx,
            evResult.get(formCtx).fold(exprResult) {
              case ExpressionResult.Hidden => ExpressionResult.Hidden // If something is Hidden keep it so.
              case _                       => exprResult
            }
          )
          exprMap.addOne(newExpr)
          evResult.pure[F]

        case GraphNode.Expr(expr) =>
          val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)

          val exprResult: ExpressionResult =
            evResult.evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)

          exprMap.addOne((expr, exprResult))
          evResult.pure[F]
      }

      // We are only interested in `ValidIf` with `In` expression and any other `validIf` is being ignored
      evalValidIfs(
        evResult,
        recData,
        retrievals,
        booleanExprResolver,
        evaluationContext,
        exprMap,
        booleanExprCacheMap
      ) >> {
        if (graphLayer.isEmpty) evResult.pure[F].void else graphLayerResult
      }

    }

  def evalBooleanExprPure(
    booleanExpr: BooleanExpr,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): Boolean = {

    val booleanExprResolver = BooleanExprResolver { booleanExpr =>
      evalBooleanExprPure(booleanExpr, evaluationResults, recData, retrievals, evaluationContext)
    }

    val rr =
      new RecalculationResolver(
        formModel,
        evaluationResults,
        recData,
        booleanExprResolver,
        evaluationContext
      )

    def loop(booleanExpr: BooleanExpr): Boolean = booleanExpr match {
      case Equals(field1, field2)              => rr.compare(field1, field2, _ identical _)
      case GreaterThan(field1, field2)         => rr.compare(field1, field2, _ > _)
      case DateAfter(field1, field2)           => rr.compareDate(field1, field2, _ after _)
      case GreaterThanOrEquals(field1, field2) => rr.compare(field1, field2, _ >= _)
      case LessThan(field1, field2)            => rr.compare(field1, field2, _ < _)
      case DateBefore(field1, field2)          => rr.compareDate(field1, field2, _ before _)
      case LessThanOrEquals(field1, field2)    => rr.compare(field1, field2, _ <= _)
      case Not(invertedExpr)                   => !loop(invertedExpr)
      case Or(expr1, expr2)                    => loop(expr1) | loop(expr2)
      case And(expr1, expr2)                   => loop(expr1) & loop(expr2)
      case IsTrue                              => true
      case IsFalse                             => false
      case Contains(field1, field2)            => rr.compare(field1, field2, _ contains _)
      case MatchRegex(expr, regex)             => rr.matchRegex(expr, regex)
      case FormPhase(value)                    => rr.compareFormPhase(value)
      case In(expr, dataSource)                => false
      case h @ HasAnswer(_, _) =>
        BooleanExprEval
          .evalHasAnswer(
            h,
            formModel,
            evaluationResults,
            evaluationContext,
            booleanExprResolver,
            SourceOrigin.changeSource(recData)
          )
      case DuplicateExists(fieldList)      => BooleanExprEval.evalDuplicateExpr(fieldList, recData)
      case First(FormCtx(formComponentId)) => BooleanExprEval.evalFirstExpr(formComponentId)
      case IsLogin(value)                  => BooleanExprEval.evalIsLoginExpr(value, retrievals)
    }

    loop(booleanExpr)
  }

  def evalBooleanExpr(
    booleanExpr: BooleanExpr,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext,
    exprMap: mutable.Map[Expr, ExpressionResult],
    booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]]
  )(implicit formModel: FormModel[Interim]): F[Boolean] = {

    val rr =
      new RecalculationResolver(
        formModel,
        evaluationResults,
        recData,
        booleanExprResolver,
        evaluationContext
      )

    def addToBooleanCache(dataSource: DataSource, value: String, result: Boolean): Unit = {
      val updatedDataSourceMap =
        booleanExprCacheMap.get(dataSource).fold(mutable.Map(value -> result)) { m =>
          m.addOne(value -> result)
          m
        }
      booleanExprCacheMap.addOne(dataSource -> updatedDataSourceMap)
    }

    def loop(booleanExpr: BooleanExpr): F[Boolean] = booleanExpr match {
      case Equals(field1, field2) =>
        rr.compareF(field1, field2, _ identical _)
      case GreaterThan(field1, field2)         => rr.compareF(field1, field2, _ > _)
      case DateAfter(field1, field2)           => rr.compareDateF(field1, field2, _ after _)
      case GreaterThanOrEquals(field1, field2) => rr.compareF(field1, field2, _ >= _)
      case LessThan(field1, field2)            => rr.compareF(field1, field2, _ < _)
      case DateBefore(field1, field2)          => rr.compareDateF(field1, field2, _ before _)
      case LessThanOrEquals(field1, field2)    => rr.compareF(field1, field2, _ <= _)
      case Not(invertedExpr)                   => loop(invertedExpr).map(!_)
      case Or(expr1, expr2)                    => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 | e2
      case And(expr1, expr2)                   => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 & e2
      case IsTrue                              => true.pure[F]
      case IsFalse                             => false.pure[F]
      case Contains(field1, field2)            => rr.compareF(field1, field2, _ contains _)
      case MatchRegex(expr, regex)             => rr.matchRegexF(expr, regex)
      case FormPhase(value)                    => rr.compareFormPhaseF(value)
      case DuplicateExists(fieldList)          => BooleanExprEval.evalDuplicateExpr(fieldList, recData).pure[F]
      case In(expr, dataSource) =>
        val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)
        val expressionResult: ExpressionResult =
          evaluationResults
            .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
            .applyTypeInfo(typeInfo)
        val hc = evaluationContext.headerCarrier

        expressionResult.convertNumberToString.withStringResult(false.pure[F]) { value =>
          exprMap.addOne((expr, expressionResult))
          def makeCall(): F[Boolean] = dataSource match {
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

          def booleanValue = for {
            dataSourceValue <- booleanExprCacheMap.get(dataSource)
            booleanValue    <- dataSourceValue.get(value)
          } yield booleanValue

          booleanValue.fold(makeCall().map { res =>
            addToBooleanCache(dataSource, value, res)
            res
          })(_.pure[F])
        }
      case h @ HasAnswer(_, _) =>
        BooleanExprEval
          .evalHasAnswer(
            h,
            formModel,
            evaluationResults,
            evaluationContext,
            booleanExprResolver,
            SourceOrigin.changeSource(recData)
          )
          .pure[F]
      case First(FormCtx(formComponentId)) => BooleanExprEval.evalFirstExpr(formComponentId).pure[F]
      case IsLogin(value)                  => BooleanExprEval.evalIsLoginExpr(value, retrievals).pure[F]
    }

    loop(booleanExpr)
  }

  private def evalBooleanExpr(
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext,
    exprMap: mutable.Map[Expr, ExpressionResult],
    booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]]
  )(
    booleanExpr: Option[BooleanExpr]
  )(implicit formModel: FormModel[Interim]): F[Boolean] =
    booleanExpr.fold(false.pure[F]) { booleanExpr =>
      for {
        b <- evalBooleanExpr(
               booleanExpr,
               evaluationResults,
               recData,
               retrievals,
               booleanExprResolver,
               evaluationContext,
               exprMap,
               booleanExprCacheMap
             )
      } yield !b
    }

  private def evalValidIfs(
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext,
    exprMap: mutable.Map[Expr, ExpressionResult],
    booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]]
  )(implicit formModel: FormModel[Interim]): F[Unit] = {
    val validIfs: List[ValidIf] = formModel.allValidIfs.flatMap(_._1)

    validIfs
      .traverse(validIf =>
        evalBooleanExpr(
          evaluationResults,
          recData,
          retrievals,
          booleanExprResolver,
          evaluationContext,
          exprMap,
          booleanExprCacheMap
        ) {
          Some(validIf.booleanExpr)
        }
      )
      .void
  }

  private def isHiddenByIncludeIf(
    fcId: FormComponentId,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext,
    exprMap: mutable.Map[Expr, ExpressionResult],
    booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]]
  )(implicit formModel: FormModel[Interim]): F[Boolean] = {
    val pageLookup: Map[FormComponentId, PageModel[Interim]] = formModel.pageLookup
    evalBooleanExpr(
      evaluationResults,
      recData,
      retrievals,
      booleanExprResolver,
      evaluationContext,
      exprMap,
      booleanExprCacheMap
    ) {
      pageLookup
        .get(fcId)
        .flatMap(_.getIncludeIf)
        .map(_.booleanExpr)
    }
  }

  private def isHiddenByComponentIncludeIf(
    fcId: FormComponentId,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext,
    exprMap: mutable.Map[Expr, ExpressionResult],
    booleanExprCacheMap: mutable.Map[DataSource, mutable.Map[String, Boolean]]
  )(implicit formModel: FormModel[Interim]): F[Boolean] =
    evalBooleanExpr(
      evaluationResults,
      recData,
      retrievals,
      booleanExprResolver,
      evaluationContext,
      exprMap,
      booleanExprCacheMap
    ) {
      formModel.fcLookup
        .get(fcId)
        .flatMap(_.includeIf)
        .map(_.booleanExpr)
    }

  private def isHiddenByRepeatsExpr(
    fcId: FormComponentId,
    evResult: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): F[Boolean] =
    formModel.fcIdRepeatsExprLookup.get(fcId).fold(false.pure[F]) { repeatsExpr =>
      val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(repeatsExpr)
      val exprResult: ExpressionResult =
        evResult.evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
      fcId.modelComponentId.maybeIndex
        .fold(false)(fcIndex => exprResult.numberRepresentation.fold(true)(fcIndex > _.intValue))
        .pure[F]
    }

  private def isHiddenByRevealingChoice(
    fcId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate]
  )(implicit formModel: FormModel[Interim]): F[Boolean] = {

    val isHidden = formModel.revealingChoiceInfo.isHiddenByParentId(fcId, recData.variadicFormData)

    isHidden.fold(false.pure[F])(x => x.pure[F])
  }
}
