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
import play.api.i18n.Messages
import scalax.collection.edges.DiEdge
import scalax.collection.immutable.Graph
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.{ FormModel, Interim, PageModel }
import uk.gov.hmrc.gform.sharedmodel.BooleanExprCache
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData, VariadicValue }
import scala.collection.mutable

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

object Recalculation {

  def recalculateFormDataNew(
    data: VariadicFormData[SourceOrigin.OutOfDate],
    formModel: FormModel[Interim],
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    messages: Messages,
    booleanExprCache: BooleanExprCache
  ): RecalculationResult = {

    implicit val fm: FormModel[Interim] = formModel

    val formTemplateExprs: Set[ExprMetadata] = AllFormTemplateExpressions(formTemplate)

    val (graph, ins): (Graph[GraphNode, DiEdge[GraphNode]], Set[In]) =
      DependencyGraph.toGraph(formModel, formTemplateExprs)

    val orderedGraph: Either[NoTopologicalOrder, Iterable[(Int, List[GraphNode])]] = DependencyGraph
      .constructDependencyGraph(graph)
      .leftMap(node => NoTopologicalOrder(node.outer, graph))

    val exprMap = mutable.Map[Expr, ExpressionResult]()
    val formDataMap = mutable.Map.newBuilder.addAll(data.data).result()

    val startEvResults = EvaluationResults(
      exprMap,
      SourceOrigin.changeSource(RecData.fromData(VariadicFormData(formDataMap))),
      formTemplate.formKind.repeatedComponentsDetails
    )

    val res: Either[GraphException, Iterable[(Int, List[GraphNode])]] =
      for {
        graphTopologicalOrder <- orderedGraph
      } yield {
        graphTopologicalOrder.toList.reverse.foreach { case (_, graphLayer) =>
          recalculateGraphLayer(
            graphLayer,
            startEvResults,
            retrievals,
            evaluationContext,
            messages,
            exprMap,
            formDataMap,
            booleanExprCache
          )
        }

        graphTopologicalOrder
      }

    res match {
      case Left(graphException) => throw new IllegalArgumentException(graphException.reportProblem)
      case Right(graphTopologicalOrder) =>
        RecalculationResult(
          startEvResults,
          GraphData(graphTopologicalOrder, graph),
          evaluationContext
        )
    }
  }

  private def recalculateGraphLayer(
    graphLayer: List[GraphNode],
    evResult: EvaluationResults,
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    messages: Messages,
    exprMap: mutable.Map[Expr, ExpressionResult],
    variadicFormDataMap: mutable.Map[ModelComponentId, VariadicValue],
    booleanExprCache: BooleanExprCache
  )(implicit formModel: FormModel[Interim]): Unit = {
    val recData = SourceOrigin.changeSourceToOutOfDate(evResult.recData)

    val booleanExprResolver = BooleanExprResolver { booleanExpr =>
      evalBooleanExpr(booleanExpr, evResult, recData, retrievals, evaluationContext, booleanExprCache)
    }

    def evalExpr(expr: Expr) = {
      val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)
      evResult
        .evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
        .applyTypeInfo(typeInfo)
        .stringRepresentation(typeInfo, messages)
    }

    graphLayer.foreach {

      case GraphNode.Simple(fcId) =>
        val fc: Option[FormComponent] = formModel.fcLookup.get(fcId)
        val isOptionHidden = fc.exists {
          case IsChoice(_) | IsRevealingChoice(_) =>
            val userResponse: Seq[String] = recData.variadicFormData.many(fcId.modelComponentId).toSeq.flatten
            val optionData: List[OptionData] = fc
              .collect {
                case IsChoice(c) =>
                  c.options.toList.collect {
                    case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.StringBased(value), _, _)
                        if userResponse.contains(value) =>
                      o
                    case o @ OptionData.ValueBased(_, _, _, _, OptionDataValue.ExprBased(expr), _, _)
                        if userResponse.contains(evalExpr(expr)) =>
                      o
                    case o: OptionData.IndexBased if userResponse.contains(o.toString) => o
                  }
                case IsRevealingChoice(rc) => rc.options.map(_.choice)
              }
              .getOrElse(List.empty[OptionData])

            val includeIfs: List[IncludeIf] = optionData.collect {
              case OptionData.ValueBased(_, _, Some(includeIf), _, _, _, _) => includeIf
              case OptionData.IndexBased(_, _, Some(includeIf), _, _)       => includeIf
            }

            val isHidden = includeIfs
              .map(i => booleanExprResolver.resolve(i.booleanExpr))
              .reduceOption(_ && _)
              .getOrElse(true)
            !isHidden
          case _ => false
        }

        if (isOptionHidden) {
          variadicFormDataMap.remove(fcId.modelComponentId)
        }

        def hiddenIncludeIf = isHiddenByIncludeIf(
          fcId,
          evResult,
          recData,
          retrievals,
          evaluationContext,
          booleanExprCache
        )

        def hiddenComponentIncludeIf = isHiddenByComponentIncludeIf(
          fcId,
          evResult,
          recData,
          retrievals,
          evaluationContext,
          booleanExprCache
        )

        def hiddenRevealingChoice = isHiddenByRevealingChoice(fcId, recData)

        def hiddenRepeatsExpr = isHiddenByRepeatsExpr(fcId, evResult, recData, booleanExprResolver, evaluationContext)

        if (hiddenIncludeIf || hiddenRevealingChoice || hiddenComponentIncludeIf || hiddenRepeatsExpr) {
          exprMap.addOne((FormCtx(fcId), ExpressionResult.Hidden))
        } else ()

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

      case GraphNode.Expr(expr) =>
        val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)

        val exprResult: ExpressionResult =
          evResult.evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)

        exprMap.addOne((expr, exprResult))
    }

    // We are only interested in `ValidIf` with `In` expression and any other `validIf` is being ignored
    evalValidIfs(
      evResult,
      recData,
      retrievals,
      evaluationContext,
      booleanExprCache
    )

  }

  private def evalBooleanExpr(
    booleanExpr: BooleanExpr,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    booleanExprCache: BooleanExprCache
  )(implicit formModel: FormModel[Interim]): Boolean = {

    val booleanExprResolver = BooleanExprResolver { booleanExpr =>
      evalBooleanExpr(booleanExpr, evaluationResults, recData, retrievals, evaluationContext, booleanExprCache)
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
      case DuplicateExists(fieldList)          => BooleanExprEval.evalDuplicateExpr(fieldList, recData)
      case in: In                              => RefreshBooleanExprCacheService.evalInExpr(in, booleanExprCache, recData)
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
      case First(FormCtx(formComponentId)) => BooleanExprEval.evalFirstExpr(formComponentId)
      case IsLogin(value)                  => BooleanExprEval.evalIsLoginExpr(value, retrievals)
    }

    loop(booleanExpr)
  }

  private def evalBooleanExpr(
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    booleanExprCache: BooleanExprCache
  )(
    booleanExpr: Option[BooleanExpr]
  )(implicit formModel: FormModel[Interim]): Boolean =
    booleanExpr.fold(false) { booleanExpr =>
      !evalBooleanExpr(
        booleanExpr,
        evaluationResults,
        recData,
        retrievals,
        evaluationContext,
        booleanExprCache
      )
    }

  private def evalValidIfs(
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    booleanExprCache: BooleanExprCache
  )(implicit formModel: FormModel[Interim]): Unit = {
    val validIfs: List[ValidIf] = formModel.allValidIfs.flatMap(_._1)

    validIfs
      .foreach(validIf =>
        evalBooleanExpr(
          evaluationResults,
          recData,
          retrievals,
          evaluationContext,
          booleanExprCache
        ) {
          Some(validIf.booleanExpr)
        }
      )
  }

  private def isHiddenByIncludeIf(
    fcId: FormComponentId,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext,
    booleanExprCache: BooleanExprCache
  )(implicit formModel: FormModel[Interim]): Boolean = {
    val pageLookup: Map[FormComponentId, PageModel[Interim]] = formModel.pageLookup
    evalBooleanExpr(
      evaluationResults,
      recData,
      retrievals,
      evaluationContext,
      booleanExprCache
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
    evaluationContext: EvaluationContext,
    booleanExprCache: BooleanExprCache
  )(implicit formModel: FormModel[Interim]): Boolean =
    evalBooleanExpr(
      evaluationResults,
      recData,
      retrievals,
      evaluationContext,
      booleanExprCache
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
  )(implicit formModel: FormModel[Interim]): Boolean =
    formModel.fcIdRepeatsExprLookup.get(fcId).fold(false) { repeatsExpr =>
      val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(repeatsExpr)
      val exprResult: ExpressionResult =
        evResult.evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
      fcId.modelComponentId.maybeIndex
        .fold(false)(fcIndex => exprResult.numberRepresentation.fold(true)(fcIndex > _.intValue))
    }

  private def isHiddenByRevealingChoice(
    fcId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate]
  )(implicit formModel: FormModel[Interim]): Boolean = {

    val isHidden = formModel.revealingChoiceInfo.isHiddenByParentId(fcId, recData.variadicFormData)

    isHidden.getOrElse(false)
  }
}
