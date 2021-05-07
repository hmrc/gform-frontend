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

package uk.gov.hmrc.gform.graph

import cats.{ Monad, MonadError, Monoid }
import cats.syntax.all._
import cats.data.StateT

import scala.language.higherKinds
import scala.util.matching.Regex
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest
import uk.gov.hmrc.gform.auth.models.{ IdentifierValue, MaterialisedRetrievals }
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.eval.{ AllFormTemplateExpressions, DateExprEval, DbLookupChecker, DelegatedEnrolmentChecker, EvaluationContext, EvaluationResults, ExprMetadata, ExpressionResult, SeissEligibilityChecker, TypeInfo }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, PageModel }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.{ DependencyGraph, GraphNode }

sealed trait GraphException {
  def reportProblem: String = this match {
    case NoTopologicalOrder(fcId, graph) =>
      s"$graph is not possible to sort topologically. Violating node is $fcId. Does graph contains cycle: ${graph.findCycle}"
    case NoFormComponent(fcId, lookup) =>
      s"""No FormComponent found for $fcId. Available FormComponents: ${lookup.keys.mkString(",")}"""
  }
}

case class NoTopologicalOrder(fcId: GraphNode, graph: Graph[GraphNode, DiEdge]) extends GraphException
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
    evaluationContext: EvaluationContext
  )(implicit me: MonadError[F, E]): F[RecalculationResult] = {

    implicit val fm: FormModel[Interim] = formModel

    val formTemplateExprs: List[ExprMetadata] = AllFormTemplateExpressions(formTemplate)

    val graph: Graph[GraphNode, DiEdge] = DependencyGraph.toGraph(formModel, formTemplateExprs)

    val orderedGraph: Either[GraphException, Traversable[(Int, List[GraphNode])]] = DependencyGraph
      .constructDependencyGraph(graph)
      .leftMap(node => NoTopologicalOrder(node.toOuter, graph))

    val startState = StateT[F, RecalculationState, EvaluationResults](s => (s, EvaluationResults.empty).pure[F])

    val res: Either[GraphException, StateT[
      F,
      RecalculationState,
      (EvaluationResults, Traversable[(Int, List[GraphNode])])
    ]] =
      for {
        graphTopologicalOrder <- orderedGraph
      } yield {
        val recalc = graphTopologicalOrder.toList.reverse.foldLeft(startState) { case (state, (_, graphLayer)) =>
          recalculateGraphLayer(graphLayer, state, retrievals, RecData.fromData(data), evaluationContext)
        }

        recalc.map { evResult =>
          (evResult, graphTopologicalOrder)
        }
      }

    res match {
      case Left(graphException) => me.raiseError(error(graphException))
      case Right(fd) =>
        fd.run(new RecalculationState(EvaluationResults.empty, thirdPartyData.booleanExprCache)).map {
          case (cacheUpdate, (evaluationResults, graphTopologicalOrder)) =>
            val finalEvaluationResults =
              implicitly[Monoid[EvaluationResults]].combine(cacheUpdate.evaluationResults, evaluationResults)
            new RecalculationResult(
              finalEvaluationResults,
              GraphData(graphTopologicalOrder, graph),
              cacheUpdate.booleanExprCache,
              evaluationContext
            )

        }
    }
  }

  private def recalculateGraphLayer(
    graphLayer: List[GraphNode],
    state: StateT[F, RecalculationState, EvaluationResults],
    retrievals: MaterialisedRetrievals,
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, EvaluationResults] =
    state.flatMap { evResult =>
      graphLayer.foldMapM {

        case GraphNode.Simple(fcId) =>
          for {
            isHiddenIncludeIf <- isHiddenByIncludeIf(fcId, evResult, recData, retrievals, evaluationContext)
            isHiddenComponentIncludeIf <-
              isHiddenByComponentIncludeIf(fcId, evResult, recData, retrievals, evaluationContext)
            isHiddenRevealingChoice <- isHiddenByRevealingChoice(fcId, recData)
            isHiddenRepeatsExpr     <- isHiddenByRepeatsExpr(fcId, evResult, recData, evaluationContext)
          } yield
            if (isHiddenIncludeIf || isHiddenRevealingChoice || isHiddenComponentIncludeIf || isHiddenRepeatsExpr) {
              evResult + (FormCtx(fcId), ExpressionResult.Hidden)
            } else {
              evResult
            }

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
              .evalExpr(typeInfo, recData, evaluationContext)
              .applyTypeInfo(typeInfo)

          noStateChange(evResult + (formCtx, evResult.get(formCtx).fold(exprResult) {
            case ExpressionResult.Hidden => ExpressionResult.Hidden // If something is Hidden keep it so.
            case _                       => exprResult
          }))

        case GraphNode.Expr(expr) =>
          val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)

          val exprResult: ExpressionResult =
            evResult.evalExpr(typeInfo, recData, evaluationContext)

          noStateChange(evResult + (expr, exprResult))
      }
    }

  private def evalIncludeIf(
    booleanExpr: BooleanExpr,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, Boolean] = {

    def compare(
      expr1: Expr,
      expr2: Expr,
      f: (ExpressionResult, ExpressionResult) => Boolean
    ): StateT[F, RecalculationState, Boolean] = {
      val typeInfo1: TypeInfo = formModel.toFirstOperandTypeInfo(expr1)
      val typeInfo2: TypeInfo = formModel.toFirstOperandTypeInfo(expr2)
      val exprRes1: ExpressionResult =
        evaluationResults.evalExpr(typeInfo1, recData, evaluationContext).applyTypeInfo(typeInfo1)
      val exprRes2: ExpressionResult =
        evaluationResults.evalExpr(typeInfo2, recData, evaluationContext).applyTypeInfo(typeInfo2)
      val res = f(exprRes1, exprRes2)
      noStateChange(res)
    }

    def compareDate(
      dateExprLHS: DateExpr,
      dateExprRHS: DateExpr,
      f: (DateResult, DateResult) => Boolean
    ): StateT[F, RecalculationState, Boolean] = {
      val evalFunc: DateExpr => Option[DateResult] =
        DateExprEval.eval(formModel, recData, evaluationContext, evaluationResults)
      val exprResultLHS = evalFunc(dateExprLHS)
      val exprResultRHS = evalFunc(dateExprRHS)
      val res = (exprResultLHS, exprResultRHS) match {
        case (Some(left), Some(right)) => f(left, right)
        case _                         => false
      }
      noStateChange(res)
    }

    def matchRegex(formCtx: FormCtx, regex: Regex): Boolean = {
      val typeInfo1 = formModel.toFirstOperandTypeInfo(formCtx)
      val exprRes1: ExpressionResult =
        evaluationResults.evalExpr(typeInfo1, recData, evaluationContext).applyTypeInfo(typeInfo1)

      exprRes1.matchRegex(regex)
    }

    def loop(booleanExpr: BooleanExpr): StateT[F, RecalculationState, Boolean] = booleanExpr match {
      case Equals(field1, field2)              => compare(field1, field2, _ identical _)
      case GreaterThan(field1, field2)         => compare(field1, field2, _ > _)
      case DateAfter(field1, field2)           => compareDate(field1, field2, _ after _)
      case GreaterThanOrEquals(field1, field2) => compare(field1, field2, _ >= _)
      case LessThan(field1, field2)            => compare(field1, field2, _ < _)
      case DateBefore(field1, field2)          => compareDate(field1, field2, _ before _)
      case LessThanOrEquals(field1, field2)    => compare(field1, field2, _ <= _)
      case Not(invertedExpr)                   => loop(invertedExpr).map(!_)
      case Or(expr1, expr2)                    => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 | e2
      case And(expr1, expr2)                   => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 & e2
      case IsTrue                              => noStateChange(true)
      case IsFalse                             => noStateChange(false)
      case Contains(field1, field2)            => compare(field1, field2, _ contains _)
      case MatchRegex(formCtx, regex)          => noStateChange(matchRegex(formCtx, regex))
      case FormPhase(value)                    => noStateChange(evaluationContext.formPhase.fold(false)(_.value == value))
      case In(expr, dataSource) =>
        val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(expr)
        val expressionResult: ExpressionResult =
          evaluationResults.evalExpr(typeInfo, recData, evaluationContext).applyTypeInfo(typeInfo)
        val hc = evaluationContext.headerCarrier

        expressionResult.convertNumberToString.withStringResult(noStateChange(false)) { value =>
          StateT[F, RecalculationState, Boolean] { s =>
            val updS = s.update(evaluationResults + (expr, expressionResult))
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

            updS
              .get(dataSource, value)
              .fold(makeCall().map { res =>
                (updS.add(dataSource, value, res), res)
              })((updS, _).pure[F])
          }
        }
    }
    loop(booleanExpr)
  }

  private def evaluateIncludeIf(
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext
  )(
    includeIf: Option[IncludeIf]
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, Boolean] =
    includeIf.fold(noStateChange(false)) { includeIf =>
      for {
        b <- evalIncludeIf(includeIf.booleanExpr, evaluationResults, recData, retrievals, evaluationContext)
      } yield !b
    }

  private def isHiddenByIncludeIf(
    fcId: FormComponentId,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, Boolean] = {
    val pageLookup: Map[FormComponentId, PageModel[Interim]] = formModel.pageLookup
    evaluateIncludeIf(evaluationResults, recData, retrievals, evaluationContext) {
      pageLookup
        .get(fcId)
        .flatMap(_.getIncludeIf)
    }
  }

  private def isHiddenByComponentIncludeIf(
    fcId: FormComponentId,
    evaluationResults: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    retrievals: MaterialisedRetrievals,
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, Boolean] =
    evaluateIncludeIf(evaluationResults, recData, retrievals, evaluationContext) {
      formModel.fcLookup
        .get(fcId)
        .flatMap(_.includeIf)
    }

  private def isHiddenByRepeatsExpr(
    fcId: FormComponentId,
    evResult: EvaluationResults,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, Boolean] =
    formModel.fcIdRepeatsExprLookup.get(fcId).fold(noStateChange(false)) { repeatsExpr =>
      val typeInfo: TypeInfo = formModel.toFirstOperandTypeInfo(repeatsExpr)
      val exprResult: ExpressionResult =
        evResult.evalExpr(typeInfo, recData, evaluationContext)
      noStateChange(
        fcId.modelComponentId.maybeIndex.fold(false)(fcIndex =>
          exprResult.numberRepresentation.fold(true)(fcIndex > _.intValue())
        )
      )
    }

  private def isHiddenByRevealingChoice(
    fcId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate]
  )(implicit formModel: FormModel[Interim]): StateT[F, RecalculationState, Boolean] = {

    val isHidden = formModel.revealingChoiceInfo.isHiddenByParentId(fcId, recData.variadicFormData)

    isHidden.fold(noStateChange(false))(noStateChange)
  }

  private def noStateChange[A](a: A): StateT[F, RecalculationState, A] = StateT(s => (s, a).pure[F])
}
