/*
 * Copyright 2020 HM Revenue & Customs
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
import cats.syntax.eq._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.instances.list._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.data.StateT

import scala.language.higherKinds
import scalax.collection.Graph
import scalax.collection.GraphEdge._
import shapeless.syntax.typeable._
import uk.gov.hmrc.gform.auth.UtrEligibilityRequest
import uk.gov.hmrc.gform.auth.models.{ IdentifierValue, MaterialisedRetrievals }
import uk.gov.hmrc.gform.eval.{ AllFormComponentExpressions, AllFormTemplateExpressions, DbLookupChecker, DelegatedEnrolmentChecker, EvaluationContext, EvaluationResults, ExprMetadata, ExpressionResult, SeissEligibilityChecker, TypedExpr }
import uk.gov.hmrc.gform.models.{ FormModel, Interim, PageModel }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
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
    maybeAccessCode: Option[AccessCode],
    envelopeId: EnvelopeId,
    evaluationContext: EvaluationContext
  )(implicit me: MonadError[F, E]): F[RecalculationResult] = {

    val pageLookup: Map[FormComponentId, PageModel[Interim]] = formModel.pageLookup
    val fcLookup: Map[FormComponentId, FormComponent] = formModel.fcLookup
    val fcByModelComponentIdLookup: Map[ModelComponentId, FormComponent] =
      formModel.formComponentByModelComponentIdLookup

    def noStateChange[A](a: A): StateT[F, RecalculationState, A] = StateT(s => (s, a).pure[F])
    def changeExpressionResult[A](a: A)(f: EvaluationResults => EvaluationResults): StateT[F, RecalculationState, A] =
      StateT(s => (s.update(f(s.evaluationResults)), a).pure[F])

    def evalIncludeIf(
      includeIf: IncludeIf,
      evaluationResults: EvaluationResults,
      recData: RecData[SourceOrigin.OutOfDate],
      evaluationContext: EvaluationContext
    ): StateT[F, RecalculationState, Boolean] = {

      def compare(expr1: Expr, expr2: Expr, f: (ExpressionResult, ExpressionResult) => Boolean) = {
        val typedExpr1 = formModel.toTypedExpr(expr1)
        val typedExpr2 = formModel.toTypedExpr(expr2)
        val evalExpr1 = evaluationResults.evalTyped(typedExpr1, recData, evaluationContext)
        val evalExpr2 = evaluationResults.evalTyped(typedExpr2, recData, evaluationContext)
        val res = f(evalExpr1, evalExpr2)
        changeExpressionResult(res)(evaluationResults =>
          evaluationResults + (typedExpr1, evalExpr1) + (typedExpr2, evalExpr2))
      }

      def loop(booleanExpr: BooleanExpr): StateT[F, RecalculationState, Boolean] = booleanExpr match {
        case Equals(field1, field2)              => compare(field1, field2, _ identical _)
        case GreaterThan(field1, field2)         => compare(field1, field2, _ > _)
        case GreaterThanOrEquals(field1, field2) => compare(field1, field2, _ >= _)
        case LessThan(field1, field2)            => compare(field1, field2, _ < _)
        case LessThanOrEquals(field1, field2)    => compare(field1, field2, _ <= _)
        case Not(invertedExpr)                   => loop(invertedExpr).map(!_)
        case Or(expr1, expr2)                    => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 | e2
        case And(expr1, expr2)                   => for { e1 <- loop(expr1); e2 <- loop(expr2) } yield e1 & e2
        case IsTrue                              => noStateChange(true)
        case IsFalse                             => noStateChange(false)
        case Contains(field1, field2)            => compare(field1, field2, _ contains _)
        case In(expr, dataSource) =>
          val typedExpr = formModel.toTypedExpr(expr)
          val expressionResult: ExpressionResult = evaluationResults.evalTyped(typedExpr, recData, evaluationContext)
          val hc = evaluationContext.headerCarrier

          expressionResult.withStringResult(noStateChange(false)) { value =>
            StateT[F, RecalculationState, Boolean] { s =>
              val updS = s.update(evaluationResults + (typedExpr, expressionResult))
              def makeCall() = dataSource match {
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
      loop(includeIf.booleanExpr)
    }

    def isHidden(
      fcId: FormComponentId,
      evaluationResults: EvaluationResults,
      recData: RecData[SourceOrigin.OutOfDate],
      evaluationContext: EvaluationContext
    ): StateT[F, RecalculationState, Boolean] =
      pageLookup.get(fcId).flatMap(_.getIncludeIf).fold(noStateChange(false)) { includeIf =>
        for {
          b <- evalIncludeIf(includeIf, evaluationResults, recData, evaluationContext)
        } yield {
          !b
        }
      }

    val formTemplateExprs: List[ExprMetadata] = AllFormTemplateExpressions(formTemplate)

    val graph: Graph[GraphNode, DiEdge] = DependencyGraph.toGraph(formModel, formTemplateExprs)

    val orderedGraph: Either[GraphException, Traversable[(Int, List[GraphNode])]] = DependencyGraph
      .constructDependencyGraph(graph)
      .leftMap(node => NoTopologicalOrder(node.toOuter, graph))

    type Context = (StateT[F, RecalculationState, EvaluationResults], RecData[SourceOrigin.OutOfDate])
    def recalculateGraphLayer(
      graphLayer: List[GraphNode],
      ctx: Context,
      evaluationContext: EvaluationContext
    ): Either[GraphException, Context] = {

      val (evResultF: StateT[F, RecalculationState, EvaluationResults], recData: RecData[SourceOrigin.OutOfDate]) = ctx

      val sums: List[Sum] = evaluationContext.typedExpressionLookup.values.flatMap(_.expr.sums).toList
      val isSum: Set[FormComponentId] = sums.collect {
        case Sum(FormCtx(formComponentId)) => formComponentId
      }.toSet

      val evaluationResults: StateT[F, RecalculationState, EvaluationResults] = graphLayer.foldMapM {
        case GraphNode.Simple(fcId) =>
          for {
            evResult <- evResultF
            isH      <- isHidden(fcId, evResult, recData, evaluationContext)
          } yield {
            if (isH) {
              val typedExpr = formModel.explicitTypedExpr(FormCtx(fcId), fcId)
              evResult + (typedExpr, ExpressionResult.Hidden)
            } else {
              fcLookup.get(fcId).fold(evResult) {
                case AllFormComponentExpressions(exprsMetadata) =>
                  exprsMetadata.foldMap(_.toEvaluationResults(evResult, formModel, recData, evaluationContext))
                case _ => evResult
              }
            }
          }

        case GraphNode.Expr(expr) =>
          for {
            evResult <- evResultF
          } yield {
            val sumsToAdd: List[FormComponentId] =
              expr.leafs
                .collect {
                  case FormCtx(formComponentId) if isSum(formComponentId) => formComponentId
                }

            val sumResults: Map[TypedExpr, ExpressionResult] = sumsToAdd
              .map { formComponentId =>
                val sumIds = recData.variadicFormData.forBaseComponentId(formComponentId.baseComponentId)

                sumIds.map {
                  case (k, v) =>
                    val typedExpr = formModel.toTypedExpr(FormCtx(k.toFormComponentId))
                    val exprResult = evResult.evalTyped(typedExpr, recData, evaluationContext)
                    (typedExpr, exprResult)
                }.toMap
              }
              .foldLeft(Map.empty[TypedExpr, ExpressionResult])(_ ++ _)

            val typedExpr = formModel.toTypedExpr(expr)
            val exprResult: ExpressionResult = evResult.evalTyped(typedExpr, recData, evaluationContext)
            evResult ++ sumResults + (typedExpr, exprResult)
          }

      }
      val res: Either[GraphException, Context] = Right((evaluationResults, recData))
      res
    }

    val dataCapitalised: VariadicFormData[SourceOrigin.OutOfDate] = data.mapValues {
      case (fcId, value) =>
        fcByModelComponentIdLookup.get(fcId) match {
          case Some(fc) =>
            fc match {
              case IsCapitalised() => value.map(_.toUpperCase())
              case _               => value
            }
          case _ => value
        }
    }

    val contextE: Either[GraphException, Context] =
      Right(
        (
          StateT[F, RecalculationState, EvaluationResults](s => (s, EvaluationResults.empty).pure[F]),
          RecData.fromData(dataCapitalised)))

    val res: Either[
      GraphException,
      StateT[F, RecalculationState, (EvaluationResults, Traversable[(Int, List[GraphNode])])]] =
      for {
        graphTopologicalOrder <- orderedGraph
        recalc <- graphTopologicalOrder.foldRight(contextE) {
                   case ((_, graphLayer), contextF) =>
                     for {
                       context    <- contextF
                       updatedCtx <- recalculateGraphLayer(graphLayer, context, evaluationContext)
                     } yield updatedCtx
                 }
      } yield {

        val (
          evResultF: StateT[F, RecalculationState, EvaluationResults],
          recalculatedData: RecData[SourceOrigin.OutOfDate]) =
          recalc

        evResultF.map { evResult =>
          val finalEvResult: EvaluationResults =
            formModel.exprsMetadata ++ formTemplateExprs match {
              case Nil => evResult
              case xs  => xs.foldMap(_.toEvaluationResults(evResult, formModel, recalculatedData, evaluationContext))
            }
          (finalEvResult, graphTopologicalOrder)
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
              cacheUpdate.booleanExprCache)
        }
    }
  }
}
