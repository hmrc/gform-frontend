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

import cats.Applicative
import cats.syntax.all._
import cats.data.StateT

import scala.language.higherKinds
import scala.util.matching.Regex
import uk.gov.hmrc.gform.eval.{ BooleanExprResolver, DateExprEval, EvaluationContext, EvaluationResults, ExpressionResult, TypeInfo }
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.models.{ FormModel, Interim }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class RecalculationResolver[F[_]: Applicative](
  formModel: FormModel[Interim],
  evaluationResults: EvaluationResults,
  recData: RecData[SourceOrigin.OutOfDate],
  booleanExprResolver: BooleanExprResolver,
  evaluationContext: EvaluationContext
) {
  def compareF(
    expr1: Expr,
    expr2: Expr,
    f: (ExpressionResult, ExpressionResult) => Boolean
  ): StateT[F, RecalculationState, Boolean] = noStateChange(compare(expr1, expr2, f))

  def compare(
    expr1: Expr,
    expr2: Expr,
    f: (ExpressionResult, ExpressionResult) => Boolean
  ): Boolean = {
    val typeInfo1: TypeInfo = formModel.toFirstOperandTypeInfo(expr1)
    val typeInfo2: TypeInfo = formModel.toFirstOperandTypeInfo(expr2)
    val exprRes1: ExpressionResult =
      evaluationResults
        .evalExpr(typeInfo1, recData, booleanExprResolver, evaluationContext)
        .applyTypeInfo(typeInfo1)
    val exprRes2: ExpressionResult =
      evaluationResults
        .evalExpr(typeInfo2, recData, booleanExprResolver, evaluationContext)
        .applyTypeInfo(typeInfo2)
    val res: Boolean = f(exprRes1, exprRes2)
    res
  }

  def compareDateF(
    dateExprLHS: DateExpr,
    dateExprRHS: DateExpr,
    f: (DateResult, DateResult) => Boolean
  ): StateT[F, RecalculationState, Boolean] = noStateChange(compareDate(dateExprLHS, dateExprRHS, f))

  def compareDate(
    dateExprLHS: DateExpr,
    dateExprRHS: DateExpr,
    f: (DateResult, DateResult) => Boolean
  ): Boolean = {
    val evalFunc: DateExpr => Option[DateResult] =
      DateExprEval.eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)
    val exprResultLHS = evalFunc(dateExprLHS)
    val exprResultRHS = evalFunc(dateExprRHS)
    val res = (exprResultLHS, exprResultRHS) match {
      case (Some(left), Some(right)) => f(left, right)
      case _                         => false
    }
    res
  }

  def matchRegexF(formCtx: FormCtx, regex: Regex): StateT[F, RecalculationState, Boolean] = noStateChange(
    matchRegex(formCtx, regex)
  )

  def matchRegex(formCtx: FormCtx, regex: Regex): Boolean = {
    val typeInfo1 = formModel.toFirstOperandTypeInfo(formCtx)
    val exprRes1: ExpressionResult =
      evaluationResults
        .evalExpr(typeInfo1, recData, booleanExprResolver, evaluationContext)
        .applyTypeInfo(typeInfo1)

    exprRes1.matchRegex(regex)
  }

  def compareFormPhaseF(value: FormPhaseValue): StateT[F, RecalculationState, Boolean] = noStateChange(
    compareFormPhase(value)
  )
  def compareFormPhase(value: FormPhaseValue): Boolean = evaluationContext.formPhase.fold(false)(_.value == value)

  private def noStateChange[A](a: A): StateT[F, RecalculationState, A] = StateT(s => (s, a).pure[F])
}
