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

import cats.Applicative
import cats.syntax.all._
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.eval._
import uk.gov.hmrc.gform.models.{ FormModel, Interim }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.util.matching.Regex

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
  ): F[Boolean] = compare(expr1, expr2, f).pure[F]

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
  ): F[Boolean] = compareDate(dateExprLHS, dateExprRHS, f).pure[F]

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

  def matchRegexF(expr: Expr, regex: Regex): F[Boolean] =
    matchRegex(expr, regex).pure[F]

  def matchRegex(expr: Expr, regex: Regex): Boolean = {
    val typeInfo1 = formModel.toFirstOperandTypeInfo(expr)
    val exprRes1: ExpressionResult =
      evaluationResults
        .evalExpr(typeInfo1, recData, booleanExprResolver, evaluationContext)
        .applyTypeInfo(typeInfo1)

    exprRes1.matchRegex(regex)
  }

  def compareFormPhaseF(value: FormPhaseValue): F[Boolean] =
    compareFormPhase(value).pure[F]
  def compareFormPhase(value: FormPhaseValue): Boolean = evaluationContext.formPhase.fold(false)(_.value == value)
}
