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

package uk.gov.hmrc.gform.eval

import cats.Monad
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.applicative._
import scala.language.higherKinds
import uk.gov.hmrc.gform.graph.{ RecData, RecalculationResult }
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ And, BooleanExpr, Contains, DateAfter, DateBefore, Equals, GreaterThan, GreaterThanOrEquals, In, IsFalse, IsTrue, LessThan, LessThanOrEquals, Not, Or }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }

/**
  * Evaluates Boolean expressions in context where they do not participate to overall FormModel.
  * For example Boolean expressions from validIf expressions.
  */
class BooleanExprEval[F[_]: Monad] {

  def eval[D <: DataOrigin](
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(
    booleanExpr: BooleanExpr
  ): F[Boolean] = {
    def loop(booleanExpr: BooleanExpr): F[Boolean] = booleanExpr match {
      case Equals(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult

        l.identical(r).pure[F]

      case GreaterThan(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult
        (l > r).pure[F]

      case DateAfter(_, _) =>
        false.pure[F]

      case GreaterThanOrEquals(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult
        (l >= r).pure[F]

      case LessThan(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult
        (l < r).pure[F]

      case DateBefore(_, _) =>
        false.pure[F]

      case LessThanOrEquals(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult

        (l <= r).pure[F]

      case Not(booleanExpr) => loop(booleanExpr).map(b => !b)
      case Or(booleanExpr1, booleanExpr2) =>
        for {
          e1 <- loop(booleanExpr1)
          e2 <- loop(booleanExpr2)
        } yield e1 | e2
      case And(booleanExpr1, booleanExpr2) =>
        for {
          e1 <- loop(booleanExpr1)
          e2 <- loop(booleanExpr2)
        } yield e1 & e2
      case IsTrue  => true.pure[F]
      case IsFalse => false.pure[F]
      case Contains(ctx, expr) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(ctx).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).expressionResult

        l.contains(r).pure[F]

      case in @ In(_, _) =>
        val formModel = formModelVisibilityOptics.formModel
        val recalculationResult = formModelVisibilityOptics.recalculationResult
        val recData = formModelVisibilityOptics.recData
        // TODO this doesn't work for BooleanExpr from ValidIf yet (they will need to be evaluated in Recalculation.scala).
        BooleanExprEval.evalInExpr(in, formModel, recalculationResult, recData).pure[F]
    }

    loop(booleanExpr)
  }
}

object BooleanExprEval {

  def evalInExpr[T <: PageMode](
    in: In,
    formModel: FormModel[T],
    recalculationResult: RecalculationResult,
    recData: RecData[SourceOrigin.Current]
  ): Boolean = {
    val typeInfo = formModel.toFirstOperandTypeInfo(in.value)
    val expressionResult = recalculationResult.evaluationResults
      .evalExprCurrent(typeInfo, recData, recalculationResult.evaluationContext)
    val maybeBoolean =
      recalculationResult.booleanExprCache.get(in.dataSource, expressionResult.stringRepresentation(typeInfo))
    maybeBoolean.getOrElse(false)
  }
}
