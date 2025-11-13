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

package uk.gov.hmrc.gform.eval

import cats.implicits.catsSyntaxEq
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ids.ModelComponentId.{ Atomic, Pure }
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListRef, And, BooleanExpr, Contains, DateAfter, DateBefore, DateExpr, DuplicateExists, Equals, First, FormComponentId, FormCtx, FormPhase, GreaterThan, GreaterThanOrEquals, HasAnswer, In, IsFalse, IsLogin, IsTrue, LessThan, LessThanOrEquals, LoginInfo, MatchRegex, Not, Or }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate

/** Evaluates Boolean expressions in context where they do not participate to overall FormModel.
  * For example Boolean expressions from validIf expressions.
  */
class BooleanExprEval {

  def eval[D <: DataOrigin](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    booleanExprCache: BooleanExprCache
  )(
    booleanExpr: BooleanExpr
  ): Boolean = {
    def loop(booleanExpr: BooleanExpr): Boolean = booleanExpr match {
      case Equals(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult

        l.identical(r)

      case GreaterThan(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult
        (l > r)

      case DateAfter(left, right) =>
        compare(left, right, _ after _, formModelVisibilityOptics)

      case GreaterThanOrEquals(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult
        (l >= r)

      case LessThan(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult
        (l < r)

      case DateBefore(left, right) =>
        compare(left, right, _ before _, formModelVisibilityOptics)

      case LessThanOrEquals(left, right) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(left).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(right).expressionResult

        (l <= r)

      case Not(booleanExpr)                => !loop(booleanExpr)
      case Or(booleanExpr1, booleanExpr2)  => loop(booleanExpr1) | loop(booleanExpr2)
      case And(booleanExpr1, booleanExpr2) => loop(booleanExpr1) & loop(booleanExpr2)
      case IsTrue                          => true
      case IsFalse                         => false
      case Contains(ctx, expr) =>
        val l = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(ctx).expressionResult
        val r = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).expressionResult

        l.contains(r)

      case in: In =>
        RefreshBooleanExprCacheService.evalInExpr(in, booleanExprCache, formModelVisibilityOptics.recData)

      case h @ HasAnswer(_, _) =>
        val formModel = formModelVisibilityOptics.formModel
        val recalculationResult = formModelVisibilityOptics.recalculationResult
        val evaluationResult = recalculationResult.evaluationResults
        val evaluationContext = recalculationResult.evaluationContext
        val recData = formModelVisibilityOptics.recData
        BooleanExprEval
          .evalHasAnswer(
            h,
            formModel,
            evaluationResult,
            evaluationContext,
            formModelVisibilityOptics.booleanExprResolver,
            recData
          )

      case DuplicateExists(fieldList: Seq[FormCtx]) =>
        val recData = formModelVisibilityOptics.recData
        BooleanExprEval
          .evalDuplicateExpr(fieldList, recData)

      case MatchRegex(expr, regex) =>
        val expressionResult: ExpressionResult =
          formModelVisibilityOptics.evalAndApplyTypeInfoFirst(expr).expressionResult
        expressionResult.matchRegex(regex)

      case First(FormCtx(formComponentId)) =>
        BooleanExprEval
          .evalFirstExpr(
            formComponentId
          )

      case FormPhase(_) =>
        false

      case IsLogin(_) =>
        false
    }

    loop(booleanExpr)
  }

  private def compare[D <: DataOrigin](
    left: DateExpr,
    right: DateExpr,
    f: (DateResult, DateResult) => Boolean,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ) = {
    val evalFunc: DateExpr => ExpressionResult = DateExprEval
      .eval(
        formModelVisibilityOptics.formModel,
        formModelVisibilityOptics.recData.asInstanceOf[RecData[OutOfDate]],
        formModelVisibilityOptics.recalculationResult.evaluationContext,
        formModelVisibilityOptics.booleanExprResolver,
        formModelVisibilityOptics.evaluationResults
      )
    val l = evalFunc(left)
    val r = evalFunc(right)
    ((l, r) match {
      case (dr1 @ DateResult(_), dr2 @ DateResult(_)) => f(dr1, dr2)
      case _                                          => false
    })
  }
}

object BooleanExprEval {
  def evalDuplicateExpr[T <: PageMode, R <: SourceOrigin](
    fields: Seq[FormCtx],
    recData: RecData[R]
  ): Boolean = {

    val filtered =
      fields.flatMap(f => recData.variadicFormData.forBaseComponentIdLessThenEqual(f.formComponentId.modelComponentId))
    val compare = filtered
      .map {
        case (Pure(component), variadicValue) =>
          (
            component.maybeIndex,
            component.baseComponentId.value,
            None,
            variadicValue.toSeq.map(CanonicalString(_, component.baseComponentId))
          )
        case (Atomic(component, atom), variadicValue) =>
          (
            component.maybeIndex,
            component.baseComponentId.value,
            Some(atom.value),
            variadicValue.toSeq.map(CanonicalString(_, component.baseComponentId))
          )
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map { case (_, baseComponentId, atom, value) => (baseComponentId, atom, value) }.toSet)
      .values

    compare.toList.size =!= compare.toSet.size
  }

  def evalHasAnswer[T <: PageMode](
    hasAnswer: HasAnswer,
    formModel: FormModel[T],
    evaluationResults: EvaluationResults,
    evaluationContext: EvaluationContext,
    booleanExprResolver: BooleanExprResolver,
    recData: RecData[SourceOrigin.Current]
  ): Boolean =
    hasAnswer.addToListRef match {
      case AddToListRef.Basic(formCtx) => false
      case AddToListRef.Expanded(formCtxs) =>
        val typeInfo = formModel.toFirstOperandTypeInfo(hasAnswer.formCtx)
        val expressionResult = evaluationResults
          .evalExprCurrent(typeInfo, recData, booleanExprResolver, evaluationContext)

        formCtxs.exists { formCtx =>
          val atlTypeInfo = formModel.toFirstOperandTypeInfo(formCtx)
          val altExpressionResult = evaluationResults
            .evalExprCurrent(atlTypeInfo, recData, booleanExprResolver, evaluationContext)

          altExpressionResult.hasAnswer(expressionResult)
        }
    }

  def evalFirstExpr[T <: PageMode](
    formComponentId: FormComponentId
  ): Boolean =
    formComponentId.modelComponentId.indexedComponentId.fold(pure => false)(indexed => indexed.index === 1)

  def evalIsLoginExpr[T <: PageMode](
    loginInfo: LoginInfo,
    retrievals: MaterialisedRetrievals
  ): Boolean =
    loginInfo match {
      case LoginInfo.GGLogin    => retrievals.maybeGovermentGatewayId.isDefined
      case LoginInfo.EmailLogin => retrievals.maybeEmailId.isDefined
      case _                    => false
    }

}
