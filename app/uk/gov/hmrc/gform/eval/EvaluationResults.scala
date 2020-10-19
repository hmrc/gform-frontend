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

package uk.gov.hmrc.gform.eval

import cats.{ Monoid }
import cats.instances.list._
import cats.instances.either._
import cats.syntax.eq._
import cats.syntax.traverse._
import uk.gov.hmrc.gform.commons.BigDecimalUtil.toBigDecimalSafe
import uk.gov.hmrc.gform.gform.AuthContextPrepop
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.graph.processor.UserCtxEvaluatorProcessor
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicValue }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class EvaluationResults(
  exprMap: Map[TypedExpr, ExpressionResult]
) {

  def +(expr: TypedExpr, result: ExpressionResult): EvaluationResults = this.copy(exprMap = exprMap + (expr -> result))
  def ++(otherExprMap: Map[TypedExpr, ExpressionResult]): EvaluationResults =
    this.copy(exprMap = exprMap ++ otherExprMap)

  def get(typedExpr: TypedExpr): Option[ExpressionResult] = exprMap.get(typedExpr)

  import ExpressionResult._

  private def unsupportedMany(str: String)(many: VariadicValue.Many): ExpressionResult =
    ExpressionResult.invalid(s"$str - unsupported value $many")

  private def unsupportedOperation(str: String)(expr: Expr): ExpressionResult =
    Invalid(s"$str - unsupported computation. Cannot combine $str and $expr")

  private def get(
    expr: FormCtx,
    typedExpr: TypedExpr,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext,
    fromVariadicValue: VariadicValue => ExpressionResult
  ): ExpressionResult =
    exprMap
      .get(typedExpr)
      .orElse(evaluationContext.typedExpressionLookup.get(expr.formComponentId).flatMap(exprMap.get))
      .getOrElse(
        recData.variadicFormData
          .get(expr.formComponentId.modelComponentId)
          .fold(ExpressionResult.empty)(fromVariadicValue))

  // Add-to-list sum field may be hidden
  private def isSumHidden(modelComponentId: ModelComponentId, toTyped: FormCtx => TypedExpr): Boolean = {
    val typedExpr = toTyped(FormCtx(modelComponentId.toFormComponentId))
    exprMap.get(typedExpr).fold(true)(_ === Hidden)
  }

  private def calculateSum(
    formComponentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate],
    invalidResult: ExpressionResult,
    toTyped: FormCtx => TypedExpr
  ): ExpressionResult = {
    val maybeListToSum: Either[ExpressionResult, List[BigDecimal]] =
      recData.variadicFormData
        .forBaseComponentId(formComponentId.baseComponentId)
        .toList
        .collect {
          case (k, v) if !isSumHidden(k, toTyped) => v
        }
        .traverse {
          case VariadicValue.One(v) =>
            toBigDecimalSafe(v)
              .fold[Either[ExpressionResult, BigDecimal]](Left(invalidResult))(Right(_))
          case VariadicValue.Many(_) => Left(invalidResult)
        }
    maybeListToSum.map(listToSum => NumberResult(listToSum.sum)).merge
  }

  private def evalNumber(
    expr: Expr,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold(one => toNumberResult(one.value))(unsupportedMany("Number"))

    def toNumberResult(value: String): ExpressionResult =
      toBigDecimalSafe(value).fold(ExpressionResult.invalid(s"Number - cannot convert '$value' to number"))(
        NumberResult.apply)

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr)         => loop(field1) + loop(field2)
      case Multiply(field1: Expr, field2: Expr)    => loop(field1) * loop(field2)
      case Subtraction(field1: Expr, field2: Expr) => loop(field1) - loop(field2)
      case Else(field1: Expr, field2: Expr)        => loop(field1) orElse loop(field2)
      case ctx @ FormCtx(formComponentId: FormComponentId) =>
        get(ctx, TypedExpr(ctx, ExprType.Number), recData, evaluationContext, fromVariadicValue)
      case Sum(FormCtx(formComponentId)) =>
        calculateSum(formComponentId, recData, unsupportedOperation("Number")(expr), TypedExpr.number)
      case Sum(_)                                     => unsupportedOperation("Number")(expr)
      case AuthCtx(value: AuthInfo)                   => unsupportedOperation("Number")(expr)
      case UserCtx(value: UserField)                  => unsupportedOperation("Number")(expr)
      case Constant(value: String)                    => toNumberResult(value)
      case HmrcRosmRegistrationCheck(value: RosmProp) => unsupportedOperation("Number")(expr)
      case Value                                      => Empty
      case FormTemplateCtx(value: FormTemplateProp)   => unsupportedOperation("Number")(expr)
      case ParamCtx(_)                                => unsupportedOperation("Number")(expr)
      case LinkCtx(_)                                 => unsupportedOperation("Number")(expr)

    }

    loop(expr)
  }

  private def evalString(
    expr: Expr,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold[ExpressionResult](one => StringResult(one.value))(many =>
        ExpressionResult.OptionResult(many.value.map(_.toInt)))

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr)         => loop(field1) + loop(field2)
      case Multiply(field1: Expr, field2: Expr)    => unsupportedOperation("String")(expr)
      case Subtraction(field1: Expr, field2: Expr) => unsupportedOperation("String")(expr)
      case Else(field1: Expr, field2: Expr)        => loop(field1) orElse loop(field2)
      case ctx @ FormCtx(formComponentId: FormComponentId) =>
        get(ctx, TypedExpr(ctx, ExprType.String), recData, evaluationContext, fromVariadicValue)
      case Sum(field1: Expr)        => unsupportedOperation("String")(expr)
      case AuthCtx(value: AuthInfo) => StringResult(AuthContextPrepop.values(value, evaluationContext.retrievals))
      case UserCtx(value: UserField) =>
        StringResult(
          UserCtxEvaluatorProcessor
            .processEvaluation(evaluationContext.retrievals, value, evaluationContext.authConfig))
      case Constant(value: String) => StringResult(value)
      case HmrcRosmRegistrationCheck(value: RosmProp) =>
        StringResult(UserCtxEvaluatorProcessor.evalRosm(evaluationContext.thirdPartyData, value))
      case Value => Empty
      case FormTemplateCtx(value: FormTemplateProp) =>
        value match {
          case FormTemplateProp.Id                  => StringResult(evaluationContext.formTemplateId.value)
          case FormTemplateProp.SubmissionReference => StringResult(evaluationContext.submissionRef.value)
        }

      case ParamCtx(queryParam) => StringResult(evaluationContext.thirdPartyData.queryParams(queryParam))
      case LinkCtx(internalLink) =>
        val link =
          internalLink match {
            case InternalLink.PrintSummaryPdf =>
              uk.gov.hmrc.gform.gform.routes.SummaryController
                .downloadPDF(evaluationContext.formTemplateId, evaluationContext.maybeAccessCode)
            case InternalLink.PrintAcknowledgementPdf =>
              uk.gov.hmrc.gform.gform.routes.AcknowledgementController
                .downloadPDF(evaluationContext.maybeAccessCode, evaluationContext.formTemplateId)

          }
        StringResult(link.url)
    }

    loop(expr)
  }

  def evalTyped[T](
    typedExpr: TypedExpr,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext,
    componentType: Option[ComponentType]
  ): ExpressionResult = {
    val expressionResult: ExpressionResult =
      typedExpr match {
        case TypedExpr.IsNumber(expr)          => evalNumber(expr, recData, evaluationContext)
        case TypedExpr.IsString(expr)          => evalString(expr, recData, evaluationContext)
        case TypedExpr.IsChoiceSelection(expr) => evalString(expr, recData, evaluationContext)
        case TypedExpr.IsIllegal(expr)         => ExpressionResult.invalid("[evalTyped] Illegal expression " + expr)
      }
    componentType.fold(expressionResult)(expressionResult.applyComponentType) // Apply rounding if applicable
  }
}

object EvaluationResults {
  val empty = EvaluationResults(Map.empty)

  implicit val monoidEvaluationResults: Monoid[EvaluationResults] = new Monoid[EvaluationResults] {
    def empty = EvaluationResults.empty
    def combine(l: EvaluationResults, r: EvaluationResults): EvaluationResults = (l, r) match {
      case (EvaluationResults(em1), EvaluationResults(em2)) => EvaluationResults(em1 ++ em2)
    }
  }
}
