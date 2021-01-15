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

import cats.Monoid
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

import java.time.LocalDate

case class EvaluationResults(
  exprMap: Map[Expr, ExpressionResult]
) {

  def +(expr: Expr, result: ExpressionResult): EvaluationResults = this.copy(exprMap = exprMap + (expr -> result))
  def ++(otherExprMap: Map[Expr, ExpressionResult]): EvaluationResults =
    this.copy(exprMap = exprMap ++ otherExprMap)

  def get(expr: Expr): Option[ExpressionResult] = exprMap.get(expr)

  import ExpressionResult._

  private def unsupportedMany(str: String)(many: VariadicValue.Many): ExpressionResult =
    ExpressionResult.invalid(s"$str - unsupported value $many")

  private def unsupportedOperation(str: String)(expr: Expr): ExpressionResult =
    Invalid(s"$str - unsupported computation. Cannot combine $str and $expr")

  private def get(
    expr: FormCtx,
    recData: RecData[SourceOrigin.OutOfDate],
    fromVariadicValue: VariadicValue => ExpressionResult
  ): ExpressionResult =
    exprMap.getOrElse(
      expr,
      recData.variadicFormData
        .get(expr.formComponentId.modelComponentId)
        .fold(ExpressionResult.empty)(fromVariadicValue))

  private def get(modelComponentId: ModelComponentId, recData: RecData[SourceOrigin.OutOfDate]): Option[VariadicValue] =
    recData.variadicFormData
      .get(modelComponentId)

  // Sum field may be hidden by AddToList or by Revealing choice
  private def isSumHidden(modelComponentId: ModelComponentId): Boolean = {
    val expr = FormCtx(modelComponentId.toFormComponentId)
    exprMap.get(expr).fold(true)(_ === Hidden)
  }

  private def calculateSum(
    formComponentId: FormComponentId,
    recData: RecData[SourceOrigin.OutOfDate],
    invalidResult: ExpressionResult
  ): ExpressionResult = {
    val maybeListToSum: Either[ExpressionResult, List[BigDecimal]] =
      recData.variadicFormData
        .forBaseComponentId(formComponentId.baseComponentId)
        .toList
        .collect {
          case (k, v) if !isSumHidden(k) => v
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
    recData: RecData[SourceOrigin.OutOfDate]
  ): ExpressionResult = {

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold(one => toNumberResult(one.value))(unsupportedMany("Number"))

    def toNumberResult(value: String): ExpressionResult =
      toBigDecimalSafe(value).fold(ExpressionResult.invalid(s"Number - cannot convert '$value' to number"))(
        NumberResult.apply)

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr)                 => loop(field1) + loop(field2)
      case Multiply(field1: Expr, field2: Expr)            => loop(field1) * loop(field2)
      case Subtraction(field1: Expr, field2: Expr)         => loop(field1) - loop(field2)
      case Else(field1: Expr, field2: Expr)                => loop(field1) orElse loop(field2)
      case ctx @ FormCtx(formComponentId: FormComponentId) => get(ctx, recData, fromVariadicValue)
      case Sum(FormCtx(formComponentId)) =>
        calculateSum(formComponentId, recData, unsupportedOperation("Number")(expr))
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

    def nonEmpty(stringResult: StringResult): ExpressionResult =
      if (stringResult.value.trim.isEmpty) Empty else stringResult

    def fromVariadicValue(variadicValue: VariadicValue): ExpressionResult =
      variadicValue.fold[ExpressionResult](one => nonEmpty(StringResult(one.value)))(many =>
        ExpressionResult.OptionResult(many.value.map(_.toInt)))

    def loop(expr: Expr): ExpressionResult = expr match {
      case Add(field1: Expr, field2: Expr)                 => loop(field1) + loop(field2)
      case Multiply(field1: Expr, field2: Expr)            => unsupportedOperation("String")(expr)
      case Subtraction(field1: Expr, field2: Expr)         => unsupportedOperation("String")(expr)
      case Else(field1: Expr, field2: Expr)                => loop(field1) orElse loop(field2)
      case ctx @ FormCtx(formComponentId: FormComponentId) => get(ctx, recData, fromVariadicValue)
      case Sum(field1: Expr)                               => unsupportedOperation("String")(expr)
      case AuthCtx(value: AuthInfo) =>
        nonEmpty(StringResult(AuthContextPrepop.values(value, evaluationContext.retrievals)))
      case UserCtx(value: UserField) =>
        nonEmpty(
          StringResult(
            UserCtxEvaluatorProcessor
              .processEvaluation(evaluationContext.retrievals, value, evaluationContext.authConfig)))
      case Constant(value: String) => nonEmpty(StringResult(value))
      case HmrcRosmRegistrationCheck(value: RosmProp) =>
        nonEmpty(StringResult(UserCtxEvaluatorProcessor.evalRosm(evaluationContext.thirdPartyData, value)))
      case Value => Empty
      case FormTemplateCtx(value: FormTemplateProp) =>
        nonEmpty {
          value match {
            case FormTemplateProp.Id                  => StringResult(evaluationContext.formTemplateId.value)
            case FormTemplateProp.SubmissionReference => StringResult(evaluationContext.submissionRef.value)
          }
        }

      case ParamCtx(queryParam) =>
        nonEmpty(StringResult(evaluationContext.thirdPartyData.queryParams(queryParam)))
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
        nonEmpty(StringResult(link.url))
    }

    loop(expr)
  }
  private def evalDateString(
    expr: Expr,
    recData: RecData[SourceOrigin.OutOfDate]
  ): ExpressionResult = {

    def loop(expr: Expr): ExpressionResult = expr match {
      case ctx @ FormCtx(formComponentId) =>
        exprMap.getOrElse(
          ctx, {
            val year = get(formComponentId.toAtomicFormComponentId(Date.year), recData)
            val month = get(formComponentId.toAtomicFormComponentId(Date.month), recData)
            val day = get(formComponentId.toAtomicFormComponentId(Date.day), recData)
            (year, month, day) match {
              case (Some(VariadicValue.One(y)), Some(VariadicValue.One(m)), Some(VariadicValue.One(d))) =>
                DateResult(LocalDate.of(y.toInt, m.toInt, d.toInt))
              case _ => Empty
            }
          }
        )
      case _ => ExpressionResult.empty
    }

    loop(expr)
  }

  def evalExprCurrent(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.Current],
    evaluationContext: EvaluationContext
  ): ExpressionResult = evalExpr(typeInfo, recData.asInstanceOf[RecData[SourceOrigin.OutOfDate]], evaluationContext)

  def evalExpr(
    typeInfo: TypeInfo,
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult =
    typeInfo.staticTypeData.exprType.fold { number =>
      evalNumber(typeInfo.expr, recData)
    } { string =>
      evalString(typeInfo.expr, recData, evaluationContext)
    } { choiceSelection =>
      evalString(typeInfo.expr, recData, evaluationContext)
    } { dateString =>
      evalDateString(typeInfo.expr, recData)
    } { illegal =>
      ExpressionResult.invalid("[evalTyped] Illegal expression " + typeInfo.expr)
    }
}

object EvaluationResults {
  val empty = EvaluationResults(Map.empty)

  def unapply(a: EvaluationResults): Option[Map[Expr, ExpressionResult]] = Some(a.exprMap)

  implicit val monoidEvaluationResults: Monoid[EvaluationResults] = new Monoid[EvaluationResults] {
    def empty = EvaluationResults.empty
    def combine(l: EvaluationResults, r: EvaluationResults): EvaluationResults = (l, r) match {
      case (EvaluationResults(em1), EvaluationResults(em2)) => EvaluationResults(em1 ++ em2)
    }
  }
}
