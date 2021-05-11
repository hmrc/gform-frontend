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

import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Date, DateExpr, DateExprValue, DateExprWithOffset, DateFormCtxVar, DateValueExpr, ExactDateExprValue, FormComponentId, FormCtx, OffsetUnit, OffsetYMD, TodayDateExprValue }
import java.time.LocalDate

import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicValue }

import scala.util.Try

object DateExprEval {

  def eval[T <: PageMode](
    formModel: FormModel[T],
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext,
    evaluationResults: EvaluationResults
  )(dateExpr: DateExpr): Option[DateResult] =
    dateExpr match {
      case DateValueExpr(value) => Some(DateResult(fromValue(value)))
      case DateFormCtxVar(formCtx) =>
        fromFormCtx(formModel, recData, evaluationResults, evaluationContext, formCtx)
      case DateExprWithOffset(dExpr, offset) =>
        eval(formModel, recData, evaluationContext, evaluationResults)(dExpr).map(r =>
          DateResult(addOffset(r.value, offset))
        )
    }

  def evalDateExpr(
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext,
    evaluationResults: EvaluationResults
  )(
    dateExpr: DateExpr
  ): ExpressionResult =
    dateExpr match {
      case DateValueExpr(value) => DateResult(fromValue(value))
      case DateFormCtxVar(formCtx @ FormCtx(formComponentId)) =>
        evaluationResults
          .get(formCtx)
          .getOrElse {
            val year = get(formComponentId.toAtomicFormComponentId(Date.year), recData)
            val month = get(formComponentId.toAtomicFormComponentId(Date.month), recData)
            val day = get(formComponentId.toAtomicFormComponentId(Date.day), recData)
            (year, month, day) match {
              case (Some(VariadicValue.One(y)), Some(VariadicValue.One(m)), Some(VariadicValue.One(d))) =>
                Try(LocalDate.of(y.toInt, m.toInt, d.toInt))
                  .fold(
                    _ => fromValue(evaluationContext, formComponentId),
                    localDate => DateResult(localDate)
                  )
              case _ => fromValue(evaluationContext, formComponentId)
            }
          }
      case DateExprWithOffset(dExpr, offset) =>
        val exprResult = evalDateExpr(recData, evaluationContext, evaluationResults)(dExpr)
        exprResult.fold[ExpressionResult](identity)(_ => exprResult)(_ => exprResult)(identity)(identity)(identity)(d =>
          d.copy(value = addOffset(d.value, offset))
        )(identity)
    }

  // for "submitMode": "summaryinfoonly" fields, since they don't exist in form data.
  private def fromValue(evaluationContext: EvaluationContext, formComponentId: FormComponentId): ExpressionResult =
    evaluationContext.dateLookup
      .get(formComponentId.modelComponentId)
      .fold(ExpressionResult.empty)(value => DateResult(value.toLocalDate))

  private def fromFormCtx[T <: PageMode](
    formModel: FormModel[T],
    recData: RecData[OutOfDate],
    evaluationResults: EvaluationResults,
    evaluationContext: EvaluationContext,
    formCtx: FormCtx
  ): Option[DateResult] = {
    val typeInfo: TypeInfo = formModel.explicitTypedExpr(formCtx, formCtx.formComponentId)
    val expressionResult =
      evaluationResults.evalExpr(typeInfo, recData, evaluationContext)
    expressionResult.fold(_ => Option.empty[DateResult])(_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(
      dateResult => Some(dateResult)
    )(_ => None)
  }

  private def addOffset(d: LocalDate, offset: OffsetYMD): LocalDate =
    offset.offsets.foldLeft(d) { (acc, offset) =>
      offset match {
        case OffsetUnit.Year(n)  => acc.plusYears(n.toLong)
        case OffsetUnit.Month(n) => acc.plusMonths(n.toLong)
        case OffsetUnit.Day(n)   => acc.plusDays(n.toLong)
      }
    }

  private def fromValue(value: DateExprValue) =
    value match {
      case TodayDateExprValue                   => LocalDate.now()
      case ExactDateExprValue(year, month, day) => LocalDate.of(year, month, day)
    }

  private def get(modelComponentId: ModelComponentId, recData: RecData[SourceOrigin.OutOfDate]): Option[VariadicValue] =
    recData.variadicFormData
      .get(modelComponentId)
}
