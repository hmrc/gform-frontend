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

import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, Empty }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Date, DateExpr, DateExprValue, DateExprWithOffset, DateFormCtxVar, DateIfElse, DateOrElse, DateValueExpr, ExactDateExprValue, FormComponentId, FormCtx, HmrcTaxPeriodCtx, HmrcTaxPeriodInfo, OffsetUnit, OffsetYMD, TodayDateExprValue }

import java.time.LocalDate
import java.time.temporal.TemporalAdjusters
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.{ ObligationDetail, SourceOrigin, VariadicValue }

import scala.util.Try

object DateExprEval {

  def eval[T <: PageMode](
    formModel: FormModel[T],
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext,
    booleanExprResolver: BooleanExprResolver,
    evaluationResults: EvaluationResults
  )(dateExpr: DateExpr): Option[DateResult] =
    dateExpr match {
      case DateValueExpr(value) => Some(DateResult(fromValue(value)))
      case DateFormCtxVar(formCtx) =>
        fromFormCtx(formModel, recData, evaluationResults, booleanExprResolver, evaluationContext, formCtx)
      case DateExprWithOffset(dExpr, offset) =>
        eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(dExpr).map(r =>
          DateResult(addOffset(r.value, offset))
        )
      case HmrcTaxPeriodCtx(FormCtx(formComponentId), hmrcTaxPeriodInfo) =>
        evalHmrcTaxPeriod(formComponentId, hmrcTaxPeriodInfo, recData, evaluationContext)
      case DateIfElse(cond, field1, field2) =>
        if (booleanExprResolver.resolve(cond))
          eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field1)
        else
          eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field2)
      case DateOrElse(field1, field2) =>
        eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field1).orElse(
          eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field2)
        )

    }

  def evalDateExpr(
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext,
    evaluationResults: EvaluationResults,
    booleanExprResolver: BooleanExprResolver
  )(
    dateExpr: DateExpr
  ): ExpressionResult =
    dateExpr match {
      case DateValueExpr(value) => DateResult(fromValue(value))
      case DateFormCtxVar(formCtx @ FormCtx(formComponentId)) =>
        evaluationResults.get(formCtx) match {
          case Some(Empty) | None =>
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
              case (None, Some(VariadicValue.One(m)), Some(VariadicValue.One(d))) =>
                val leapYear = 2020 //makes 29th of feb valid when we don't know the year. The year 2020 is a leap year
                Try(LocalDate.of(leapYear, m.toInt, d.toInt))
                  .fold(
                    _ => fromValue(evaluationContext, formComponentId),
                    localDate => DateResult(localDate)
                  )
              case _ => fromValue(evaluationContext, formComponentId)
            }
          case Some(value) => value
        }
      case DateExprWithOffset(dExpr, offset) =>
        val exprResult = evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(dExpr)
        exprResult.fold[ExpressionResult](identity)(_ => exprResult)(_ => exprResult)(identity)(identity)(identity)(d =>
          d.copy(value = addOffset(d.value, offset))
        )(identity)(identity)(identity)(identity)
      case HmrcTaxPeriodCtx(FormCtx(formComponentId), hmrcTaxPeriodInfo) =>
        evalHmrcTaxPeriod(formComponentId, hmrcTaxPeriodInfo, recData, evaluationContext).getOrElse(
          ExpressionResult.empty
        )
      case DateIfElse(cond, field1, field2) =>
        if (booleanExprResolver.resolve(cond))
          evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field1)
        else
          evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field2)
      case DateOrElse(field1, field2) =>
        evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field1).orElse(
          evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field2)
        )
    }

  private def evalHmrcTaxPeriod(
    formComponentId: FormComponentId,
    hmrcTaxPeriodInfo: HmrcTaxPeriodInfo,
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext
  ): Option[DateResult] = {
    val period: String = recData.variadicFormData.one(formComponentId.modelComponentId).getOrElse("")
    val maybeObligationDetail: Option[ObligationDetail] =
      evaluationContext.thirdPartyData.obligations.findByFcPeriodKey(formComponentId, period)

    maybeObligationDetail
      .map { value =>
        DateResult(
          hmrcTaxPeriodInfo match {
            case HmrcTaxPeriodInfo.PeriodTo   => value.inboundCorrespondenceToDate
            case HmrcTaxPeriodInfo.PeriodFrom => value.inboundCorrespondenceFromDate
            case HmrcTaxPeriodInfo.PeriodDue  => value.inboundCorrespondenceDueDate
          }
        )
      }
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
    booleanExprResolver: BooleanExprResolver,
    evaluationContext: EvaluationContext,
    formCtx: FormCtx
  ): Option[DateResult] = {
    val typeInfo: TypeInfo = formModel.explicitTypedExpr(formCtx, formCtx.formComponentId)
    val expressionResult =
      evaluationResults.evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)
    expressionResult.fold(_ => Option.empty[DateResult])(_ => None)(_ => None)(_ => None)(_ => None)(_ => None)(
      dateResult => Some(dateResult)
    ) { tp =>
      Some(
        DateResult(LocalDate.of(tp.year, tp.month, 1).`with`(TemporalAdjusters.lastDayOfMonth()))
      )
    }(_ => None)(_ => None)(_ => None)
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
