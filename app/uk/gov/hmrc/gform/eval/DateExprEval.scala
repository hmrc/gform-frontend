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

import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, Empty, ListResult, TaxPeriodResult }
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin.OutOfDate
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import java.time.LocalDate
import java.time.temporal.TemporalAdjusters
import scala.util.Try

object DateExprEval {

  def eval[T <: PageMode](
    formModel: FormModel[T],
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext,
    booleanExprResolver: BooleanExprResolver,
    evaluationResults: EvaluationResults
  )(dateExpr: DateExpr): ExpressionResult =
    dateExpr match {
      case DateValueExpr(value) => DateResult(value.toLocalDate(evaluationContext.formStartDate))
      case DateFormCtxVar(formCtx) =>
        fromFormCtx(formModel, recData, evaluationResults, booleanExprResolver, evaluationContext, formCtx)
      case DateExprWithOffset(dExpr, offset) =>
        eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(dExpr) match {
          case DateResult(localDate) => DateResult(addOffset(localDate, offset))
          case otherwise             => otherwise
        }
      case HmrcTaxPeriodCtx(FormCtx(formComponentId), hmrcTaxPeriodInfo) =>
        evalHmrcTaxPeriod(formComponentId, hmrcTaxPeriodInfo, recData, evaluationContext)
      case DataRetrieveDateCtx(id, attribute) => evalDataRetrieveDate(id, attribute, evaluationContext)
      case DateIfElse(cond, field1, field2) =>
        if (booleanExprResolver.resolve(cond))
          eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field1)
        else
          eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field2)
      case DateOrElse(field1, field2) =>
        eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field1).orElse(
          eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(field2)
        )
      case d @ DateConstructExpr(_, _) =>
        evalDateConstructExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(d)
      case EarliestOf(exprs) =>
        val localDates = exprs
          .map(expr => eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(expr))
          .flatMap {
            case ListResult(xs) => xs
            case other          => List(other)
          }
          .collect { case DateResult(localDate) =>
            localDate
          }
        if (localDates.isEmpty) {
          ExpressionResult.Empty
        } else {
          DateResult(localDates.min)
        }
      case LatestOf(exprs) =>
        val localDates = exprs
          .map(expr => eval(formModel, recData, evaluationContext, booleanExprResolver, evaluationResults)(expr))
          .flatMap {
            case ListResult(xs) => xs
            case other          => List(other)
          }
          .collect { case DateResult(localDate) =>
            localDate
          }
        if (localDates.isEmpty) {
          ExpressionResult.Empty
        } else {
          DateResult(localDates.max)
        }
    }

  def evalDateConstructExpr(
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext,
    evaluationResults: EvaluationResults,
    booleanExprResolver: BooleanExprResolver
  )(
    dExpr: DateConstructExpr
  ): ExpressionResult = {
    val yearExprTypeInfo: TypeInfo = evaluationResults.typeInfoForExpr(dExpr.year, evaluationContext)
    val yearResult = evaluationResults.evalExpr(yearExprTypeInfo, recData, booleanExprResolver, evaluationContext)

    val dayMonthResult: ExpressionResult =
      evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(dExpr.dayMonth)

    val resultOpt: Option[LocalDate] = dayMonthResult match {
      case dm: DateResult =>
        yearResult.fold[Option[LocalDate]](_ => None)(_ => None)(_ => None)(d =>
          Try(LocalDate.of(d.value.toInt, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption
        )(e => Try(LocalDate.of(e.value.toInt, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption)(f =>
          Try(LocalDate.of(f.value.head.toInt, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption
        )(g => Try(LocalDate.of(g.value.getYear, dm.value.getMonthValue, dm.value.getDayOfMonth)).toOption)(_ => None)(
          _ => None
        )(_ => None)(_ => None)
      case _ => None
    }

    resultOpt.map(DateResult).getOrElse(ExpressionResult.Empty)
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
      case DateValueExpr(value) => DateResult(value.toLocalDate(evaluationContext.formStartDate))
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
        evalHmrcTaxPeriod(formComponentId, hmrcTaxPeriodInfo, recData, evaluationContext)
      case DataRetrieveDateCtx(id, attribute) => evalDataRetrieveDate(id, attribute, evaluationContext)
      case DateIfElse(cond, field1, field2) =>
        if (booleanExprResolver.resolve(cond))
          evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field1)
        else
          evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field2)
      case DateOrElse(field1, field2) =>
        evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field1).orElse(
          evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field2)
        )
      case d @ DateConstructExpr(_, _) =>
        evalDateConstructExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(d)
      case EarliestOf(exprs) =>
        val localDates = exprs
          .map(expr => evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(expr))
          .flatMap {
            case ListResult(xs) => xs
            case other          => List(other)
          }
          .collect { case DateResult(localDate) =>
            localDate
          }
        if (localDates.isEmpty) {
          ExpressionResult.Empty
        } else {
          DateResult(localDates.min)
        }
      case LatestOf(exprs) =>
        val localDates = exprs
          .map(expr => evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(expr))
          .flatMap {
            case ListResult(xs) => xs
            case other          => List(other)
          }
          .collect { case DateResult(localDate) =>
            localDate
          }
        if (localDates.isEmpty) {
          ExpressionResult.Empty
        } else {
          DateResult(localDates.max)
        }
    }

  private def evalHmrcTaxPeriod(
    formComponentId: FormComponentId,
    hmrcTaxPeriodInfo: HmrcTaxPeriodInfo,
    recData: RecData[OutOfDate],
    evaluationContext: EvaluationContext
  ): ExpressionResult = {
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
      .getOrElse(ExpressionResult.Empty)
  }

  def toDateResult(value: String): Option[DateResult] = Try(DateResult(LocalDate.parse(value))).toOption

  def evalDataRetrieveDate(
    id: DataRetrieveId,
    attribute: DataRetrieve.Attribute,
    evaluationContext: EvaluationContext
  ): ExpressionResult =
    evaluationContext.thirdPartyData.dataRetrieve
      .flatMap { dataRetrieve =>
        DataRetrieveEval
          .getDataRetrieveAttribute(dataRetrieve, DataRetrieveCtx(id, attribute))
          .flatMap {
            case s :: Nil => toDateResult(s)
            case xs       => None
          }
      }
      .getOrElse(ExpressionResult.Empty)

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
  ): ExpressionResult = {
    val typeInfo: TypeInfo = formModel.explicitTypedExpr(formCtx, formCtx.formComponentId)
    val expressionResult =
      evaluationResults.evalExpr(typeInfo, recData, booleanExprResolver, evaluationContext)

    expressionResult match {
      case TaxPeriodResult(month, year) =>
        DateResult(LocalDate.of(year, month, 1).`with`(TemporalAdjusters.lastDayOfMonth()))
      case otherwise => otherwise
    }
  }

  private def addOffset(d: LocalDate, offset: OffsetYMD): LocalDate =
    offset.offsets.foldLeft(d) { (acc, offset) =>
      offset match {
        case OffsetUnit.Year(n)  => acc.plusYears(n.toLong)
        case OffsetUnit.Month(n) => acc.plusMonths(n.toLong)
        case OffsetUnit.Day(n)   => acc.plusDays(n.toLong)
      }
    }

  private def get(modelComponentId: ModelComponentId, recData: RecData[SourceOrigin.OutOfDate]): Option[VariadicValue] =
    recData.variadicFormData
      .get(modelComponentId)
}
