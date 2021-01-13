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

import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.eval.ExpressionResult.DateResult
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.{ FormModel, PageMode }
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DateExpr, DateExprValue, DateExprWithOffset, DateFormCtxVar, DateValueExpr, ExactDateExprValue, FormComponentId, FormCtx, OffsetUnit, OffsetUnitDay, OffsetUnitMonth, OffsetUnitYear, TodayDateExprValue }

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.{ Failure, Success, Try }

object DateExprEval {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val DATE_FORMAT = DateTimeFormatter.ofPattern("yyyyMMdd")

  def eval[T <: PageMode, U <: SourceOrigin](
    formModel: FormModel[T],
    recData: RecData[U],
    evaluationContext: EvaluationContext,
    evaluationResults: EvaluationResults)(dateExpr: DateExpr): Option[DateResult] =
    dateExpr match {
      case DateValueExpr(value) => Some(DateResult(fromValue(value)))
      case DateFormCtxVar(formComponentId) =>
        fromFormCtx(formModel, recData, evaluationResults, evaluationContext, formComponentId)
      case DateExprWithOffset(dExpr, offset, offsetUnit) =>
        eval(formModel, recData, evaluationContext, evaluationResults)(dExpr).map(r =>
          DateResult(addOffset(r.value, offset, offsetUnit)))
    }

  private def fromFormCtx[T <: PageMode, U <: SourceOrigin](
    formModel: FormModel[T],
    recData: RecData[U],
    evaluationResults: EvaluationResults,
    evaluationContext: EvaluationContext,
    formComponentId: FormComponentId): Option[DateResult] = {
    val typeInfo: TypeInfo = formModel.explicitTypedExpr(FormCtx(formComponentId), formComponentId)
    val expressionResult =
      evaluationResults.evalExpr(typeInfo, recData.asInstanceOf[RecData[SourceOrigin.OutOfDate]], evaluationContext)
    expressionResult.fold(_ => Option.empty[DateResult])(_ => None)(_ => None)(_ => None)(dateStr => {
      Try(LocalDate.parse(dateStr.value, DATE_FORMAT)) match {
        case Success(parseDate) =>
          Option(DateResult(parseDate))
        case Failure(exception) =>
          logger.warn(s"Failed to parse date [fcId=$formComponentId, value=${dateStr.value}", exception)
          None
      }
    })(_ => None)(_ => None)
  }

  private def addOffset(d: LocalDate, offset: Int, offsetUnit: OffsetUnit) =
    offsetUnit match {
      case OffsetUnitDay   => d.plusDays(offset.toLong)
      case OffsetUnitYear  => d.plusYears(offset.toLong)
      case OffsetUnitMonth => d.plusMonths(offset.toLong)
    }

  private def fromValue(value: DateExprValue) =
    value match {
      case TodayDateExprValue                   => LocalDate.now()
      case ExactDateExprValue(year, month, day) => LocalDate.of(year, month, day)
    }
}
