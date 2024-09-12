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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import java.time.LocalDate
import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.eval.{ BooleanExprResolver, EvaluationContext, EvaluationResults }
import uk.gov.hmrc.gform.eval.DateExprEval.evalDateExpr
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId, SourceOrigin }

sealed trait DateExpr {
  def leafExprs: List[Expr] = this match {
    case DateValueExpr(_)              => DateCtx(this) :: Nil
    case DateFormCtxVar(formCtx)       => formCtx :: Nil
    case DateExprWithOffset(dExpr, _)  => dExpr.leafExprs
    case HmrcTaxPeriodCtx(formCtx, _)  => formCtx :: Nil
    case DataRetrieveDateCtx(_, _)     => DateCtx(this) :: Nil
    case DateIfElse(_, field1, field2) => field1.leafExprs ++ field2.leafExprs
    case DateOrElse(field1, field2)    => field1.leafExprs ++ field2.leafExprs
  }

  def maybeFormCtx(
    recData: RecData[SourceOrigin.OutOfDate],
    evaluationContext: EvaluationContext,
    evaluationResults: EvaluationResults,
    booleanExprResolver: BooleanExprResolver
  ): Option[FormCtx] = this match {
    case DateValueExpr(_)        => None
    case DateFormCtxVar(formCtx) => Some(formCtx)
    case DateExprWithOffset(dExpr, _) =>
      dExpr.maybeFormCtx(recData, evaluationContext, evaluationResults, booleanExprResolver)
    case HmrcTaxPeriodCtx(formCtx, _) => Some(formCtx)
    case DataRetrieveDateCtx(_, _)    => None
    case DateIfElse(cond, field1, field2) =>
      if (booleanExprResolver.resolve(cond))
        field1.maybeFormCtx(recData, evaluationContext, evaluationResults, booleanExprResolver)
      else
        field2.maybeFormCtx(recData, evaluationContext, evaluationResults, booleanExprResolver)

    case DateOrElse(field1, field2) =>
      val isFirst = evalDateExpr(recData, evaluationContext, evaluationResults, booleanExprResolver)(field1)
        .fold[Boolean](_ => true)(_ => false)(_ => false)(_ => true)(_ => true)(_ => true)(_ => true)(_ => true)(_ =>
          true
        )(_ => true)(_ => true)
      if (isFirst)
        field1.maybeFormCtx(recData, evaluationContext, evaluationResults, booleanExprResolver)
      else
        field2.maybeFormCtx(recData, evaluationContext, evaluationResults, booleanExprResolver)
  }

  def expand(index: Int): DateExpr = this match {
    case DateFormCtxVar(FormCtx(formComponentId))       => DateFormCtxVar(FormCtx(formComponentId.withIndex(index)))
    case DateExprWithOffset(dateExpr: DateExpr, offset) => DateExprWithOffset(dateExpr.expand(index), offset)
    case other                                          => other
  }
}

sealed trait OffsetUnit
object OffsetUnit {
  case class Day(n: Int) extends OffsetUnit
  case class Year(n: Int) extends OffsetUnit
  case class Month(n: Int) extends OffsetUnit

  implicit val format: OFormat[OffsetUnit] = derived.oformat()
}

case class OffsetYMD(offsets: List[OffsetUnit]) // Order matters, since OffsetUnit is not commutative

object OffsetYMD {
  implicit val format: OFormat[OffsetYMD] = derived.oformat()
}

sealed trait DateExprValue {
  def toLocalDate =
    this match {
      case TodayDateExprValue                   => LocalDate.now()
      case ExactDateExprValue(year, month, day) => LocalDate.of(year, month, day)
    }
}
case object TodayDateExprValue extends DateExprValue
case class ExactDateExprValue(year: Int, month: Int, day: Int) extends DateExprValue

object DateExprValue {
  implicit val format: OFormat[DateExprValue] = derived.oformat()
}

case class DateValueExpr(value: DateExprValue) extends DateExpr
case class DateFormCtxVar(formCtx: FormCtx) extends DateExpr
case class DateExprWithOffset(dExpr: DateExpr, offset: OffsetYMD) extends DateExpr
case class HmrcTaxPeriodCtx(formCtx: FormCtx, hmrcTaxPeriodInfo: HmrcTaxPeriodInfo) extends DateExpr
case class DataRetrieveDateCtx(id: DataRetrieveId, attribute: DataRetrieve.Attribute) extends DateExpr
case class DateIfElse(ifElse: BooleanExpr, field1: DateExpr, field2: DateExpr) extends DateExpr
case class DateOrElse(field1: DateExpr, field2: DateExpr) extends DateExpr

object DateExpr {
  implicit val format: OFormat[DateExpr] = derived.oformat()
}
