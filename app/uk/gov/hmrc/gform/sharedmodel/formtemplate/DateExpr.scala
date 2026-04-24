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

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }

import java.time.LocalDate

sealed trait DateExpr {

  def booleanExprs(): List[BooleanExpr] = this match {
    case DateValueExpr(_)                   => Nil
    case DateFormCtxVar(_)                  => Nil
    case DateExprWithOffset(dExpr, _)       => dExpr.booleanExprs()
    case HmrcTaxPeriodCtx(_, _)             => Nil
    case DataRetrieveDateCtx(_, _)          => Nil
    case DateIfElse(ifElse, field1, field2) => ifElse :: field1.booleanExprs() ++ field2.booleanExprs()
    case DateOrElse(field1, field2)         => field1.booleanExprs() ++ field2.booleanExprs()
    case DateConstructExpr(dm, _)           => dm.booleanExprs()
    case EarliestOf(exprs)                  => exprs.flatMap(_.booleanExprs())
    case LatestOf(exprs)                    => exprs.flatMap(_.booleanExprs())
  }

  def collect[T](pf: PartialFunction[Expr, T]): List[T] = this match {
    case DateValueExpr(_)             => Nil
    case DateFormCtxVar(formCtx)      => formCtx.collect(pf)
    case DateExprWithOffset(dExpr, _) => dExpr.collect(pf)
    case HmrcTaxPeriodCtx(formCtx, _) => formCtx.collect(pf)
    case DataRetrieveDateCtx(_, _)    => Nil
    case DateIfElse(ifElse, field1, field2) =>
      ifElse.allExpressions.flatMap(_.collect(pf)) ++ field1.collect(pf) ++ field2.collect(pf)
    case DateOrElse(field1, field2)  => field1.collect(pf) ++ field2.collect(pf)
    case DateConstructExpr(dm, expr) => dm.collect(pf) ++ expr.collect(pf)
    case EarliestOf(exprs)           => exprs.flatMap(_.collect(pf))
    case LatestOf(exprs)             => exprs.flatMap(_.collect(pf))
  }

  def expand(index: Int): DateExpr = this match {
    case DateFormCtxVar(FormCtx(formComponentId))       => DateFormCtxVar(FormCtx(formComponentId.withIndex(index)))
    case DateExprWithOffset(dateExpr: DateExpr, offset) => DateExprWithOffset(dateExpr.expand(index), offset)
    case EarliestOf(exprs)                              => EarliestOf(exprs.map(_.expand(index)))
    case LatestOf(exprs)                                => LatestOf(exprs.map(_.expand(index)))
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
  def toLocalDate(formStartDate: LocalDate): LocalDate =
    this match {
      case TodayDateExprValue                   => LocalDate.now()
      case ExactDateExprValue(year, month, day) => LocalDate.of(year, month, day)
      case FormStartDateExprValue               => formStartDate
    }
}
case object TodayDateExprValue extends DateExprValue
case object FormStartDateExprValue extends DateExprValue
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
case class DateConstructExpr(dayMonth: DateExpr, year: Expr) extends DateExpr
case class EarliestOf(exprs: List[DateExpr]) extends DateExpr
case class LatestOf(exprs: List[DateExpr]) extends DateExpr

object DateExpr {
  implicit val format: OFormat[DateExpr] = derived.oformat()
}
