/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.gform

import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ConcatFormatSubstituter {

  import Substituter._

  implicit val exprSubstituter: Substituter[ConcatFormatSubstitutions, Expr] =
    new Substituter[ConcatFormatSubstitutions, Expr] {
      override def substitute(substitutions: ConcatFormatSubstitutions, expr: Expr): Expr = expr match {
        case Add(field1: Expr, field2: Expr) =>
          Add(substitute(substitutions, field1), substitute(substitutions, field2))
        case Multiply(field1: Expr, field2: Expr) =>
          Multiply(substitute(substitutions, field1), substitute(substitutions, field2))
        case Subtraction(field1: Expr, field2: Expr) =>
          Subtraction(substitute(substitutions, field1), substitute(substitutions, field2))
        case Divide(field1: Expr, field2: Expr) =>
          Divide(substitute(substitutions, field1), substitute(substitutions, field2))
        case HideZeroDecimals(field1: Expr) => HideZeroDecimals(substitute(substitutions, field1))
        case IfElse(cond, field1: Expr, field2: Expr) =>
          IfElse(cond(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case Else(field1: Expr, field2: Expr) =>
          Else(substitute(substitutions, field1), substitute(substitutions, field2))
        case e: FormCtx => e

        case Sum(field1: Expr)            => Sum(substitute(substitutions, field1))
        case DateCtx(dateExpr)            => DateCtx(dateExpr(substitutions))
        case e: Count                     => e
        case e: Index                     => e
        case e: AuthCtx                   => e
        case e: UserCtx                   => e
        case e: Constant                  => e
        case e: PeriodValue               => e
        case Value                        => Value
        case e: FormTemplateCtx           => e
        case e: ParamCtx                  => e
        case e: LinkCtx                   => e
        case LangCtx                      => LangCtx
        case DateFunction(dateProjection) => DateFunction(dateProjection(substitutions))
        case Period(field1, field2)       => Period(substitute(substitutions, field1), substitute(substitutions, field2))
        case PeriodExt(period, func)      => PeriodExt(substitute(substitutions, period), func)
        case e: AddressLens               => e
        case e: DataRetrieveCtx           => e
        case e: DataRetrieveCount         => e
        case e: CsvCountryCheck           => e
        case e: CsvOverseasCountryCheck   => e
        case e: CsvCountryCountCheck      => e
        case e: Size                      => e
        case Typed(e, tpe)                => Typed(substitute(substitutions, e), tpe)
        case e: IndexOf                   => e
        case e: IndexOfDataRetrieveCtx    => e
        case e: NumberedList              => e
        case e: BulletedList              => e
        case StringOps(field1, stringFnc) => StringOps(substitute(substitutions, field1), stringFnc)
        case e: Concat                    => Constant(substitutions.formatConcat(e))
        case CountryOfItmpAddress         => CountryOfItmpAddress
        case e: ChoicesRevealedField      => e
        case e: ChoiceLabel               => Constant(substitutions.formatConcat(e))
        case e: ChoicesSelected           => e
        case e: ChoicesAvailable          => e
      }
    }

  implicit val booleanExprSubstituter: Substituter[ConcatFormatSubstitutions, BooleanExpr] =
    new Substituter[ConcatFormatSubstitutions, BooleanExpr] {
      override def substitute(substitutions: ConcatFormatSubstitutions, expr: BooleanExpr): BooleanExpr = expr match {
        case Equals(left, right)              => Equals(left(substitutions), right(substitutions))
        case GreaterThan(left, right)         => GreaterThan(left(substitutions), right(substitutions))
        case GreaterThanOrEquals(left, right) => GreaterThanOrEquals(left(substitutions), right(substitutions))
        case LessThan(left, right)            => LessThan(left(substitutions), right(substitutions))
        case LessThanOrEquals(left, right)    => LessThanOrEquals(left(substitutions), right(substitutions))
        case Not(e)                           => Not(substitute(substitutions, e))
        case Or(left, right)                  => Or(substitute(substitutions, left), substitute(substitutions, right))
        case And(left, right)                 => And(substitute(substitutions, left), substitute(substitutions, right))
        case IsTrue                           => IsTrue
        case IsFalse                          => IsFalse
        case Contains(multiValueField, value) => Contains(multiValueField, value(substitutions))
        case In(formCtx, dataSource)          => In(formCtx(substitutions), dataSource)
        case HasAnswer(formCtx, atlFormCtx)   => HasAnswer(formCtx, atlFormCtx)
        case MatchRegex(expr, regex)          => MatchRegex(expr(substitutions), regex)
        case FormPhase(value)                 => FormPhase(value)
        case First(formCtx)                   => First(formCtx)
        case IsLogin(value)                   => IsLogin(value)
        case DateBefore(left, right)          => DateBefore(left(substitutions), right(substitutions))
        case DateAfter(left, right)           => DateAfter(left(substitutions), right(substitutions))
        case DuplicateExists(fieldList)       => DuplicateExists(fieldList)
      }
    }

  implicit val dateExprSubstituter: Substituter[ConcatFormatSubstitutions, DateExpr] =
    new Substituter[ConcatFormatSubstitutions, DateExpr] {
      override def substitute(substitutions: ConcatFormatSubstitutions, expr: DateExpr): DateExpr = expr match {
        case DateValueExpr(value)                         => DateValueExpr(value)
        case DateFormCtxVar(formCtx)                      => DateFormCtxVar(formCtx)
        case DateExprWithOffset(dExpr, o)                 => DateExprWithOffset(substitute(substitutions, dExpr), o)
        case HmrcTaxPeriodCtx(formCtx, hmrcTaxPeriodInfo) => HmrcTaxPeriodCtx(formCtx, hmrcTaxPeriodInfo)
        case d @ DataRetrieveDateCtx(_, _)                => d
        case DateIfElse(ifElse, field1, field2) =>
          DateIfElse(ifElse(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case DateOrElse(field1, field2) =>
          DateOrElse(substitute(substitutions, field1), substitute(substitutions, field2))
      }
    }

  implicit val dateProjectionSubstituter: Substituter[ConcatFormatSubstitutions, DateProjection] =
    new Substituter[ConcatFormatSubstitutions, DateProjection] {
      override def substitute(substitutions: ConcatFormatSubstitutions, expr: DateProjection): DateProjection =
        expr match {
          case DateProjection.Day(dateExpr)   => DateProjection.Day(dateExpr(substitutions))
          case DateProjection.Month(dateExpr) => DateProjection.Month(dateExpr(substitutions))
          case DateProjection.Year(dateExpr)  => DateProjection.Year(dateExpr(substitutions))
        }
    }
}

case class ConcatFormatSubstitutions(concatFormatFunction: Expr => String) {
  def formatConcat(conc: Expr): String = concatFormatFunction(conc)
}
