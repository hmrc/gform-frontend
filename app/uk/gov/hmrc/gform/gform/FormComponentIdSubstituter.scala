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

object FormComponentIdSubstituter {
  import Substituter._

  implicit val exprSubstituter: Substituter[FormComponentIdSubstitutions, Expr] =
    new Substituter[FormComponentIdSubstitutions, Expr] {
      override def substitute(substitutions: FormComponentIdSubstitutions, expr: Expr): Expr = expr match {
        case Add(field1: Expr, field2: Expr) =>
          Add(substitute(substitutions, field1), substitute(substitutions, field2))
        case Multiply(field1: Expr, field2: Expr) =>
          Multiply(substitute(substitutions, field1), substitute(substitutions, field2))
        case Subtraction(field1: Expr, field2: Expr) =>
          Subtraction(substitute(substitutions, field1), substitute(substitutions, field2))
        case Divide(field1: Expr, field2: Expr) =>
          Divide(substitute(substitutions, field1), substitute(substitutions, field2))
        case IfElse(cond, field1: Expr, field2: Expr) =>
          IfElse(cond(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case SmartStringIf(cond, field1: Expr, field2: Expr) =>
          SmartStringIf(cond(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case Else(field1: Expr, field2: Expr) =>
          Else(substitute(substitutions, field1), substitute(substitutions, field2))
        case FormCtx(fcId)                 => FormCtx(substitutions.updateFormComponentId(fcId))
        case Sum(field1: Expr)             => Sum(substitute(substitutions, field1))
        case DateCtx(dateExpr)             => DateCtx(dateExpr(substitutions))
        case Count(fcId)                   => Count(substitutions.updateFormComponentId(fcId))
        case e: AuthCtx                    => e
        case e: UserCtx                    => e
        case e: Constant                   => e
        case e: PeriodValue                => e
        case Value                         => Value
        case e: FormTemplateCtx            => e
        case e: ParamCtx                   => e
        case e: LinkCtx                    => e
        case LangCtx                       => LangCtx
        case DateFunction(dateProjection)  => DateFunction(dateProjection(substitutions))
        case Period(field1, field2)        => Period(substitute(substitutions, field1), substitute(substitutions, field2))
        case PeriodExt(period, func)       => PeriodExt(substitute(substitutions, period), func)
        case AddressLens(fcId, details)    => AddressLens(substitutions.updateFormComponentId(fcId), details)
        case e: DataRetrieveCtx            => e
        case e: DataRetrieveCount          => e
        case CsvCountryCheck(fcId, column) => CsvCountryCheck(substitutions.updateFormComponentId(fcId), column)
        case CsvOverseasCountryCheck(fcId, column) =>
          CsvOverseasCountryCheck(substitutions.updateFormComponentId(fcId), column)
        case CsvCountryCountCheck(fcId, column, value) =>
          CsvCountryCountCheck(substitutions.updateFormComponentId(fcId), column, value)
        case Size(fcId, index)            => Size(substitutions.updateFormComponentId(fcId), index)
        case Typed(e, tpe)                => Typed(substitute(substitutions, e), tpe)
        case IndexOf(fcId, index)         => IndexOf(substitutions.updateFormComponentId(fcId), index)
        case e: IndexOfDataRetrieveCtx    => e
        case NumberedList(fcId)           => NumberedList(substitutions.updateFormComponentId(fcId))
        case BulletedList(fcId)           => BulletedList(substitutions.updateFormComponentId(fcId))
        case StringOps(field1, stringFnc) => StringOps(substitute(substitutions, field1), stringFnc)
        case Concat(exprs)                => Concat(exprs.map(substitute(substitutions, _)))
        case CountryOfItmpAddress         => CountryOfItmpAddress
        case ChoicesRevealedField(fcId)   => ChoicesRevealedField(substitutions.updateFormComponentId(fcId))
        case ChoiceLabel(fcId)            => ChoiceLabel(substitutions.updateFormComponentId(fcId))
      }
    }

  implicit val booleanExprSubstituter: Substituter[FormComponentIdSubstitutions, BooleanExpr] =
    new Substituter[FormComponentIdSubstitutions, BooleanExpr] {
      override def substitute(substitutions: FormComponentIdSubstitutions, expr: BooleanExpr): BooleanExpr =
        expr match {
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
          case Contains(FormCtx(fcId), value) =>
            Contains(FormCtx(substitutions.updateFormComponentId(fcId)), value(substitutions))
          case In(e, dataSource)       => In(e(substitutions), dataSource)
          case MatchRegex(expr, regex) => MatchRegex(expr(substitutions), regex)
          case FormPhase(value)        => FormPhase(value)
          case First(FormCtx(fcId))    => First(FormCtx(substitutions.updateFormComponentId(fcId)))
          case IsLogin(value)          => IsLogin(value)
          case DateBefore(left, right) => DateBefore(left(substitutions), right(substitutions))
          case DateAfter(left, right)  => DateAfter(left(substitutions), right(substitutions))
        }
    }

  implicit val dateExprSubstituter: Substituter[FormComponentIdSubstitutions, DateExpr] =
    new Substituter[FormComponentIdSubstitutions, DateExpr] {
      override def substitute(substitutions: FormComponentIdSubstitutions, expr: DateExpr): DateExpr = expr match {
        case DateValueExpr(value)          => DateValueExpr(value)
        case DateFormCtxVar(FormCtx(fcId)) => DateFormCtxVar(FormCtx(substitutions.updateFormComponentId(fcId)))
        case DateExprWithOffset(dExpr, o)  => DateExprWithOffset(substitute(substitutions, dExpr), o)
        case HmrcTaxPeriodCtx(FormCtx(fcId), hmrcTaxPeriodInfo) =>
          HmrcTaxPeriodCtx(FormCtx(substitutions.updateFormComponentId(fcId)), hmrcTaxPeriodInfo)
        case DateIfElse(ifElse, field1, field2) =>
          DateIfElse(ifElse(substitutions), substitute(substitutions, field1), substitute(substitutions, field2))
        case DateOrElse(field1, field2) =>
          DateOrElse(substitute(substitutions, field1), substitute(substitutions, field2))
      }

    }
  implicit val dateProjectionSubstituter: Substituter[FormComponentIdSubstitutions, DateProjection] =
    new Substituter[FormComponentIdSubstitutions, DateProjection] {
      override def substitute(substitutions: FormComponentIdSubstitutions, expr: DateProjection): DateProjection =
        expr match {
          case DateProjection.Day(dateExpr)   => DateProjection.Day(dateExpr(substitutions))
          case DateProjection.Month(dateExpr) => DateProjection.Month(dateExpr(substitutions))
          case DateProjection.Year(dateExpr)  => DateProjection.Year(dateExpr(substitutions))

        }

    }

  class FormComponentIdSubstitutions() {
    def updateFormComponentId(fcId: FormComponentId): FormComponentId =
      fcId.modelComponentId.removeIndex.toFormComponentId
  }

}
