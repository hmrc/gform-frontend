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

object HiddenFormComponentSubstituter {
  import Substituter._

  implicit val exprSubstituter: Substituter[HiddenFormComponentSubstitutions, Expr] =
    new Substituter[HiddenFormComponentSubstitutions, Expr] {
      override def substitute(substitutions: HiddenFormComponentSubstitutions, expr: Expr): Expr = expr match {
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
        case Else(field1: Expr, field2: Expr) =>
          Else(substitute(substitutions, field1), substitute(substitutions, field2))
        case e @ FormCtx(fcId)                             => substitutions.replaceWithZero(fcId, e)
        case Sum(field1: Expr)                             => Sum(substitute(substitutions, field1))
        case DateCtx(dateExpr)                             => DateCtx(dateExpr(substitutions))
        case Count(fcId)                                   => Count(fcId)
        case e: AuthCtx                                    => e
        case e: UserCtx                                    => e
        case e: Constant                                   => e
        case e: PeriodValue                                => e
        case Value                                         => Value
        case e: FormTemplateCtx                            => e
        case e: ParamCtx                                   => e
        case e: LinkCtx                                    => e
        case LangCtx                                       => LangCtx
        case DateFunction(dateProjection)                  => DateFunction(dateProjection(substitutions))
        case Period(field1, field2)                        => Period(substitute(substitutions, field1), substitute(substitutions, field2))
        case PeriodExt(period, func)                       => PeriodExt(substitute(substitutions, period), func)
        case e @ AddressLens(fcId, details)                => e
        case e: DataRetrieveCtx                            => e
        case e: DataRetrieveCount                          => e
        case CsvCountryCheck(fcId, column)                 => CsvCountryCheck(substitutions.updateHiddenFormComponent(fcId), column)
        case e @ CsvOverseasCountryCheck(fcId, column)     => e
        case e @ CsvCountryCountCheck(fcId, column, value) => e
        case e @ Size(fcId, index)                         => e
        case Typed(e, tpe)                                 => Typed(substitute(substitutions, e), tpe)
        case e @ IndexOf(fcId, index)                      => e
        case e: IndexOfDataRetrieveCtx                     => e
        case e @ NumberedList(fcId)                        => e
        case e @ BulletedList(fcId)                        => e
        case StringOps(field1, stringFnc)                  => StringOps(substitute(substitutions, field1), stringFnc)
        case Concat(exprs)                                 => Concat(exprs.map(substitute(substitutions, _)))
        case CountryOfItmpAddress                          => CountryOfItmpAddress
        case e @ ChoicesRevealedField(fcId)                => e
        case e @ ChoiceLabel(fcId)                         => e
      }
    }

  implicit val booleanExprSubstituter: Substituter[HiddenFormComponentSubstitutions, BooleanExpr] =
    new Substituter[HiddenFormComponentSubstitutions, BooleanExpr] {
      override def substitute(substitutions: HiddenFormComponentSubstitutions, expr: BooleanExpr): BooleanExpr =
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
            Contains(FormCtx(substitutions.updateHiddenFormComponent(fcId)), value(substitutions))
          case In(e, dataSource)        => In(e(substitutions), dataSource)
          case MatchRegex(expr, regex)  => MatchRegex(expr(substitutions), regex)
          case FormPhase(value)         => FormPhase(value)
          case e @ First(FormCtx(fcId)) => e
          case IsLogin(value)           => IsLogin(value)
          case DateBefore(left, right)  => DateBefore(left(substitutions), right(substitutions))
          case DateAfter(left, right)   => DateAfter(left(substitutions), right(substitutions))
        }
    }

  implicit val dateExprSubstituter: Substituter[HiddenFormComponentSubstitutions, DateExpr] =
    new Substituter[HiddenFormComponentSubstitutions, DateExpr] {
      override def substitute(substitutions: HiddenFormComponentSubstitutions, expr: DateExpr): DateExpr = expr match {
        case otherwise => otherwise
      }

    }
  implicit val dateProjectionSubstituter: Substituter[HiddenFormComponentSubstitutions, DateProjection] =
    new Substituter[HiddenFormComponentSubstitutions, DateProjection] {
      override def substitute(substitutions: HiddenFormComponentSubstitutions, expr: DateProjection): DateProjection =
        expr match {
          case otherwise => otherwise
        }

    }

  class HiddenFormComponentSubstitutions(hiddenFormComponentIds: List[FormComponentId]) {
    def updateHiddenFormComponent(fcId: FormComponentId): FormComponentId =
      fcId.modelComponentId.removeIndex.toFormComponentId
    def replaceWithZero(fcId: FormComponentId, exp: Expr): Expr =
      if (hiddenFormComponentIds.contains(fcId))
        Constant("0")
      else
        exp
  }

}
