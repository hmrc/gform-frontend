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

package uk.gov.hmrc.gform.gform

import cats.instances.int._
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ExprUpdater(index: Int, baseIds: List[FormComponentId]) {

  val beUpdater = new BooleanExprUpdater(index, baseIds)

  private def expandFcId(fcId: FormComponentId): FormComponentId =
    if (baseIds.contains(fcId) && index =!= 0) FormComponentId(s"${index}_${fcId.value}") else fcId

  def expandExpr(expr: Expr): Expr = expr match {
    case Add(field1, field2)                  => Add(expandExpr(field1), expandExpr(field2))
    case Multiply(field1, field2)             => Multiply(expandExpr(field1), expandExpr(field2))
    case Subtraction(field1, field2)          => Subtraction(expandExpr(field1), expandExpr(field2))
    case Divide(field1, field2)               => Divide(expandExpr(field1), expandExpr(field2))
    case HideZeroDecimals(field1: Expr)       => HideZeroDecimals(expandExpr(field1))
    case Else(field1, field2)                 => Else(expandExpr(field1), expandExpr(field2))
    case IfElse(cond, field1, field2)         => IfElse(beUpdater(cond), expandExpr(field1), expandExpr(field2))
    case FormCtx(formComponentId)             => FormCtx(expandFcId(formComponentId))
    case Sum(expr)                            => Sum(expandExpr(expr))
    case DateCtx(dateExpr)                    => DateCtx(expandDateExpr(dateExpr))
    case DateFunction(dateFunc)               => DateFunction(expandDateFunc(dateFunc))
    case AddressLens(formComponentId, detail) => AddressLens(expandFcId(formComponentId), detail)
    case LinkCtx(PageLink(id))                => LinkCtx(PageLink(id.withIndex(index)))
    case DataRetrieveCtx(id, attribute)       => DataRetrieveCtx(id.withIndex(index), attribute)
    case DataRetrieveCount(id)                => DataRetrieveCount(id.withIndex(index))
    case LookupColumn(fcId, c)                => LookupColumn(expandFcId(fcId), c)
    case CsvCountryCountCheck(fcId, c, v)     => CsvCountryCountCheck(expandFcId(fcId), c, v)
    case Size(formComponentId, index)         => Size(expandFcId(formComponentId), index)
    case Typed(expr, tpe)                     => Typed(expandExpr(expr), tpe)
    case Count(formComponentId)               => Count(expandFcId(formComponentId))
    case Index(formComponentId)               => Index(expandFcId(formComponentId))
    case AuthCtx(_)                           => expr
    case UserCtx(_)                           => expr
    case Constant(_)                          => expr
    case PeriodValue(_)                       => expr
    case Value                                => expr
    case FormTemplateCtx(_)                   => expr
    case ParamCtx(_)                          => expr
    case LinkCtx(_)                           => expr
    case LangCtx                              => expr
    case Period(_, _)                         => expr
    case PeriodExt(_, _)                      => expr
    case b @ Between(_, _, _) =>
      b match {
        case Between(DateCtx(dateExpr1), DateCtx(dateExpr2), m) =>
          Between(DateCtx(expandDateExpr(dateExpr1)), DateCtx(expandDateExpr(dateExpr2)), m)
        case _ => expr
      }
    case IndexOf(_, _)                         => expr // This is not expanded on purpose, so it can be used correctly inside ATL
    case IndexOfDataRetrieveCtx(_, _)          => expr
    case NumberedList(formComponentId)         => NumberedList(expandFcId(formComponentId))
    case BulletedList(formComponentId)         => BulletedList(expandFcId(formComponentId))
    case StringOps(expr, stringFnc)            => StringOps(expandExpr(expr), stringFnc)
    case Concat(exprs)                         => Concat(exprs.map(expandExpr))
    case CountryOfItmpAddress                  => expr
    case ChoicesRevealedField(formComponentId) => ChoicesRevealedField(expandFcId(formComponentId))
    case ChoicesSelected(formComponentId)      => ChoicesSelected(expandFcId(formComponentId))
    case ChoicesAvailable(formComponentId)     => ChoicesAvailable(expandFcId(formComponentId))
    case CountSelectedChoices(formComponentId) => CountSelectedChoices(expandFcId(formComponentId))
    case TaskStatus(_)                         => expr
    case LookupOps(expr, lookupFnc)            => LookupOps(expandExpr(expr), lookupFnc)
  }

  private def expandDateFunc(dateFunc: DateProjection): DateProjection = dateFunc match {
    case DateProjection.Day(dateExpr)   => DateProjection.Day(expandDateExpr(dateExpr))
    case DateProjection.Month(dateExpr) => DateProjection.Month(expandDateExpr(dateExpr))
    case DateProjection.Year(dateExpr)  => DateProjection.Year(expandDateExpr(dateExpr))
  }

  private def expandDateExpr(dateExpr: DateExpr): DateExpr = dateExpr match {
    case DateFormCtxVar(formCtx)             => DateFormCtxVar(expandFormCtx(formCtx))
    case DateExprWithOffset(dateExr, offset) => DateExprWithOffset(expandDateExpr(dateExr), offset)
    case DateConstructExpr(dm, year)         => DateConstructExpr(expandDateExpr(dm), expandExpr(year))
    case otherwise                           => otherwise
  }

  def expandFormCtx(formCtx: FormCtx): FormCtx = FormCtx(expandFcId(formCtx.formComponentId))
}

object ExprUpdater {
  def apply(expr: Expr, index: Int, baseIds: List[FormComponentId]) = new ExprUpdater(index, baseIds).expandExpr(expr)

  def formCtx(formCtx: FormCtx, index: Int, baseIds: List[FormComponentId]): FormCtx =
    new ExprUpdater(index, baseIds).expandFormCtx(formCtx)
}
