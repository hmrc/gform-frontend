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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprUpdater(index: Int, baseIds: List[FormComponentId]) {

  private def expandExpr(expr: Expr): Expr = ExprUpdater(expr, index, baseIds)
  private def expandFormCtx(formCtx: FormCtx): FormCtx = ExprUpdater.formCtx(formCtx, index, baseIds)
  private def expandDateExpr(dateExpr: DateExpr): DateExpr = dateExpr match {
    case de @ DateValueExpr(value: DateExprValue)               => de
    case DateFormCtxVar(formCtx: FormCtx)                       => DateFormCtxVar(expandFormCtx(formCtx))
    case DateExprWithOffset(dExpr: DateExpr, offset: OffsetYMD) => DateExprWithOffset(expandDateExpr(dExpr), offset)
    case HmrcTaxPeriodCtx(formCtx: FormCtx, _)                  => DateFormCtxVar(expandFormCtx(formCtx))
    case d @ DataRetrieveDateCtx(_, _)                          => d
    case DateIfElse(cond, field1, field2)                       => DateIfElse(cond, expandDateExpr(field1), expandDateExpr(field2))
    case DateOrElse(field1, field2)                             => DateOrElse(expandDateExpr(field1), expandDateExpr(field2))
    case DateConstructExpr(dm, year)                            => DateConstructExpr(dm, expandExpr(year))
  }
  private def expandAddLoListRef(addToListRef: AddToListRef): AddToListRef = addToListRef match {
    case AddToListRef.Basic(atlFormCtx) =>
      NonEmptyList
        .fromList((1 to index).toList.map(idx => ExprUpdater.formCtx(atlFormCtx, idx, baseIds)))
        .fold[AddToListRef](AddToListRef.Basic(atlFormCtx))(AddToListRef.Expanded(_))
    case AddToListRef.Expanded(_) => addToListRef
  }

  def apply(booleanExpr: BooleanExpr): BooleanExpr = booleanExpr match {
    case Equals(left, right)              => Equals(expandExpr(left), expandExpr(right))
    case GreaterThan(left, right)         => GreaterThan(expandExpr(left), expandExpr(right))
    case GreaterThanOrEquals(left, right) => GreaterThanOrEquals(expandExpr(left), expandExpr(right))
    case LessThan(left, right)            => LessThan(expandExpr(left), expandExpr(right))
    case LessThanOrEquals(left, right)    => LessThanOrEquals(expandExpr(left), expandExpr(right))
    case Not(e)                           => Not(apply(e))
    case Or(left, right)                  => Or(apply(left), apply(right))
    case And(left, right)                 => And(apply(left), apply(right))
    case Contains(formCtx, expr)          => Contains(expandFormCtx(formCtx), expandExpr(expr))
    case In(expr, dataSource)             => In(expandExpr(expr), dataSource)
    case HasAnswer(formCtx, atlRef)       => HasAnswer(expandFormCtx(formCtx), expandAddLoListRef(atlRef))
    case MatchRegex(expr, regex)          => MatchRegex(expandExpr(expr), regex)
    case DateAfter(left, right)           => DateAfter(expandDateExpr(left), expandDateExpr(right))
    case DateBefore(left, right)          => DateBefore(expandDateExpr(left), expandDateExpr(right))
    case First(formCtx)                   => First(expandFormCtx(formCtx))
    case DuplicateExists(fieldList)       => DuplicateExists(fieldList.map(expandFormCtx))
    case otherwise                        => otherwise
  }
}

object BooleanExprUpdater {
  def apply(booleanExpr: BooleanExpr, index: Int, baseIds: List[FormComponentId]): BooleanExpr =
    new BooleanExprUpdater(index, baseIds)(booleanExpr)
}
