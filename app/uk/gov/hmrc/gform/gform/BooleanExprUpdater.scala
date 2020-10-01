/*
 * Copyright 2020 HM Revenue & Customs
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

import uk.gov.hmrc.gform.models.PageMode
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class BooleanExprUpdater(index: Int, baseIds: List[FormComponentId]) {

  private def expandExpr(expr: Expr): Expr = ExprUpdater(expr, index, baseIds)
  private def expandFormCtx(formCtx: FormCtx): FormCtx = ExprUpdater.formCtx(formCtx, index, baseIds)

  def apply(booleanExpr: BooleanExpr): BooleanExpr = booleanExpr match {
    case Equals(left, right)              => Equals(expandExpr(left), expandExpr(right))
    case NotEquals(left, right)           => NotEquals(expandExpr(left), expandExpr(right))
    case GreaterThan(left, right)         => GreaterThan(expandExpr(left), expandExpr(right))
    case GreaterThanOrEquals(left, right) => GreaterThanOrEquals(expandExpr(left), expandExpr(right))
    case LessThan(left, right)            => LessThan(expandExpr(left), expandExpr(right))
    case LessThanOrEquals(left, right)    => LessThanOrEquals(expandExpr(left), expandExpr(right))
    case Not(e)                           => Not(apply(e))
    case Or(left, right)                  => Or(apply(left), apply(right))
    case And(left, right)                 => And(apply(left), apply(right))
    case Contains(formCtx, expr)          => Contains(expandFormCtx(formCtx), expandExpr(expr))
    case In(expr, dataSource)             => In(expandExpr(expr), dataSource)
    case otherwise                        => otherwise
  }
}

object BooleanExprUpdater {
  def apply(booleanExpr: BooleanExpr, index: Int, baseIds: List[FormComponentId]): BooleanExpr =
    new BooleanExprUpdater(index, baseIds)(booleanExpr)
}
