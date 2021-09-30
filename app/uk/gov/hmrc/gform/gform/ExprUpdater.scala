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

package uk.gov.hmrc.gform.gform

import cats.instances.int._
import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.InternalLink.PageLink
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ExprUpdater(index: Int, baseIds: List[FormComponentId]) {

  val beUpdater = new BooleanExprUpdater(index, baseIds)

  private def expandFcId(fcId: FormComponentId): FormComponentId =
    if (baseIds.contains(fcId) && index =!= 0) FormComponentId(index + "_" + fcId.value) else fcId

  def expandExpr(expr: Expr): Expr = expr match {
    case Add(field1, field2)                  => Add(expandExpr(field1), expandExpr(field2))
    case Multiply(field1, field2)             => Multiply(expandExpr(field1), expandExpr(field2))
    case Subtraction(field1, field2)          => Subtraction(expandExpr(field1), expandExpr(field2))
    case Else(field1, field2)                 => Else(expandExpr(field1), expandExpr(field2))
    case IfElse(cond, field1, field2)         => IfElse(beUpdater(cond), expandExpr(field1), expandExpr(field2))
    case FormCtx(formComponentId)             => FormCtx(expandFcId(formComponentId))
    case Sum(expr)                            => Sum(expandExpr(expr))
    case DateCtx(dateExpr)                    => DateCtx(expandDateExpr(dateExpr))
    case AddressLens(formComponentId, detail) => AddressLens(expandFcId(formComponentId), detail)
    case LinkCtx(PageLink(id))                => LinkCtx(PageLink(id.withIndex(index)))
    case DataRetrieveCtx(id, attribute)       => DataRetrieveCtx(id.withIndex(index), attribute)
    case otherwise                            => otherwise
  }

  private def expandDateExpr(dateExpr: DateExpr): DateExpr = dateExpr match {
    case DateFormCtxVar(formCtx)             => DateFormCtxVar(expandFormCtx(formCtx))
    case DateExprWithOffset(dateExr, offset) => DateExprWithOffset(expandDateExpr(dateExr), offset)
    case otherwise                           => otherwise
  }

  def expandFormCtx(formCtx: FormCtx): FormCtx = FormCtx(expandFcId(formCtx.formComponentId))
}

object ExprUpdater {
  def apply(expr: Expr, index: Int, baseIds: List[FormComponentId]) = new ExprUpdater(index, baseIds).expandExpr(expr)

  def formCtx(formCtx: FormCtx, index: Int, baseIds: List[FormComponentId]): FormCtx =
    new ExprUpdater(index, baseIds).expandFormCtx(formCtx)
}
