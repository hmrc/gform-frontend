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

import cats.instances.int._
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ExprUpdater(index: Int, baseIds: List[FormComponentId]) {

  private def expandFcId(fcId: FormComponentId): FormComponentId =
    if (baseIds.contains(fcId) && index =!= 0) FormComponentId(index + "_" + fcId.value) else fcId

  def expandExpr(expr: Expr): Expr = expr match {
    case Add(field1, field2)           => Add(expandExpr(field1), expandExpr(field2))
    case Multiply(field1, field2)      => Multiply(expandExpr(field1), expandExpr(field2))
    case Subtraction(field1, field2)   => Subtraction(expandExpr(field1), expandExpr(field2))
    case Else(field1, field2)          => Else(expandExpr(field1), expandExpr(field2))
    case FormCtx(formComponentId)      => FormCtx(expandFcId(formComponentId))
    case Sum(FormCtx(formComponentId)) => Sum(FormCtx(expandFcId(formComponentId)))
    case otherwise                     => otherwise
  }

  def expandFormCtx(formCtx: FormCtx): FormCtx = FormCtx(expandFcId(formCtx.formComponentId))
}

object ExprUpdater {
  def apply(expr: Expr, index: Int, baseIds: List[FormComponentId]) = new ExprUpdater(index, baseIds).expandExpr(expr)

  def formCtx(formCtx: FormCtx, index: Int, baseIds: List[FormComponentId]): FormCtx =
    new ExprUpdater(index, baseIds).expandFormCtx(formCtx)
}
