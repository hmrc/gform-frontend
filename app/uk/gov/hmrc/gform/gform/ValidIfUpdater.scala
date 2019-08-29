/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ValidIfUpdater(validIf: ValidIf, index: Int, baseIds: List[FormComponentId]) {
  private def expandFcId(fcId: FormComponentId): FormComponentId =
    if (baseIds.contains(fcId) && index =!= 0) FormComponentId(index + "_" + fcId.value) else fcId

  private def expandExpr(expr: Expr): Expr = expr match {
    case Add(field1, field2)         => Add(expandExpr(field1), expandExpr(field2))
    case Multiply(field1, field2)    => Multiply(expandExpr(field1), expandExpr(field2))
    case Subtraction(field1, field2) => Subtraction(expandExpr(field1), expandExpr(field2))
    case Else(field1, field2)        => Else(expandExpr(field1), expandExpr(field2))
    case f @ FormCtx(value)          => FormCtx(expandFcId(f.toFieldId).value)
    case Sum(field1)                 => Sum(expandExpr(field1))
    case otherwise                   => otherwise
  }

  private def expandBooleanExpr(expr: BooleanExpr): BooleanExpr = expr match {
    case Equals(left, right)              => Equals(expandExpr(left), expandExpr(right))
    case NotEquals(left, right)           => NotEquals(expandExpr(left), expandExpr(right))
    case GreaterThan(left, right)         => GreaterThan(expandExpr(left), expandExpr(right))
    case GreaterThanOrEquals(left, right) => GreaterThanOrEquals(expandExpr(left), expandExpr(right))
    case LessThan(left, right)            => LessThan(expandExpr(left), expandExpr(right))
    case LessThanOrEquals(left, right)    => LessThanOrEquals(expandExpr(left), expandExpr(right))
    case Not(e)                           => Not(expandBooleanExpr(e))
    case Or(left, right)                  => Or(expandBooleanExpr(left), expandBooleanExpr(right))
    case And(left, right)                 => And(expandBooleanExpr(left), expandBooleanExpr(right))
    case otherwise                        => otherwise
  }

  val updated = validIf.copy(expr = expandBooleanExpr(validIf.expr))
}

object ValidIfUpdater {
  def apply(validIf: ValidIf, index: Int, group: Group) = new ValidIfUpdater(validIf, index, group.fields.map(_.id))
  def apply(validIf: ValidIf, index: Int, section: Section) =
    new ValidIfUpdater(validIf, index, section.fields.map(_.id))
}
