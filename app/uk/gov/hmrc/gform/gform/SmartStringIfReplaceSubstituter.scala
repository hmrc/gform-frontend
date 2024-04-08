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

object SmartStringIfReplaceSubstituter {
  implicit val exprSubstituter: Substituter[SmartStringIfReplaceSubstitutions, Expr] =
    new Substituter[SmartStringIfReplaceSubstitutions, Expr] {
      override def substitute(substitutions: SmartStringIfReplaceSubstitutions, expr: Expr): Expr = expr match {
        case SmartStringIf(cond, field1: Concat, field2: Concat) =>
          SmartStringIf(cond, substitutions.doSubstitute(field1), substitutions.doSubstitute(field2))
        case SmartStringIf(cond, field1: Expr, field2: Concat) =>
          SmartStringIf(cond, substitute(substitutions, field1), substitutions.doSubstitute(field2))
        case SmartStringIf(cond, field1: Concat, field2: Expr) =>
          SmartStringIf(cond, substitutions.doSubstitute(field1), substitute(substitutions, field2))
        case SmartStringIf(cond, field1: Expr, field2: Expr) =>
          SmartStringIf(cond, substitute(substitutions, field1), substitute(substitutions, field2))
        case e => e
      }
    }
}

case class SmartStringIfReplaceSubstitutions(toReplace: String, replaceWith: String) {
  val f: Concat => Expr = concat => {
    Concat(concat.exprs.map {
      case Constant(value) => Constant(value.replace(toReplace, replaceWith))
      case otherwise       => otherwise
    })
  }
  def doSubstitute(concat: Concat): Expr = f(concat)
}
