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

object SmartStringIfBranchSubstituter {
  implicit val exprSubstituter1: Substituter[SmartStringIfBranchSubstitutions, Expr] =
    new Substituter[SmartStringIfBranchSubstitutions, Expr] {
      override def substitute(substitutions: SmartStringIfBranchSubstitutions, expr: Expr): Expr = expr match {
        case s: SmartStringIf => substitute(substitutions, substitutions.reduce(s))
        case e                => e
      }
    }
}

case class SmartStringIfBranchSubstitutions(resolve: BooleanExpr => Boolean) {
  def reduce(smartStringIf: SmartStringIf): Expr = {
    val SmartStringIf(cond, field1, field2) = smartStringIf
    if (resolve(cond)) field1 else field2
  }
}
