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

package uk.gov.hmrc.gform

import cats.syntax.eq._
import org.scalatest.matchers.{ MatchResult, Matcher }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, FormComponent, HasExpr }

trait FormComponentMatcher {

  def containsExpr(expr: Expr): Matcher[FormComponent] = new ContainsExpr(expr)

  final private class ContainsExpr(expected: Expr) extends Matcher[FormComponent] {
    def apply(fc: FormComponent): MatchResult =
      MatchResult(
        fc match {
          case HasExpr(expr) => expr === expected
          case _             => false
        },
        s"'${fc.`type`}' do not contains an expression matching '$expected'.",
        s"'${fc.`type`}' contains an expression '$expected', but should not have."
      )
  }
}
