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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import munit.FunSuite
import scala.language.implicitConversions

class ExprPrettyPrintSuite extends FunSuite {

  implicit def literalToConstant(s: String): Expr = FormCtx(s)
  implicit def literalToFormComponentId(s: String): FormComponentId = FormComponentId(s)

  val booleanExpressions: List[(BooleanExpr, String)] =
    List(
      Equals("a", "b")                            -> "a = b",
      Equals(Add("a", "b"), "c")                  -> "a + b = c",
      Equals(Subtraction("a", "b"), "c")          -> "a - b = c",
      Equals(Multiply("a", "b"), "c")             -> "a * b = c",
      Equals(Divide("a", "b"), "c")               -> "a / b = c",
      GreaterThanOrEquals("a", "b")               -> "a >= b",
      LessThanOrEquals("a", "b")                  -> "a <= b",
      GreaterThan("a", "b")                       -> "a > b",
      LessThan("a", "b")                          -> "a < b",
      Contains(FormCtx("fieldId"), Constant("1")) -> "fieldId contains 1",
      DateBefore(
        DateFormCtxVar(FormCtx("startDate")),
        DateFormCtxVar(FormCtx("endDate"))
      ) -> "date from startDate component is before date from endDate component"
    )

  booleanExpressions.map { case (expression, expected) =>
    test(s"pretty print boolean expression: $expression") {
      val res = expression.prettyPrint
      assertEquals(res, expected)
    }
  }
}
