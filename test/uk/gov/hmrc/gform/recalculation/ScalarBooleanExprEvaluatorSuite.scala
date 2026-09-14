/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import munit.FunSuite
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ And, BooleanExpr, IsFalse, IsTrue, Not, Or }

class ScalarBooleanExprEvaluatorSuite extends FunSuite {

  test("combines mixed ATL results using scalar any-match semantics") {
    val leafResults: Map[BooleanExpr, Boolean] = Map(
      IsTrue  -> true,
      IsFalse -> false
    )

    val expression = And(Or(IsTrue, IsFalse), Or(IsFalse, IsFalse))

    assertEquals(
      ScalarBooleanExprEvaluator.evaluate(expression)(leafResults),
      false
    )
    assertEquals(
      ScalarBooleanExprEvaluator.evaluate(Or(IsTrue, IsFalse))(leafResults),
      true
    )
  }

  test("negates a scalar result after reducing the nested expression") {
    val leafResults: Map[BooleanExpr, Boolean] = Map(
      IsTrue  -> true,
      IsFalse -> false
    )

    assertEquals(
      ScalarBooleanExprEvaluator.evaluate(Not(Or(IsTrue, IsFalse)))(leafResults),
      false
    )
  }
}
