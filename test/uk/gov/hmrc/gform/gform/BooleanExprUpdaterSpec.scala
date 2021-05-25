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

import munit.FunSuite
import scala.util.matching.Regex
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, DateAfter, DateBefore, DateExpr, DateFormCtxVar, FormComponentId, FormCtx, MatchRegex }

class BooleanExprUpdaterSpec extends FunSuite {

  private val beUpdater = new BooleanExprUpdater(1, List(FormComponentId("foo"), FormComponentId("bar")))

  private def toBooleanExpr(foo: String, bar: String, g: (DateExpr, DateExpr) => BooleanExpr): BooleanExpr = {
    val left = DateFormCtxVar(FormCtx(FormComponentId(foo)))
    val right = DateFormCtxVar(FormCtx(FormComponentId(bar)))
    g(left, right)
  }

  test("BooleanExprUpdater should expand DateBefore") {

    val result: BooleanExpr = beUpdater(toBooleanExpr("foo", "bar", DateBefore(_, _)))
    val expected: BooleanExpr = toBooleanExpr("1_foo", "1_bar", DateBefore(_, _))

    assertEquals(expected, result)
  }

  test("BooleanExprUpdater should expand DateAfter") {

    val result: BooleanExpr = beUpdater(toBooleanExpr("foo", "bar", DateAfter(_, _)))
    val expected: BooleanExpr = toBooleanExpr("1_foo", "1_bar", DateAfter(_, _))

    assertEquals(expected, result)
  }

  test("BooleanExprUpdater should expand MatchRegex") {

    val regex = new Regex("")

    val result: BooleanExpr = beUpdater(MatchRegex(FormCtx(FormComponentId("foo")), regex))
    val expected: BooleanExpr = MatchRegex(FormCtx(FormComponentId("1_foo")), regex)

    assertEquals(expected, result)
  }
}
