/*
 * Copyright 2025 HM Revenue & Customs
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
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.recalculation.EvaluationStatus._

class EvaluationStatusSuite extends FunSuite {

  implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map("en" -> Map.empty[String, String])
    )
  )

  test("EvaluationStatus subtraction") {
    // Be careful to test subtraction on EvaluationStatus (not the one on NumberResult!!!)
    // These type annotations are here on purpose.
    val l: EvaluationStatus = NumberResult(10)
    val r: EvaluationStatus = NumberResult(6)

    val result = l - r

    assertEquals(result, NumberResult(4))
  }

  test("EvaluationStatus addition of two NumberResults") {
    // Be careful to test addition on EvaluationStatus (not the one on NumberResult!!!)
    // These type annotations are here on purpose.
    val l: EvaluationStatus = NumberResult(10)
    val r: EvaluationStatus = NumberResult(6)

    val result = l + r

    assertEquals(result, NumberResult(16))
  }

  test("EvaluationStatus addition of two StringResults") {
    // Be careful to test addition on EvaluationStatus (not the one on StringResult!!!)
    // These type annotations are here on purpose.
    val l: EvaluationStatus = StringResult("Foo")
    val r: EvaluationStatus = StringResult("Bar")

    val result = l + r

    assertEquals(result, StringResult("FooBar"))
  }

  test("EvaluationStatus addition of two StringResult and NumberResult") {
    val l = StringResult("Foo")
    val r = NumberResult(12)

    val result = l + r

    assertEquals(result, StringResult("Foo12"))
  }

  test("EvaluationStatus addition of two NumberResult and StringResult") {
    val l = NumberResult(12)
    val r = StringResult("Foo")

    val result = l + r

    assertEquals(result, StringResult("12Foo"))
  }

  val emptyResults = List(
    Empty,
    Hidden,
    StringResult(""),
    AddressResult(List.empty[(Atom, String)]),
    ListResult(List.empty[EvaluationStatus]),
    ListResult(List(Hidden)),
    ListResult(List(Hidden, Hidden)),
    ListResult(List(Empty)),
    ListResult(List(Empty, Empty)),
    ListResult(List(StringResult(""))),
    ListResult(List(Hidden, Empty, StringResult("")))
  )

  emptyResults.foreach { evaluationStatus =>
    test(s"identity: $evaluationStatus.identical($evaluationStatus)") {
      assert(evaluationStatus.identical(evaluationStatus).forall(_ == true))
    }
  }

  val combinations: Iterator[List[EvaluationStatus]] = emptyResults.combinations(2)
  combinations.foreach {
    case lhs :: rhs :: Nil =>
      test(s"identity: $lhs.identical($rhs)") {
        assert(lhs.identical(rhs).forall(_ == true))
      }
      test(s"identity: $rhs.identical($lhs)") {
        assert(rhs.identical(lhs).forall(_ == true))
      }
    case unexpected => fail(s"Unexpected input: $unexpected")
  }
}
