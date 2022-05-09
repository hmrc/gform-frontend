/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.eval

import java.time.Period
import munit.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Messages
import play.api.test.Helpers

import java.time.LocalDate
import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, Empty, Hidden, ListResult, NumberResult, OptionResult, PeriodResult, StringResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DateCtx, DateValueExpr, FormComponentId, FormCtx, TodayDateExprValue, WholeSterling }

class ExpressionResultSpec extends FunSuite {

  private implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.January" -> "January",
          "date.May"     -> "May"
        )
      )
    )
  )

  test("stringRepresentation should format DateResult as 'dd MMMM yyyy'") {
    val dateResult = DateResult(LocalDate.of(1970, 1, 1))
    val result = dateResult.stringRepresentation(
      TypeInfo(DateCtx(DateValueExpr(TodayDateExprValue)), StaticTypeData(ExprType.dateString, None)),
      messages
    )

    assertEquals(result, "1 January 1970")
  }

  test("identical needs to return true for Number(222.00) and Constant(222)") {
    val stringResult = StringResult("222")

    val numberResult = NumberResult(222.00)
    val strNum = stringResult.identical(numberResult)
    val numStr = numberResult.identical(stringResult)

    assertEquals(numberResult.value.toString, "222.0")
    assert(strNum)
    assert(numStr)
  }

  test("identical needs to return true for Hidden and Empty") {

    val hidEmp = Hidden.identical(Empty)
    val empHid = Empty.identical(Hidden)

    assert(hidEmp)
    assert(empHid)
  }

  test("identical needs to return false for Hidden and Hidden") {

    val hidHid = Hidden.identical(Hidden)

    assertEquals(false, hidHid)
  }

  test("identical needs to return true for Empty and Empty") {

    val empEmp = Empty.identical(Empty)

    assertEquals(true, empEmp)
  }

  test("StringResult and DateResult concatenation") {
    val stringResult = StringResult("Foo")

    val dateResult = DateResult(LocalDate.of(2020, 5, 23))

    val stringDate = stringResult + dateResult
    val dateString = dateResult + stringResult
    val dateDate = dateResult + dateResult

    assertEquals(stringDate, StringResult("Foo23 May 2020"))
    assertEquals(dateString, StringResult("23 May 2020Foo"))
    assertEquals(dateDate, StringResult("23 May 202023 May 2020"))
  }

  test("StringResult and PeriodResult concatenation") {
    val stringResult = StringResult("Foo")

    val periodResult = PeriodResult(Period.of(1, 1, 1))

    val stringPeriod = stringResult + periodResult
    val periodString = periodResult + stringResult
    val periodsDate = periodResult + periodResult

    assertEquals(stringPeriod, StringResult("FooP1Y1M1D"))
    assertEquals(periodString, StringResult("P1Y1M1DFoo"))
    assertEquals(periodsDate, PeriodResult(Period.of(2, 2, 2)))
  }

  test("applyTypeInfo should return updated expression result based on the text constraint") {
    val table = TableDrivenPropertyChecks.Table(
      ("input", "staticTypeData", "expected"),
      (
        NumberResult(BigDecimal(1)),
        StaticTypeData(ExprType.Number, Some(WholeSterling(true))),
        NumberResult(BigDecimal(1))
      ),
      (
        NumberResult(BigDecimal(1.1)),
        StaticTypeData(ExprType.Number, Some(WholeSterling(true))),
        NumberResult(BigDecimal(1))
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (input, staticTypeData, expected) =>
      assertEquals(input.applyTypeInfo(TypeInfo(FormCtx(FormComponentId("a")), staticTypeData)), expected)
    }
  }

  test("contains should work for ListResult with OptionResult (choice or revealing choice) types") {

    val table = TableDrivenPropertyChecks.Table(
      ("listResult", "containsValue", "expected"),
      (ListResult(List(OptionResult(Seq("0", "1")), OptionResult(Seq("2", "3")))), NumberResult(1), true),
      (ListResult(List(OptionResult(Seq("0", "1")), OptionResult(Seq("2")))), NumberResult(3), false)
    )

    TableDrivenPropertyChecks.forAll(table) { (listResult, containsValue, expected) =>
      assertEquals(listResult.contains(containsValue), expected)
    }
  }

  test("identical should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (
        ListResult(List(StringResult("A"), StringResult("B"))),
        ListResult(List(StringResult("A"), StringResult("B"))),
        true
      ),
      (ListResult(List(StringResult("A"), StringResult("B"))), ListResult(List(StringResult("A"))), false),
      (ListResult(List(NumberResult(1), NumberResult(2))), ListResult(List(NumberResult(1), NumberResult(2))), true),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(3), false),
      (
        ListResult(List(OptionResult(Seq("0", "1")), OptionResult(Seq("2", "3")))),
        ListResult(List(OptionResult(Seq("0", "1")), OptionResult(Seq("2", "3")))),
        true
      ),
      (
        ListResult(List(OptionResult(Seq("0", "1")), OptionResult(Seq("2")))),
        ListResult(List(OptionResult(Seq("0", "1")))),
        false
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs.identical(rhs), expected)
    }
  }

  test("> (greater than) should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(0), true),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(2), false)
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs > rhs, expected)
    }
  }

  test(">= (greater than or equals) should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(2), true),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(3), false)
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs >= rhs, expected)
    }
  }

  test("< (less than) should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(3), true),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(0), false)
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs < rhs, expected)
    }
  }

  test("<= (less than or equals) should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(1), true),
      (ListResult(List(NumberResult(1), NumberResult(2))), NumberResult(0), false)
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs <= rhs, expected)
    }
  }

  test("before should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (
        ListResult(List(DateResult(LocalDate.of(2020, 1, 1)), DateResult(LocalDate.of(2021, 1, 1)))),
        DateResult(LocalDate.of(2022, 1, 1)),
        true
      ),
      (
        ListResult(List(DateResult(LocalDate.of(2020, 1, 1)), DateResult(LocalDate.of(2021, 1, 1)))),
        DateResult(LocalDate.of(2019, 1, 1)),
        false
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs before rhs, expected)
    }
  }

  test("after should work for ListResult") {

    val table = TableDrivenPropertyChecks.Table(
      ("lhs", "rhs", "expected"),
      (
        ListResult(List(DateResult(LocalDate.of(2020, 1, 1)), DateResult(LocalDate.of(2021, 1, 1)))),
        DateResult(LocalDate.of(2019, 1, 1)),
        true
      ),
      (
        ListResult(List(DateResult(LocalDate.of(2020, 1, 1)), DateResult(LocalDate.of(2021, 1, 1)))),
        DateResult(LocalDate.of(2022, 1, 1)),
        false
      )
    )

    TableDrivenPropertyChecks.forAll(table) { (lhs, rhs, expected) =>
      assertEquals(lhs after rhs, expected)
    }
  }

  test("ListResult and NumberResult operations") {
    val nr1 = NumberResult(1)
    val nr2 = NumberResult(2)
    val lr12 = ListResult(List(nr1, nr2))

    assertEquals(nr2 + lr12, ListResult(List(NumberResult(3), NumberResult(4))))
    assertEquals(lr12 + nr2, ListResult(List(NumberResult(3), NumberResult(4))))
    assertEquals(nr2 - lr12, ListResult(List(NumberResult(1), NumberResult(0))))
    assertEquals(lr12 - nr2, ListResult(List(NumberResult(-1), NumberResult(0))))
    assertEquals(nr2 * lr12, ListResult(List(NumberResult(2), NumberResult(4))))
    assertEquals(lr12 * nr2, ListResult(List(NumberResult(2), NumberResult(4))))
    assertEquals(nr2 / lr12, ListResult(List(NumberResult(2), NumberResult(1))))
    assertEquals(lr12 / nr2, ListResult(List(NumberResult(0.5), NumberResult(1))))

    assertEquals(lr12 + Hidden, lr12)
    assertEquals(lr12 - Hidden, lr12)
    assertEquals(lr12 * Hidden, ListResult(List(NumberResult(0), NumberResult(0))))
    assertEquals(lr12 / Hidden, ListResult(List(NumberResult(0), NumberResult(0))))

    assertEquals(Hidden + lr12, lr12)
    assertEquals(Hidden - lr12, lr12)
    assertEquals(Hidden * lr12, ListResult(List(NumberResult(0), NumberResult(0))))
    assertEquals(Hidden / lr12, ListResult(List(NumberResult(0), NumberResult(0))))
  }

  test("ListResult and ListResult operations") {
    val nr1 = NumberResult(1)
    val nr2 = NumberResult(2)
    val nr3 = NumberResult(3)
    val lr12 = ListResult(List(nr1, nr2))
    val lr13 = ListResult(List(nr1, nr3))
    val lrH2 = ListResult(List(Hidden, nr2))
    val lrH3 = ListResult(List(Hidden, nr3))

    assertEquals(lr13 + lr12, ListResult(List(NumberResult(2), NumberResult(5))))
    assertEquals(lr13 - lr12, ListResult(List(NumberResult(0), NumberResult(1))))
    assertEquals(lr13 * lr12, ListResult(List(NumberResult(1), NumberResult(6))))
    assertEquals(lr13 / lr12, ListResult(List(NumberResult(1), NumberResult(1.5))))

    assertEquals(lr13 + lrH2, ListResult(List(NumberResult(1), NumberResult(5))))
    assertEquals(lr13 - lrH2, ListResult(List(NumberResult(1), NumberResult(1))))
    assertEquals(lr13 * lrH2, ListResult(List(NumberResult(0), NumberResult(6))))
    assertEquals(lr13 / lrH2, ListResult(List(NumberResult(0), NumberResult(1.5))))

    assertEquals(lrH3 + lr12, ListResult(List(NumberResult(1), NumberResult(5))))
    assertEquals(lrH3 - lr12, ListResult(List(NumberResult(1), NumberResult(1))))
    assertEquals(lrH3 * lr12, ListResult(List(NumberResult(0), NumberResult(6))))
    assertEquals(lrH3 / lr12, ListResult(List(NumberResult(0), NumberResult(1.5))))

  }
}
