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

package uk.gov.hmrc.gform.eval

import java.time.Period
import munit.FunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Messages
import play.api.test.Helpers

import java.time.LocalDate
import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, NumberResult, PeriodResult, StringResult }
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

  test("identical need return true for Number(222.00) and Constant(222)") {
    val stringResult = StringResult("222")

    val numberResult = NumberResult(222.00)
    val strNum = stringResult.identical(numberResult)
    val numStr = numberResult.identical(stringResult)

    assertEquals(numberResult.value.toString, "222.0")
    assert(strNum)
    assert(numStr)
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
}
