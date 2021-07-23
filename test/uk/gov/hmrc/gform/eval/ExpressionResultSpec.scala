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

import munit.FunSuite
import play.api.i18n.Messages
import play.api.test.Helpers

import java.time.LocalDate
import uk.gov.hmrc.gform.eval.ExpressionResult.{ DateResult, NumberResult, StringResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DateCtx, DateValueExpr, TodayDateExprValue }

class ExpressionResultSpec extends FunSuite {

  private val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.January" -> "January"
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
}
