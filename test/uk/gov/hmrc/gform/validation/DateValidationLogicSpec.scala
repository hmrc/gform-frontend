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

package uk.gov.hmrc.gform.validation

import munit.FunSuite
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.DateValidationLogic.MessageKeyWithVars

class DateValidationLogicSpec extends FunSuite {

  implicit val messages: Messages = Helpers.stubMessages(
    Helpers.stubMessagesApi(
      Map(
        "en" -> Map(
          "date.inMonth"    -> "in {0}",
          "date.April"      -> "April",
          "date.inYear"     -> "in {0}",
          "date.year"       -> "year",
          "date.firstDay"   -> "the first day",
          "date.ofTheMonth" -> "of the month",
          "date.lastDay"    -> "the last day",
          "date.exactDay"   -> "the {0}",
          "date.ofAnyMonth" -> "of any month",
          "date.ordinal.3"  -> "rd"
        )
      )
    )
  )

  test("DateValidationLogic.incorrectDate with precisely yyyy-04-dd should return message: in April") {
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Exact(4), Day.Any), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(" in April ")))
    )
  }

  test("DateValidationLogic.incorrectDate with precisely next-mm-dd should return message in year 202x") {
    val nextYear = DateValidationLogic.getNextYear
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Next, Month.Any, Day.Any), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(s"  in year $nextYear")))
    )
  }

  test("DateValidationLogic.incorrectDate with precisely previous-mm-dd should return message in year 202x") {
    val previousYear = DateValidationLogic.getPreviousYear
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Previous, Month.Any, Day.Any), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(s"  in year $previousYear")))
    )
  }

  test(
    "DateValidationLogic.incorrectDate with precisely yyyy-mm-firstDay should return message: the first day of the month"
  ) {
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Any, Day.First), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(s"the first day of the month ")))
    )
  }

  test(
    "DateValidationLogic.incorrectDate with precisely yyyy-mm-lastDay should return message: the last day of the month"
  ) {
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(s"the last day of the month ")))
    )
  }

  test("DateValidationLogic.incorrectDate with precisely 2018-mm-dd should return message: must be in 2018") {
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Exact(2018), Month.Any, Day.Any), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(s"  in 2018")))
    )
  }

  test(
    "DateValidationLogic.incorrectDate with precisely yyyy-mm-3 should return message: the 3rd day of any month"
  ) {
    assertEquals(
      DateValidationLogic
        .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Exact(3)), OffsetDate(0)),
      MessageKeyWithVars("date.precisely", Some(List(s"the 3rd of any month ")))
    )
  }
}
