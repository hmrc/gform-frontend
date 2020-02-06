/*
 * Copyright 2020 HM Revenue & Customs
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

import org.scalatest.{ FlatSpec, Matchers }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class DateValidationLogicSpec(implicit messages: Messages) extends FlatSpec with Matchers {

  "DateValidationLogic.incorrectDate with precisely yyyy-04-dd" should "return message: must be in April" in {

    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Exact(4), Day.Any), OffsetDate(0)) shouldBe
      "must be in April"
  }

  "DateValidationLogic.incorrectDate with precisely next-mm-dd" should "return message must be in April" in {
    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Next, Month.Any, Day.Any), OffsetDate(0)) shouldBe
      s"must be in ${DateValidationLogic.getNextYear}"
  }

  "DateValidationLogic.incorrectDate with precisely previous-mm-dd" should "return message must be in April" in {
    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Previous, Month.Any, Day.Any), OffsetDate(0)) shouldBe
      s"must be in ${DateValidationLogic.getPreviousYear}"
  }

  "DateValidationLogic.incorrectDate with precisely yyyy-mm-firstDay" should "return message: must be the first day of the month" in {
    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Any, Day.First), OffsetDate(0)) shouldBe
      s"must be the first day of the month"
  }

  "DateValidationLogic.incorrectDate with precisely yyyy-mm-lastDay" should "return message: must be the last day of the month" in {
    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Last), OffsetDate(0)) shouldBe
      s"must be the last day of the month"
  }

  "DateValidationLogic.incorrectDate with precisely 2018-mm-dd" should "return message: must be in 2018" in {
    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Exact(2018), Month.Any, Day.Any), OffsetDate(0)) shouldBe
      s"must be in 2018"
  }

  "DateValidationLogic.incorrectDate with precisely yyyy-mm-3" should "return message: must be the third day of the month" in {
    DateValidationLogic
      .incorrectDateMessage(Precisely, ConcreteDate(Year.Any, Month.Any, Day.Exact(3)), OffsetDate(0)) shouldBe
      s"must be the 3rd of any month"
  }

}
