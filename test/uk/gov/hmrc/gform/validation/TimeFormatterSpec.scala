/*
 * Copyright 2024 HM Revenue & Customs
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

import uk.gov.hmrc.gform.Spec

import java.time.LocalTime

class TimeFormatterSpec extends Spec {
  ".normalizeLocalTime" should "remove the leading 0 if there is only one digit for the hour" in {
    val normalisedTime: String = TimeFormatter.normalizeLocalTime(LocalTime.parse("09:00"))
    val expectedTime: String = "9:00am"

    normalisedTime shouldBe expectedTime
  }

  it should "return the time as a 12-hour clock followed by am when appropriate" in {
    val normalisedTime: String = TimeFormatter.normalizeLocalTime(LocalTime.parse("05:00"))
    val expectedTime: String = "5:00am"

    normalisedTime shouldBe expectedTime
  }

  it should "return the time as a 12-hour clock followed by pm when appropriate" in {
    val normalisedTime: String = TimeFormatter.normalizeLocalTime(LocalTime.parse("15:00"))
    val expectedTime: String = "3:00pm"

    normalisedTime shouldBe expectedTime
  }

  ".maybeLocalTime" should "accept a number from 1-9 as a valid time" in {
    val maybeLocalTime: Option[LocalTime] = TimeFormatter.maybeLocalTime("3")
    val expectedTime: Option[LocalTime] = Some(LocalTime.parse("03:00"))

    maybeLocalTime shouldBe expectedTime
  }

  ".maybeLocalTime" should "not accept the number 0 as a valid time" in {
    val maybeLocalTime: Option[LocalTime] = TimeFormatter.maybeLocalTime("0")
    val expectedTime: Option[LocalTime] = None

    maybeLocalTime shouldBe expectedTime
  }
}
