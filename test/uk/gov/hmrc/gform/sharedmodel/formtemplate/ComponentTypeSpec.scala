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

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.ComponentTypeGen

import java.time.LocalTime

class ComponentTypeSpec extends Spec {
  "ComponentType" should "round trip derived JSON" in {
    forAll(ComponentTypeGen.componentTypeGen()) { obj =>
      ComponentType.format.reads(ComponentType.format.writes(obj)) should beJsSuccess(obj)
    }
  }

  "Range" should "format times without a space between the numbers and the am/pm" in {
    val formattedTime = Range.twelveHoursFormat.format(LocalTime.parse("01:45")).replace("AM", "am")

    formattedTime shouldBe "01:45am"
  }

  it should "produce a list of time slots without a space between the numbers and the am/pm" in {
    val startTime = StartTime(LocalTime.parse("00:00"))
    val endTime = EndTime(LocalTime.parse("23:59"))
    val range = Range(startTime, endTime)
    val intervalMins = IntervalMins(15)
    val time = Time(List(range), intervalMins)

    val formattedTimes = Range.timeSlots(time)

    formattedTimes.count(timeSlot => timeSlot.contains(" ")) shouldBe 0
  }

  it should "produce a list of time slots with am/pm all lowercase" in {
    val startTime = StartTime(LocalTime.parse("00:00"))
    val endTime = EndTime(LocalTime.parse("23:59"))
    val range = Range(startTime, endTime)
    val intervalMins = IntervalMins(15)
    val time = Time(List(range), intervalMins)

    val formattedTimes = Range.timeSlots(time)

    formattedTimes.count(timeSlot => timeSlot.endsWith("am") || timeSlot.endsWith("pm")) shouldBe formattedTimes.length
    formattedTimes.count(timeSlot => timeSlot.endsWith("AM") || timeSlot.endsWith("PM")) shouldBe 0
  }
}
