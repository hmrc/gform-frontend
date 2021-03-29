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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import java.time.{ ZoneId, ZonedDateTime }
import org.scalacheck.Gen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DateExprValue, ExactDateExprValue, TodayDateExprValue }

trait DateExprValueGen {

  val zoneId = ZoneId.of("Europe/London")

  def todayDateExprValueGen: Gen[TodayDateExprValue.type] = Gen.const(TodayDateExprValue)
  def exactDateExprValueGen: Gen[ExactDateExprValue] = PrimitiveGen.instantGen.flatMap { instant =>
    val zonedDateTime: ZonedDateTime = instant.atZone(zoneId)
    val year = zonedDateTime.getYear()
    val day = zonedDateTime.getDayOfMonth()
    val month = zonedDateTime.getMonthValue()
    Gen.const(ExactDateExprValue(year, month, day))
  }

  def dateExprValueGen: Gen[DateExprValue] =
    Gen.oneOf(todayDateExprValueGen, exactDateExprValueGen)
}

object DateExprValueGen extends DateExprValueGen
