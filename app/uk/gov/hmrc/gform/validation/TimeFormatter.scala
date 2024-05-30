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

import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import cats.implicits._

object TimeFormatter {

  def normalizeLocalTime(localTime: LocalTime): String = {
    val formatter = DateTimeFormatter.ofPattern("hh:mma")
    val formattedTime = localTime.format(formatter)
    formattedTime.replace("AM", "am").replace("PM", "pm")
  }

  def maybeLocalTime(time: String): Option[LocalTime] = {
    val timeNormalized = time.toUpperCase().replaceAll("\\s+", "")
    val patterns = List(
      "HH",
      "hha",
      "HHa",
      "HH:mm",
      "HH.mm",
      "HHmm",
      "hh:mma",
      "hh.mma",
      "hhmma",
      "HH:mma",
      "HH.mma",
      "HHmma"
    )
    val formatters = patterns.map(DateTimeFormatter.ofPattern)
    formatters.collectFirst(
      Function.unlift(formatter =>
        Either.catchOnly[DateTimeParseException](LocalTime.parse(timeNormalized, formatter)).toOption
      )
    )
  }

  def isNoonConfusing(time: LocalTime, timeStr: String): Boolean = {
    val noon = LocalTime.NOON
    val isInputConfusing = timeStr.replaceAll("[\\s.:]", "") === "12"
    time.equals(noon) && isInputConfusing
  }

  def isNoonRangeConfusing(time: LocalTime, timeStr: String): Boolean = {
    val startRange = LocalTime.NOON.minusMinutes(1)
    val endRange = LocalTime.of(13, 0)
    val normalizedInput = timeStr.replaceAll("[\\s.:]", "")

    time.isAfter(startRange) && time.isBefore(endRange) && "^12\\d{2}$".r.matches(normalizedInput)
  }
}
