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
import java.util.Locale
import cats.implicits._

object TimeFormatter {

  def normalizeLocalTime(localTime: LocalTime): String = {
    val formatter = DateTimeFormatter.ofPattern("h:mma")
    val formattedTime = localTime.format(formatter)
    formattedTime.replace("AM", "am").replace("PM", "pm")
  }

  def maybeLocalTime(time: String): Option[LocalTime] = {
    val timeNormalized = if (time === "0") {
      ""
    } else {
      time
        .toUpperCase()
        .replaceAll("\\s+", "")
        .replaceAll("A\\.", "A")
        .replaceAll("P\\.", "P")
        .replaceAll("AM\\.", "AM")
        .replaceAll("PM\\.", "PM")
        .replaceAll("Y\\.", "Y")
        .replaceAll("YB\\.", "AM")
        .replaceAll("YP\\.", "PM")
        .replace("YB", "AM")
        .replace("yb", "AM")
        .replace("YP", "PM")
        .replace("yp", "PM")
    }
    val patterns = List(
      "H",
      "HH",
      "H:mm",
      "h:mma",
      "h.mma",
      "h:a",
      "h.a",
      "ha",
      "hha",
      "HHa",
      "H.",
      "Hmm",
      "hmma",
      "hmm.a",
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
        Either
          .catchOnly[DateTimeParseException](LocalTime.parse(timeNormalized, formatter.withLocale(Locale.ENGLISH)))
          .toOption
      )
    )
  }

  def isNoonConfusing(time: LocalTime, timeStr: String): Boolean = {
    val noon = LocalTime.NOON
    val isInputConfusing = timeStr.replaceAll("[\\s.:]", "").stripSuffix("00") === "12"
    time.equals(noon) && isInputConfusing
  }

  def isNoonRangeConfusing(time: LocalTime, timeStr: String): Boolean = {
    val startRange = LocalTime.NOON.minusMinutes(1)
    val endRange = LocalTime.of(13, 0)
    val normalizedInput = timeStr.replaceAll("[\\s.:]", "")

    time.isAfter(startRange) && time.isBefore(endRange) && "^12\\d{2}$".r.matches(normalizedInput)
  }
}
