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

package uk.gov.hmrc.gform.testonly.filter

import cats.syntax.all._
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }

import java.time.LocalDateTime
import scala.util.Try

object DateTimeValidator {
  private def validateDay(day: String, id: String): Validated[Map[String, String], Int] =
    if (day.matches("[0-9]+")) {
      val d = day.toInt
      if (d > 31) {
        Invalid(Map(id -> s"Day cannot be greater then 31"))
      } else {
        Valid(d)
      }
    } else Invalid(Map(id -> s"Not a valid day: $day"))

  private def validateMonth(month: String, id: String): Validated[Map[String, String], Int] =
    if (month.matches("[0-9]+")) {
      val m = month.toInt
      if (m > 12) {
        Invalid(Map(id -> s"Month cannot be greater then 12"))
      } else {
        Valid(m)
      }
    } else Invalid(Map(id -> s"Not a valid month: $month"))

  private def validateYear(year: String, id: String): Validated[Map[String, String], Int] =
    if (year.matches("[0-9]+")) {
      val y = year.toInt
      if (y < 2000) {
        Invalid(Map(id -> s"Year must be 2000 or more"))
      } else if (y > 2099) {
        Invalid(Map(id -> s"Year must less then 2099"))
      } else {
        Valid(y)
      }
    } else Invalid(Map(id -> s"Not a valid year: $year"))

  private def validateHour(hour: String, id: String): Validated[Map[String, String], Int] =
    if (hour.matches("[0-9]+")) {
      val h = hour.toInt
      if (h > 23) {
        Invalid(Map(id -> s"Hour cannot be greater then 23"))
      } else {
        Valid(h)
      }
    } else Invalid(Map(id -> s"Not a valid hour: $hour"))

  private def validateMinute(minute: String, id: String): Validated[Map[String, String], Int] =
    if (minute.matches("[0-9]+")) {
      val h = minute.toInt
      if (h > 59) {
        Invalid(Map(id -> s"Minute cannot be greater then 59"))
      } else {
        Valid(h)
      }
    } else Invalid(Map(id -> s"Not a valid minute: $minute"))

  private def validateLocalDateTime(
    prefix: String,
    day: Int,
    month: Int,
    year: Int,
    hour: Int,
    minute: Int
  ): Validated[Map[String, String], LocalDateTime] = {
    val localDateTime = Try(LocalDateTime.of(year, month, day, hour, minute))
    localDateTime.fold(
      error => Invalid(Map(prefix + "-day" -> error.getMessage)),
      localDateTime => Valid(localDateTime)
    )
  }

  def validateUserInput(
    prefix: String,
    input: DateTimeUserInput
  ): Validated[Map[String, String], Option[LocalDateTime]] =
    (input.day, input.month, input.year, input.hour, input.minute) match {
      case (Some(day), Some(month), Some(year), Some(hour), Some(minute)) =>
        (
          validateDay(day, prefix + "-day"),
          validateMonth(month, prefix + "-month"),
          validateYear(year, prefix + "-year"),
          validateHour(hour, prefix + "-hour"),
          validateMinute(minute, prefix + "-minute")
        ).tupled
          .andThen { case (day, month, year, hour, minute) =>
            validateLocalDateTime(prefix, day, month, year, hour, minute)
          }
          .map(localDateTime => Some(localDateTime))
      case (None, None, None, None, None) => Valid(None)
      case (None, _, _, _, _)             => Invalid(Map(prefix + "-day" -> "Missing day"))
      case (_, None, _, _, _)             => Invalid(Map(prefix + "-month" -> "Missing month"))
      case (_, _, None, _, _)             => Invalid(Map(prefix + "-year" -> "Missing year"))
      case (_, _, _, None, _)             => Invalid(Map(prefix + "-hour" -> "Missing hour"))
      case (_, _, _, _, None)             => Invalid(Map(prefix + "-minute" -> "Missing minute"))
    }

  def mayBeErrors(validated: Option[Validated[Map[String, String], Option[LocalDateTime]]]): Map[String, String] =
    validated match {
      case Some(Invalid(e)) => e
      case _                => Map.empty[String, String]
    }

}
