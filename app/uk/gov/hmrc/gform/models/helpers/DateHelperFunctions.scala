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

package uk.gov.hmrc.gform.models.helpers

import scala.util.{ Failure, Success, Try }
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.Month

import uk.gov.hmrc.gform.models.DateExpr
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Offset

object DefaultDateFormatter {
  val formatPattern = "yyyy-MM-dd"
  val dateFormatter = DateTimeFormatter.ofPattern(formatPattern)
}

object DateHelperFunctions {

  import DefaultDateFormatter._

  def convertToDateExpr(localDate: LocalDate): DateExpr = {
    val splitBy = (str: String) => str.split("-")

    val dateToStr = dateFormatter.format(localDate)
    val dateArray = splitBy(dateToStr)

    DateExpr(dateArray(0).toInt, dateArray(1).toInt, dateArray(2).toInt)
  }

  def withOffset(offset: Offset, dateExpr: DateExpr): DateExpr = offset.value match {
    case 0 => dateExpr // merge cases
    case nonZero =>
      val zeroPadding = (x: Int) => "%02d".format(x)
      val dateExprToString = (dateExpr: DateExpr) =>
        s"${dateExpr.year}-${zeroPadding(dateExpr.month)}-${zeroPadding(dateExpr.day)}"

      val dateWithOffset = LocalDate
        .parse(dateExprToString(dateExpr), dateFormatter)
        .plusDays(nonZero.toLong)

      convertToDateExpr(dateWithOffset)
  }

  def toCamelCase(month: String): String = {
    val m = month
    val restValue = month.drop(1)

    s"${m.charAt(0)}${restValue.toLowerCase}"
  }

  val renderMonth: String => String = (str: String) =>
    Try(str.toInt) match {
      case Success(month) => month.toString
      case Failure(_)     => ""
    }

  val getMonthValue: String => String = (str: String) =>
    Try(str.toInt) match {
      case Success(month) => toCamelCase(Month.values().apply(month - 1).toString)
      case Failure(_)     => ""
    }

}
