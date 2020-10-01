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

package uk.gov.hmrc.gform.models

import java.time.LocalDate

import uk.gov.hmrc.gform.models.helpers.DefaultDateFormatter.dateFormatter
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.typeclasses.Now

final case class DateExpr(year: Int, month: Int, day: Int) {
  def valueForAtom(atom: Atom): String = atom match {
    case Date.day   => day.toString
    case Date.month => month.toString
    case Date.year  => year.toString
    case _          => ""
  }
}

object DateExpr {

  def fromDateValue(dv: DateValue): DateExpr = dv match {
    case TodayDateValue                   => todayDateExpr
    case ExactDateValue(year, month, day) => DateExpr(year, month, day)
    case n @ NextDateValue(_, _)          => nextDateExpr(n)
    case p @ PreviousDateValue(_, _)      => previousDateExpr(p)

  }

  private def nextDateExpr(nextDate: NextDateValue)(implicit now: Now[LocalDate]): DateExpr = {

    import nextDate._

    val year = now.apply().getYear

    val nextDateYear =
      LocalDate.of(year, month, day).isAfter(now.apply()) match {
        case true  => year
        case false => year + 1
      }

    DateExpr(nextDateYear, month, day)
  }

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
        dateExpr.year + "-" + zeroPadding(dateExpr.month) + "-" + zeroPadding(dateExpr.day)

      val dateWithOffset = LocalDate
        .parse(dateExprToString(dateExpr), dateFormatter)
        .plusDays(nonZero.toLong)

      convertToDateExpr(dateWithOffset)
  }

  private def previousDateExpr(previousDate: PreviousDateValue)(implicit now: Now[LocalDate]): DateExpr = {

    import previousDate._

    val year = now.apply().getYear

    val previousDateYear =
      LocalDate.of(year, month, day).isAfter(now.apply()) match {
        case true  => year - 1
        case false => year
      }

    DateExpr(previousDateYear, month, day)
  }

  private def todayDateExpr(implicit now: Now[LocalDate]): DateExpr = {
    val n = now.apply()
    val year = n.getYear()
    val month = n.getMonthValue()
    val day = n.getDayOfMonth()
    DateExpr(year, month, day)
  }

}
