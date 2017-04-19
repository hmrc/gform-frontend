/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.models.components

import java.time.LocalDate
import uk.gov.hmrc.bforms.typeclasses.Now

final case class DateExpr(year: Int, month: Int, day: Int)

object DateExpr {

  def fromDateValue(dv: DateValue): DateExpr = dv match {
    case TodayDateValue => todayDateExpr
    case ExactDateValue(year, month, day) => DateExpr(year, month, day)
    case n @ NextDateValue(_, _) => nextDateExpr(n)
    case p @ PreviousDateValue(_, _) => previousDateExpr(p)
  }

  private def nextDateExpr(nextDate: NextDateValue)(implicit now: Now[LocalDate]): DateExpr = {

    import nextDate._

    val year = now.apply().getYear

    val nextDateYear =
      LocalDate.of(year, month, day).isAfter(now.apply()) match {
        case true => year
        case false => year + 1
      }

    DateExpr(nextDateYear, month, day)
  }

  private def previousDateExpr(previousDate: PreviousDateValue)(implicit now: Now[LocalDate]): DateExpr = {

    import previousDate._

    val year = now.apply().getYear

    val previousDateYear =
      LocalDate.of(year, month, day).isAfter(now.apply()) match {
        case true => year - 1
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
