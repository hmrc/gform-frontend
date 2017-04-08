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

package uk.gov.hmrc.bforms.core.utils

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import uk.gov.hmrc.bforms.models.{DateExpr, Offset}

/**
  * Created by dimitra on 05/04/17.
  */

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

    DateExpr(dateArray(2).toInt, dateArray(1).toInt, dateArray(0).toInt)
  }

  def adjustDate(offset: Offset, optionalDateExpr: Option[DateExpr]): Option[DateExpr] = {

    (offset.value, optionalDateExpr) match {
      case (0, Some(dateExpr)) => Some(dateExpr)
      case (nonZero, Some(dateExpr)) =>
        val dateExprToString = (dateExpr: DateExpr) => dateExpr.year + "-" + dateExpr.month + "-" + dateExpr.day

        val dateExprAsLocalDate: LocalDate = LocalDate.parse(dateExprToString(dateExpr), dateFormatter)
        val dateWithOffset: LocalDate = dateExprAsLocalDate.plusDays(nonZero)

        Some(convertToDateExpr(dateWithOffset))

      case (_, None) => None
    }

  }

}
