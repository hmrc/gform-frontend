/*
 * Copyright 2019 HM Revenue & Customs
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
import java.text.DateFormatSymbols
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.implicits._
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedNumeric

import scala.util.{ Failure, Success, Try }

object DateValidationLogic {

  def getLastDayOfMonth(year: Int, month: Int): Int = LocalDate.of(year, month, 1).lengthOfMonth

  def getNextYear: Int = LocalDate.now().getYear + 1

  def getPreviousYear: Int = LocalDate.now().getYear - 1

  def getMonthName(month: Int): String = new DateFormatSymbols().getMonths.toList(month - 1)

  def ordinalAbbrev(n: Int): String = {
    val ans = "th"
    if (n % 100 / 10 == 1) ans
    else
      (n % 10) match {
        case 1 => "st"
        case 2 => "nd"
        case 3 => "rd"
        case _ => ans
      }
  }

  def exactParameterListToString(parameters: List[ExactParameter]): String =
    parameters
      .map {
        case ExactYear(year)   => year.toString
        case ExactMonth(month) => getMonthName(month)
        case ExactDay(day)     => day + ordinalAbbrev(day)
      }
      .mkString(" ")

  def getDay(year: Int, month: Int, day: Day): Int = day match {
    case ExactDay(d) => d
    case FirstDay    => 1
    case LastDay     => getLastDayOfMonth(year, month)
  }

  def getYear(year: Year): Int = year match {
    case ExactYear(y) => y
    case Next         => getNextYear
    case Previous     => getPreviousYear
  }

  def isExactDay(day: Day): Boolean = day match {
    case _: ExactDay => true
    case _           => false
  }

  def exactConcreteDateToLocalDate(concreteDate: ConcreteDate): LocalDate = concreteDate match {

    case date @ ConcreteDate(exactYear: Year, exactMonth: ExactMonth, exactDay: Day) if date.isExact =>
      LocalDate.of(getYear(exactYear), exactMonth.month, getDay(getYear(exactYear), exactMonth.month, exactDay))

  }

  def localDateToConcreteDate(localDate: LocalDate): ConcreteDate =
    ConcreteDate(localDate.getYear, localDate.getMonthValue, localDate.getDayOfMonth)

  def incorrectDateMessage(
    beforeAfterPrecisely: BeforeAfterPrecisely,
    concreteDate: ConcreteDate,
    offsetDate: OffsetDate): String = {

    val govDateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy")
    val dateWithOffset = (localDate: LocalDate, offset: OffsetDate) =>
      localDate.plusDays(offset.value.toLong).format(govDateFormat)

    val yearString = concreteDate.year match {
      case ExactYear(year) if concreteDate.month == AnyMonth => s"in $year "
      case ExactYear(year) if concreteDate.month != AnyMonth => s"$year "
      case Next                                              => s"in $getNextYear "
      case Previous                                          => s"in $getPreviousYear "
      case _                                                 => ""
    }

    val monthString = concreteDate.month match {
      case ExactMonth(month) if !isExactDay(concreteDate.day)               => s"in ${getMonthName(month)} "
      case ExactMonth(month) if isExactDay(concreteDate.day)                => s"of ${getMonthName(month)} "
      case _ if isExactDay(concreteDate.day)                                => "of any month "
      case _ if concreteDate.day == FirstDay || concreteDate.day == LastDay => "of the month "
      case _                                                                => ""
    }

    val dayString = concreteDate.day match {
      case ExactDay(day) => s"the $day${ordinalAbbrev(day)} "
      case FirstDay      => "the first day "
      case LastDay       => "the last day "
      case _             => ""
    }

    val result = concreteDate match {
      case date if date.isExact =>
        s"must be ${beforeAfterPrecisely.mkString} ${dateWithOffset(exactConcreteDateToLocalDate(concreteDate), offsetDate)}"
      case _ => s"must be $dayString$monthString$yearString"

    }
    result.trim
  }

  def isNumeric(str: String, timeUnitLabel: String, label: String): ValidatedNumeric =
    if (str.isEmpty) Invalid(s"$label must be entered")
    else
      Try(str.toInt) match {
        case Success(x) => Valid(x)
        case Failure(_) => Invalid(s"$timeUnitLabel must be numeric")
      }

  def isWithinBounds(number: Int, dayOrMonth: Int, label: String): ValidatedNumeric =
    number match {
      case x if number <= dayOrMonth => Valid(number)
      case y if number > dayOrMonth  => Invalid(s"$label must not be greater than $dayOrMonth")
    }

  def hasValidNumberOfDigits(number: Int, digits: Int, label: String): ValidatedNumeric =
    number.toString.length match {
      case x if x == digits => Valid(number)
      case y if y != digits => Invalid(s"$label must be a $digits digit number")
    }

  def parallelWithApplicative[E: Semigroup](v1: Validated[E, Int], v2: Validated[E, Int], v3: Validated[E, Int])(
    f: (Int, Int, Int) => ConcreteDate): Validated[E, ConcreteDate] = (v3, v2, v1).mapN(f)
}
