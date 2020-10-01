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

package uk.gov.hmrc.gform.validation
import java.text.DateFormatSymbols
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.implicits._
import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedNumeric
import uk.gov.hmrc.gform.views.html.localisedDateString

import scala.util.{ Failure, Success, Try }

object DateValidationLogic {

  def getLastDayOfMonth(year: Int, month: Int): Int = LocalDate.of(year, month, 1).lengthOfMonth

  def getNextYear: Int = LocalDate.now().getYear + 1

  def getPreviousYear: Int = LocalDate.now().getYear - 1

  def getMonthName(month: Int)(implicit messages: Messages): String = {
    val monthKey = new DateFormatSymbols().getMonths.toList(month - 1)
    messages(s"date.$monthKey")
  }

  def exactConcreteDateToLocalDate(concreteDate: ConcreteDate): Option[LocalDate] = concreteDate match {
    case ConcreteDate(HasYear(year), HasMonth(month), HasDay(day)) => day(year)(month).some
    case _                                                         => none
  }

  def localDateToConcreteDate(localDate: LocalDate): ConcreteDate =
    ConcreteDate(
      Year.Exact(localDate.getYear),
      Month.Exact(localDate.getMonthValue),
      Day.Exact(localDate.getDayOfMonth))

  def incorrectDateMessage(
    beforeAfterPrecisely: BeforeAfterPrecisely,
    concreteDate: ConcreteDate,
    offsetDate: OffsetDate)(implicit messages: Messages): MessageKeyWithVars = {

    val govDateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy")
    val dateWithOffset = (localDate: LocalDate, offset: OffsetDate) =>
      localisedDateString(localDate.plusDays(offset.value.toLong).format(govDateFormat))

    // format: off
    val yearString = concreteDate.year match {
      case Year.Exact(year) if concreteDate.month.isAny && concreteDate.day.isAny => messages("date.inYear", year.toString)
      case Year.Exact(year) if concreteDate.month.isAny                           => messages("date.inYear", year.toString)
      case Year.Exact(year) if !concreteDate.month.isAny                          => s"$year "
      case Year.Next if concreteDate.month.isAny && concreteDate.day.isAny        => messages("date.inYear", messages("date.year") + " " + getNextYear.toString)
      case Year.Next                                                              => messages("date.inYear", getNextYear.toString)
      case Year.Previous if concreteDate.month.isAny && concreteDate.day.isAny    => messages("date.inYear", messages("date.year") + " " + getPreviousYear.toString)
      case Year.Previous                                                          => messages("date.inYear", getPreviousYear.toString)
      case _                                                                      => ""
    }
    // format: on

    // format: off
    val monthString = concreteDate.month match {
      case Month.Exact(month) if concreteDate.day.isAny             => messages("date.inMonth", getMonthName(month))
      case Month.Exact(month) if concreteDate.day.isExact           => messages("date.ofMonth", getMonthName(month))
      case _ if concreteDate.day.isExact                            => messages("date.ofAnyMonth")
      case _ if concreteDate.day.isFirst || concreteDate.day.isLast => messages("date.ofTheMonth")
      case _                                                        => ""
    }
    // format: on

    val dayString = concreteDate.day match {
      case Day.Exact(day) => messages("date.exactDay", s"$day${messages(s"date.ordinal.$day")}")
      case Day.First      => messages("date.firstDay")
      case Day.Last       => messages("date.lastDay")
      case _              => ""
    }

    val vars: List[String] = exactConcreteDateToLocalDate(concreteDate) match {
      case Some(localDate) => dateWithOffset(localDate, offsetDate) :: Nil
      case _               => s"$dayString $monthString $yearString" :: Nil
    }

    val result = s"date.${beforeAfterPrecisely.mkString}"

    MessageKeyWithVars(result.trim, Some(vars))
  }

  case class MessageKeyWithVars(
    messageKey: String,
    vars: Option[List[String]]
  )

  def hasMaximumLength(str: String, maximumLength: Int, label: String)(
    implicit messages: Messages): Validated[String, String] =
    if (str.length > maximumLength) Invalid(messages("generic.error.maxLength", label, maximumLength))
    else Valid(str)

  def isNumeric(str: String, timeUnitLabel: String, label: String)(implicit messages: Messages): ValidatedNumeric =
    if (str.isEmpty) Invalid(messages("field.error.required", label))
    else
      Try(str.toInt) match {
        case Success(x) => Valid(x)
        case Failure(_) => Invalid(messages("field.error.number", timeUnitLabel))
      }

  def isWithinBounds(number: Int, dayOrMonth: Int, label: String)(implicit messages: Messages): ValidatedNumeric =
    number match {
      case x if number <= dayOrMonth => Valid(number)
      case y if number > dayOrMonth  => Invalid(messages("field.error.notGreaterThan", label, dayOrMonth))
    }

  def hasValidNumberOfDigits(number: Int, digits: Int, label: String)(implicit messages: Messages): ValidatedNumeric =
    number.toString.length match {
      case x if x === digits => Valid(number)
      case y if y =!= digits => Invalid(messages("field.error.exactDigits", label, digits))
    }

  def parallelWithApplicative[E: Semigroup, A](v1: Validated[E, Int], v2: Validated[E, Int], v3: Validated[E, Int])(
    f: (Int, Int, Int) => A): Validated[E, A] = (v3, v2, v1).mapN(f)
}

object HasYear {
  def unapply(year: Year): Option[Int] = year match {
    case Year.Exact(y) => y.some
    case Year.Next     => DateValidationLogic.getNextYear.some
    case Year.Previous => DateValidationLogic.getPreviousYear.some
    case Year.Any      => none
  }
}

object HasMonth {
  def unapply(month: Month): Option[Int] = month match {
    case Month.Exact(y) => y.some
    case Month.Any      => none
  }
}

object HasDay {
  private def toLocalDate(day: Int) = (year: Int) => (month: Int) => LocalDate.of(year, month, day)
  def unapply(day: Day): Option[Int => Int => LocalDate] = day match {
    case Day.Exact(y) => toLocalDate(y).some
    case Day.First    => toLocalDate(1).some
    case Day.Last =>
      ((year: Int) => (month: Int) => LocalDate.of(year, month, DateValidationLogic.getLastDayOfMonth(year, month))).some
    case Day.Any => none
  }
}
