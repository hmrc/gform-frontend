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
import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ Validated, ValidatedNec }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import cats.implicits._
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedConcreteDate, ValidatedLocalDate }

sealed trait DateError {
  def errorMessage: String
}

case object DayValueOutOfRange extends DateError {
  def errorMessage: String = "Day must be greater than 0 and less than or equal to 31."
}

case object MonthValueOutOfRange extends DateError {
  def errorMessage: String = "Month must be greater than 0 and less than or equal to 12."
}

case object YearValueTooManyDigits extends DateError {
  def errorMessage: String = "Year must be 4 digits."
}

sealed trait DateValidatorNec {

  type ValidationResult[A] = ValidatedNec[DateError, A]

  def validateDay(day: Day): Validated[GformError, Day] = day match {

    case ExactDay(dayOfMonth) =>
      if (dayOfMonth > 0 && dayOfMonth <= 31) day.valid else Map[FormComponentId, Set[String]]().invalid //DayValueOutOfRange.invalidNec

    case otherDay => otherDay.valid

  }

  def validateMonth(month: Month): Validated[GformError, Month] = month match {
    case ExactMonth(monthOfYear) =>
      if (monthOfYear > 0 && monthOfYear <= 12) month.valid else Map[FormComponentId, Set[String]]().invalid //MonthValueOutOfRange.invalid

    case otherMonth => otherMonth.valid

  }

  def validateYear(year: Year): Validated[GformError, Year] = year match {

    case ExactYear(yearValue) =>
      if (yearValue.toString.length == 4) Valid(year) else Map[FormComponentId, Set[String]]().invalid //Invalid(YearValueTooManyDigits) //.invalid

    case otherYear => Valid(otherYear)

  }

  def validateConcreteDate(concreteDate: ConcreteDate): Validated[GformError, ConcreteDate] =
    (validateYear(concreteDate.year), validateMonth(concreteDate.month), validateDay(concreteDate.day))
      .mapN(ConcreteDate.apply)
}

object DateValidatorNec extends DateValidatorNec
