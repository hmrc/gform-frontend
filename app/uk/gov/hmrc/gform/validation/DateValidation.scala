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
import java.time.LocalDate

import cats.Monoid
import cats.data.Validated._
import cats.data._
import cats.implicits._
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.gform.validation.ComponentsValidator.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil._
import uk.gov.hmrc.gform.validation.ComponentsValidator._
import uk.gov.hmrc.gform.views.html.localisation
import uk.gov.hmrc.gform.validation.DateValidationLogic._

import scala.util.{ Failure, Success, Try }

object DateValidation {

  def validateDate(
    fieldValue: FormComponent,
    date: Date,
    otherFieldValue: Option[FormComponent],
    data: FormDataRecalculated): ValidatedType[Unit] =
    Monoid[ValidatedType[Unit]].combineAll(
      List(validateDateRequiredField(fieldValue, data), validateDateImpl(fieldValue, date, otherFieldValue)(data)))

  private val dataGetter: (FormComponent, FormDataRecalculated) => String => Seq[String] = (fv, data) =>
    suffix => data.data.get(fv.id.withSuffix(suffix)).toList.flatten
  private def validateDateRequiredField(fieldValue: FormComponent, data: FormDataRecalculated): ValidatedType[Unit] = {
    val dateValueOf = dataGetter(fieldValue, data)

    val validatedResult = fieldValue.mandatory match {
      case true =>
        List(
          ComponentsValidator.validateRF(fieldValue, "day")(dateValueOf("day")),
          ComponentsValidator.validateRF(fieldValue, "month")(dateValueOf("month")),
          ComponentsValidator.validateRF(fieldValue, "year")(dateValueOf("year"))
        )
      case false => List(().valid)
    }
    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def validateDateImpl(fieldValue: FormComponent, date: Date, otherFieldValue: Option[FormComponent])(
    data: FormDataRecalculated): ValidatedType[Unit] =
    date.constraintType match {

      case AnyDate =>
        validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data, otherFieldValue).andThen(lDate =>
          validationSuccess)

      case DateConstraints(dateConstraintList) =>
        val result = dateConstraintList.map {

          case DateConstraint(beforeOrAfterOrPrecisely, dateConstrInfo, offsetDate) =>
            (beforeOrAfterOrPrecisely, dateConstrInfo, offsetDate) match {

              case (beforeAfterPrecisely: BeforeAfterPrecisely, concreteDate: ConcreteDate, offset) =>
                validateConcreteDateWithMessages(fieldValue, beforeAfterPrecisely, concreteDate, offset, data)

              case (beforeAfterPrecisely: BeforeAfterPrecisely, Today, offset) =>
                validateTodayWithMessages(fieldValue, beforeAfterPrecisely, offset, data)

              case (beforeAfterPrecisely @ _, dateField: DateField, offset) =>
                validateDateFieldWithMessages(
                  fieldValue,
                  beforeAfterPrecisely,
                  dateField,
                  offset,
                  data,
                  otherFieldValue)
            }
        }
        Monoid[ValidatedType[Unit]].combineAll(result)
    }
  private def validateConcreteDateWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    concreteDate: ConcreteDate,
    offset: OffsetDate,
    data: FormDataRecalculated): Validated[GformError, Unit] =
    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data).andThen(
      inputDate =>
        validateConcreteDate(
          fieldValue,
          inputDate,
          concreteDate,
          offset,
          Map(fieldValue.id -> errors(fieldValue, incorrectDateMessage(beforeAfterPrecisely, concreteDate, offset))))(
          concreteDateFunctionMatch(beforeAfterPrecisely)))

  private def validateTodayWithSpecificMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate,
    data: FormDataRecalculated): Validated[GformError, Unit] =
    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
      .andThen(
        inputDate =>
          validateToday(
            fieldValue,
            inputDate,
            offset,
            Map(fieldValue.id -> errors(fieldValue, s"must be ${beforeAfterPrecisely.mkString} today")))(
            todayFunctionMatch(beforeAfterPrecisely)))

  private def validateDateFieldWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    dateField: DateField,
    offset: OffsetDate,
    data: FormDataRecalculated,
    otherFieldValue: Option[FormComponent]): Validated[GformError, Unit] = {

    val validateOtherDate = validateInputDate(fieldValue, dateField.value, None, data, otherFieldValue)

    val validatedThisDate = validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)

    validateOtherDate.andThen { otherLocalDate =>
      validatedThisDate.andThen { thisLocalDate =>
        validateConcreteDate(
          fieldValue,
          thisLocalDate,
          localDateToConcreteDate(otherLocalDate),
          offset,
          Map(
            fieldValue.id ->
              errors(
                fieldValue,
                incorrectDateMessage(beforeAfterPrecisely, localDateToConcreteDate(otherLocalDate), offset)))
        )(concreteDateFunctionMatch(beforeAfterPrecisely))
      }
    }
  }

  private def validateTodayWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate,
    data: FormDataRecalculated): Validated[GformError, Unit] =
    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
      .andThen(
        inputDate =>
          validateToday(fieldValue, inputDate, offset, Map(fieldValue.id -> errors(fieldValue, "must be today")))(
            todayFunctionMatch(beforeAfterPrecisely)))

  private def validateToday(fieldValue: FormComponent, localDate: LocalDate, offset: OffsetDate, dateError: GformError)(
    func: (LocalDate, OffsetDate) => Boolean): ValidatedType[Unit] =
    func(localDate, offset) match {
      case true  => validationSuccess
      case false => dateError.invalid
    }

  private def validateConcreteDate(
    fieldValue: FormComponent,
    localDate: LocalDate,
    concreteDate: ConcreteDate,
    offset: OffsetDate = OffsetDate(0),
    dateError: GformError)(func: (LocalDate, ConcreteDate, OffsetDate) => Boolean): ValidatedType[Unit] =
    func(localDate, concreteDate, offset) match {
      case true  => validationSuccess
      case false => dateError.invalid
    }

  private def concreteDateFunctionMatch(beforeAfterPrecisely: BeforeAfterPrecisely)(
    date: LocalDate,
    concreteDate: ConcreteDate,
    offset: OffsetDate): Boolean =
    beforeAfterPrecisely match {
      case Before if concreteDate.isExact => isBeforeExactConcreteDate(date, concreteDate, offset)

      case After if concreteDate.isExact => isAfterExactConcreteDate(date, concreteDate, offset)

      case Precisely => preciselyFunctionMatch(date, concreteDate, offset)

    }

  import cats.instances.int._
  import cats.syntax.eq._

  private def preciselyFunctionMatch(date: LocalDate, concreteDate: ConcreteDate, offset: OffsetDate): Boolean = {
    val parametersLength = concreteDate.getNumericParameters.length
    if (concreteDate.isExact) {
      isPreciselyExactConcreteDate(date, concreteDate, offset)
    } else {
      concreteDate match {
        case concreteDate: ConcreteDate if concreteDate.day === FirstDay || concreteDate.day === LastDay =>
          isFirstOrLastDay(date, concreteDate)
        case concreteDate: ConcreteDate if concreteDate.year === Next || concreteDate.year === Previous =>
          isNextOrPreviousYear(date, concreteDate)
        case concreteDate: ConcreteDate if parametersLength === 1 || parametersLength === 2 =>
          isSameAbstractDate(date, concreteDate)
        case _ => true
      }
    }
  }

  private def todayFunctionMatch(
    beforeAfterPrecisely: BeforeAfterPrecisely)(date: LocalDate, offset: OffsetDate): Boolean =
    beforeAfterPrecisely match {
      case Before    => isBeforeToday(date, offset)
      case After     => isAfterToday(date, offset)
      case Precisely => isPreciselyToday(date, offset)
    }

  private def isAfterToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isAfter(now.apply().plusDays(offset.value.toLong))

  private def isAfterExactConcreteDate(date: LocalDate, concreteDay: ConcreteDate, offset: OffsetDate): Boolean =
    date.isAfter(exactConcreteDateToLocalDate(concreteDay).plusDays(offset.value.toLong))

  private def isBeforeToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isBefore(now.apply().plusDays(offset.value.toLong))

  private def isBeforeExactConcreteDate(date: LocalDate, concreteDay: ConcreteDate, offset: OffsetDate): Boolean =
    date.isBefore(exactConcreteDateToLocalDate(concreteDay).plusDays(offset.value.toLong))

  private def isPreciselyToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isEqual(now.apply().plusDays(offset.value.toLong))

  private def isPreciselyExactConcreteDate(date: LocalDate, concreteDay: ConcreteDate, offset: OffsetDate): Boolean =
    date.isEqual(exactConcreteDateToLocalDate(concreteDay).plusDays(offset.value.toLong))

  private def isSameAbstractDate(date: LocalDate, concreteDay: ConcreteDate): Boolean =
    concreteDay.getNumericParameters
      .map {
        case ExactYear(year)   => date.getYear === year
        case ExactMonth(month) => date.getMonthValue === month
        case ExactDay(day)     => date.getDayOfMonth === day
      }
      .forall(identity)

  private def isFirstOrLastDay(date: LocalDate, concreteDay: ConcreteDate): Boolean = concreteDay.day match {
    case FirstDay => date.getDayOfMonth === 1
    case LastDay =>
      LocalDate.of(date.getYear, date.getMonthValue, date.getDayOfMonth).lengthOfMonth() === date.getDayOfMonth
  }

  private def isNextOrPreviousYear(date: LocalDate, concreteDay: ConcreteDate): Boolean = {
    val areMonthAndDayEqual = isSameAbstractDate(date: LocalDate, concreteDay: ConcreteDate)
    concreteDay.year match {
      case Next     => date.getYear === getNextYear && areMonthAndDayEqual
      case Previous => date.getYear === getPreviousYear && areMonthAndDayEqual
    }
  }

  private def tryConcreteDateAsLocalDate(concreteDate: ConcreteDate): Try[LocalDate] =
    concreteDate match {
      case ConcreteDate(year: ExactParameter, ExactMonth(month), day: ExactParameter) =>
        Try(LocalDate.of(getYear(year), month, getDay(getYear(year), month, day)))
    }

  private def validateInputDate(
    formComponent: FormComponent,
    formComponentId: FormComponentId,
    errorMsg: Option[String],
    data: FormDataRecalculated,
    otherFormComponent: Option[FormComponent] = None): ValidatedLocalDate = {
    val fieldIdList = Date.fields(formComponentId).map(fId => data.data.get(fId)).toList

    fieldIdList match {
      case Some(day +: Nil) :: Some(month +: Nil) :: Some(year +: Nil) :: Nil =>
        validateLocalDate(formComponent, formComponentId, otherFormComponent, errorMsg, day, month, year) match {
          case Valid(ConcreteDate(ExactYear(concYear), ExactMonth(concMonth), ExactDay(concDay))) =>
            Try(LocalDate.of(concYear, concMonth, concDay)) match {
              case Success(date) => Valid(date)
              case Failure(ex)   => Map(formComponentId -> errors(formComponent, "must be a valid date")).invalid
            }
          case Invalid(nonEmptyList) =>
            Invalid(nonEmptyList)
        }

      case _ =>
        validationFailure(formComponent, "is missing")
    }
  }

  private def validateLocalDate(
    formComponent: FormComponent,
    formComponentId: FormComponentId,
    otherFormComponent: Option[FormComponent],
    errorMessage: Option[String],
    day: String,
    month: String,
    year: String): ValidatedConcreteDate = {

    val dayLabel = messagePrefix(formComponent, formComponentId, otherFormComponent) + " " + localisation("day")
    val monthLabel = messagePrefix(formComponent, formComponentId, otherFormComponent) + " " + localisation("month")
    val yearLabel = messagePrefix(formComponent, formComponentId, otherFormComponent) + " " + localisation("year")

    val d = isNumeric(day, dayLabel)
      .andThen(y => isWithinBounds(y, 31, dayLabel))
      .leftMap(er => Map(formComponentId.withSuffix("day") -> Set(errorMessage.getOrElse(er))))
    val m = isNumeric(month, monthLabel)
      .andThen(y => isWithinBounds(y, 12, monthLabel))
      .leftMap(er => Map(formComponentId.withSuffix("month") -> Set(errorMessage.getOrElse(er))))
    val y = isNumeric(year, yearLabel)
      .andThen(y => hasValidNumberOfDigits(y, 4, yearLabel))
      .leftMap(er => Map(formComponentId.withSuffix("year") -> Set(errorMessage.getOrElse(er))))

    parallelWithApplicative(d, m, y)(ConcreteDate.apply)
  }

}
