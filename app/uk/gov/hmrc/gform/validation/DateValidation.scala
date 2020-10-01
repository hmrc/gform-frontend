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
import java.time.LocalDate

import cats.Monoid
import cats.data.Validated._
import cats.data._
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil._
import uk.gov.hmrc.gform.validation.DateValidationLogic._
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.{ errors, fieldDescriptor }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

import scala.util.{ Failure, Success, Try }

case class SomeDate(year: Int, month: Int, day: Int)

class DateValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(
  implicit messages: Messages,
  l: LangADT,
  sse: SmartStringEvaluator
) {

  def validateDate(
    fieldValue: FormComponent,
    date: Date
  ): ValidatedType[Unit] =
    Monoid[ValidatedType[Unit]].combineAll(
      List(
        validateDateRequiredField(fieldValue),
        validateDateImpl(fieldValue, date)
      )
    )

  private def validationFailed[T](
    formComponent: FormComponent,
    messageKey: String,
    vars: Option[List[String]]
  ): ValidatedType[T] =
    Map[ModelComponentId, Set[String]](
      formComponent.firstAtomModelComponentId -> errors(formComponent, messageKey, vars)).invalid

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](modelComponentId -> errors(formComponent, "field.error.required", None, "")).invalid

  private def validateDateRequiredField(
    fieldValue: FormComponent
  ): ValidatedType[Unit] = {

    val validatedResult =
      if (fieldValue.mandatory) {
        fieldValue.multiValueId.atomsModelComponentIds.map { modelComponentId =>
          val answer = formModelVisibilityOptics.data.one(modelComponentId)
          answer.filterNot(_.isEmpty()).fold(requiredError(fieldValue, modelComponentId))(_ => validationSuccess)
        }
      } else List(validationSuccess)

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def validateDateImpl(
    fieldValue: FormComponent,
    date: Date
  )(
    implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    date.constraintType match {

      case AnyDate =>
        validateInputDate(
          fieldValue
        ).andThen(lDate => validationSuccess)

      case DateConstraints(dateConstraintList) =>
        val result = dateConstraintList.map {

          case DateConstraint(beforeOrAfterOrPrecisely, dateConstrInfo, offset) =>
            dateConstrInfo match {
              case _: Today.type =>
                validateTodayWithMessages(fieldValue, beforeOrAfterOrPrecisely, offset)
              case concreteDate: ConcreteDate =>
                validateConcreteDateWithMessages(
                  fieldValue,
                  beforeOrAfterOrPrecisely,
                  concreteDate,
                  offset
                )
              case dateField: DateField =>
                validateDateFieldWithMessages(
                  fieldValue,
                  beforeOrAfterOrPrecisely,
                  dateField,
                  offset
                )
            }
        }
        Monoid[ValidatedType[Unit]].combineAll(result)
    }

  private def validateConcreteDateWithMessages(
    formComponent: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    concreteDate: ConcreteDate,
    offset: OffsetDate
  ): ValidatedType[Unit] =
    validateInputDate(formComponent)
      .andThen { inputDate =>
        if (validateConcreteDate(concreteDate, beforeAfterPrecisely, inputDate, offset)) validationSuccess
        else {
          val messageKeyWithVars: MessageKeyWithVars = incorrectDateMessage(beforeAfterPrecisely, concreteDate, offset)
          validationFailed(formComponent, messageKeyWithVars.messageKey, messageKeyWithVars.vars)
        }
      }

  private def validateTodayWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate
  ): ValidatedType[Unit] =
    validateInputDate(fieldValue)
      .andThen(validateToday(fieldValue, beforeAfterPrecisely, offset))

  private def validateDateFieldWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    dateField: DateField,
    offset: OffsetDate
  ): ValidatedType[Unit] = {

    val otherFieldValue = formModelVisibilityOptics.fcLookup.getOrElse(
      dateField.value,
      throw new IllegalArgumentException(s"Cannot find ${dateField.value} in visibility model."))

    val validateOtherDate = validateInputDate(otherFieldValue)
    val validatedThisDate = validateInputDate(fieldValue)

    validateOtherDate.andThen { otherLocalDate =>
      validatedThisDate.andThen { thisLocalDate =>
        if (beforeAfterPrecisely.datePredicate(thisLocalDate, otherLocalDate.plusDays(offset.value))) validationSuccess
        else {
          val messageKeyWithVars: MessageKeyWithVars =
            incorrectDateMessage(beforeAfterPrecisely, localDateToConcreteDate(otherLocalDate), offset)
          validationFailed(fieldValue, messageKeyWithVars.messageKey, messageKeyWithVars.vars)
        }
      }
    }
  }

  private def validateConcreteDate(
    concreteDate: ConcreteDate,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    date: LocalDate,
    offset: OffsetDate
  ): Boolean = {
    val maybeLocalDate = exactConcreteDateToLocalDate(concreteDate)

    def op1: (Int, Int) => Boolean = beforeAfterPrecisely.intPredicate
    def op2: (LocalDate, LocalDate) => Boolean = beforeAfterPrecisely.datePredicate

    def preciselyFalse(boolean: Boolean) = if (beforeAfterPrecisely === Precisely) false else boolean

    // format: off
    def a1(day: Int): Boolean                        = op1(date.getDayOfMonth, day)
    def a2(month: Int): Boolean                      = op1(date.getMonthValue, month)
    def a3(month: Int, day: Int): Boolean            = preciselyFalse(op1(date.getMonthValue, month)) || (date.getMonthValue === month && op1(date.getDayOfMonth, day))
    def a4(year: Int): Boolean                       = op1(date.getYear, year)
    def a5(year: Int, day: Int): Boolean             = preciselyFalse(op1(date.getYear, year)) || (date.getYear === year && op1(date.getDayOfMonth, day))
    def a6(year: Int, month: Int): Boolean           = preciselyFalse(op1(date.getYear, year)) || (date.getYear === year && op1(date.getMonthValue, month))
    def a7(year: Int, month: Int, day: Int): Boolean = op2(date, LocalDate.of(year, month, day).plusDays(offset.value))
    // format: on

    val nextYear: Int = DateValidationLogic.getNextYear
    val previousYear: Int = DateValidationLogic.getPreviousYear

    val y = new CachedHasDay(date)
    val x = new CachedHasYear(previousYear, nextYear)

    concreteDate match {
      // format: off
      case ConcreteDate(Year.Any,         Month.Any,          Day.Any)       => true
      case ConcreteDate(Year.Any,         Month.Any,          y.HasDay(day)) => a1(day + offset.value)
      case ConcreteDate(Year.Any,         Month.Exact(month), Day.Any)       => a2(month)
      case ConcreteDate(Year.Any,         Month.Exact(month), y.HasDay(day)) => a3(month, day + offset.value)
      case ConcreteDate(x.HasYear(year),  Month.Any,          Day.Any)       => a4(year)
      case ConcreteDate(x.HasYear(year),  Month.Any,          y.HasDay(day)) => a5(year, day + offset.value)
      case ConcreteDate(x.HasYear(year),  Month.Exact(month), Day.Any)       => a6(year, month)
      case ConcreteDate(x.HasYear(year),  Month.Exact(month), y.HasDay(day)) => a7(year, month, day)
      // format: on
    }
  }

  private def validateToday(
    formComponent: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate
  )(
    date: LocalDate
  )(
    implicit
    now: Now[LocalDate]
  ): ValidatedType[Unit] = {
    val nowWithOffset = now.apply().plusDays(offset.value)
    if (beforeAfterPrecisely.datePredicate(date, nowWithOffset)) validationSuccess
    else validationFailed(formComponent, s"date.${beforeAfterPrecisely.mkString}", Some("today" :: Nil))
  }

  private def validateInputDate(
    formComponent: FormComponent
  ): ValidatedType[LocalDate] = {

    val fieldIdList = formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one).toList

    fieldIdList match {
      case Some(day) :: Some(month) :: Some(year) :: Nil =>
        validateLocalDate(formComponent, day, month, year).andThen {
          case SomeDate(concYear, concMonth, concDay) =>
            Try(LocalDate.of(concYear, concMonth, concDay)) match {
              case Success(date) => Valid(date)
              case Failure(ex)   => validationFailed(formComponent, "date.invalid", None)
            }
        }

      case _ =>
        validationFailure(formComponent.firstAtomModelComponentId, formComponent, "date.isMissing", None)
    }
  }

  private def validateLocalDate(
    formComponent: FormComponent,
    day: String,
    month: String,
    year: String
  ): ValidatedType[SomeDate] = {

    val errorMessage = formComponent.errorMessage.map(_.value)

    def errorGranularity(suffix: Atom): ModelComponentId =
      formComponent.atomicFormComponentId(suffix)

    val label = fieldDescriptor(formComponent, "")
    val dayLabel = label + " " + messages("date.day")
    val monthLabel = label + " " + messages("date.month")
    val yearLabel = label + " " + messages("date.year")
    val d = hasMaximumLength(day, 2, dayLabel)
      .andThen(_ => isNumeric(day, dayLabel, label))
      .andThen(y => isWithinBounds(y, 31, dayLabel))
      .leftMap(er => Map(errorGranularity(Date.day) -> Set(errorMessage.getOrElse(er))))
    val m = hasMaximumLength(month, 2, monthLabel)
      .andThen(_ => isNumeric(month, monthLabel, label))
      .andThen(y => isWithinBounds(y, 12, monthLabel))
      .leftMap(er => Map(errorGranularity(Date.month) -> Set(errorMessage.getOrElse(er))))
    val y = hasMaximumLength(year, 4, yearLabel)
      .andThen(_ => isNumeric(year, yearLabel, label))
      .andThen(y => hasValidNumberOfDigits(y, 4, yearLabel))
      .leftMap(er => Map(errorGranularity(Date.year) -> Set(errorMessage.getOrElse(er))))

    parallelWithApplicative(d, m, y)(SomeDate.apply)
  }
}

class CachedHasDay(date: LocalDate) {
  object HasDay {
    def unapply(day: Day): Option[Int] = day match {
      case Day.Any        => None
      case Day.Last       => Some(date.lengthOfMonth)
      case Day.First      => Some(1)
      case Day.Exact(day) => Some(day)
    }
  }
}

class CachedHasYear(previous: Int, next: Int) {
  object HasYear {
    def unapply(year: Year): Option[Int] = year match {
      case Year.Any         => None
      case Year.Previous    => Some(previous)
      case Year.Next        => Some(next)
      case Year.Exact(year) => Some(year)
    }
  }
}
