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

package uk.gov.hmrc.gform.validation
import java.time.LocalDate
import cats.Monoid
import cats.data.Validated._
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil._
import uk.gov.hmrc.gform.validation.DateValidationLogic.MessageKeyWithVars
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.{ errors, fieldDescriptor }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess
import uk.gov.hmrc.gform.sharedmodel.SmartString

import scala.annotation.nowarn
import scala.util.{ Failure, Success, Try }

case class SomeDate(year: Int, month: Int, day: Int)

class DateValidation[D <: DataOrigin](formModelVisibilityOptics: FormModelVisibilityOptics[D])(implicit
  messages: Messages,
  sse: SmartStringEvaluator
) {

  def validateDate(
    fieldValue: FormComponent,
    date: Date
  ): ValidatedType[Unit] =
    if (fieldValue.mandatory) {
      Monoid[ValidatedType[Unit]].combineAll(
        List(
          checkAnyFieldsEmpty(fieldValue),
          validateDateImpl(fieldValue, date)
        )
      )
    } else {
      checkAllFieldsEmpty(fieldValue) orElse validateDateImpl(fieldValue, date)
    }

  private def validationFailed[T](
    formComponent: FormComponent,
    messageKey: String,
    vars: Option[List[String]]
  ): ValidatedType[T] =
    Map[ModelComponentId, Set[String]](
      formComponent.firstAtomModelComponentId -> errors(formComponent, messageKey, vars)
    ).invalid

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId): ValidatedType[Unit] = {
    val placeholder1 = formComponent.errorShortName
      .flatMap(_.nonBlankValue())
      .getOrElse(SmartString.blank.transform(_ => "a date", _ => "ddyddiad").value())
    val placeholder2 = formComponent.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
    Map[ModelComponentId, Set[String]](
      modelComponentId -> errors(
        formComponent,
        "generic.error.date.required",
        Some(List(placeholder1, placeholder2)),
        ""
      )
    ).invalid
  }

  private def checkAnyFieldsEmpty(
    fieldValue: FormComponent
  ): ValidatedType[Unit] = {
    val validatedResult =
      fieldValue.multiValueId.atomsModelComponentIds.map { modelComponentId =>
        val answer = formModelVisibilityOptics.data.one(modelComponentId)
        answer.filterNot(_.isEmpty()).fold(requiredError(fieldValue, modelComponentId))(_ => validationSuccess)
      }
    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def checkAllFieldsEmpty(
    fieldValue: FormComponent
  ): ValidatedType[Unit] = {
    val answers = fieldValue.multiValueId.atomsModelComponentIds
      .map { modelComponentId =>
        formModelVisibilityOptics.data.one(modelComponentId).filter(_.trim.nonEmpty)
      }
    if (answers.exists(_.nonEmpty)) {
      requiredError(fieldValue, fieldValue.firstAtomModelComponentId)
    } else validationSuccess
  }

  private def validateDateImpl(
    fieldValue: FormComponent,
    date: Date
  ): ValidatedType[Unit] =
    date.constraintType match {

      case AnyDate =>
        validateInputDate(
          fieldValue
        ).andThen(lDate => validationSuccess)

      case DateConstraints(dateConstraintList) =>
        val result = dateConstraintList.map { case DateConstraint(beforeOrAfterOrPrecisely, dateConstrInfo, offset) =>
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
          val messageKeyWithVars: MessageKeyWithVars =
            DateValidationLogic.incorrectDateMessage(beforeAfterPrecisely, concreteDate, offset)
          val messageKey =
            concreteDate.day.fold(_ => messageKeyWithVars.messageKey)(_ => messageKeyWithVars.messageKey)(_ =>
              "generic.error.date.first"
            )(_ => "generic.error.date.last")
          val placeholder = formComponent.errorShortNameStart
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "Date", _ => "dyddiad").value())
          val vars = messageKeyWithVars.vars.map(placeholder :: _)
          validationFailed(formComponent, messageKey, vars)
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

    val dateFieldValue = fieldValue.modelComponentId.indexedComponentId
      .fold(_ => dateField.value)(indexed => dateField.value.withIndex(indexed.index))

    val otherFieldValue = formModelVisibilityOptics.fcLookup.getOrElse(
      dateFieldValue,
      throw new IllegalArgumentException(s"Cannot find ${dateField.value} in visibility model.")
    )

    val validateOtherDate = validateInputDate(otherFieldValue)
    val validatedThisDate = validateInputDate(fieldValue)

    validateOtherDate.andThen { otherLocalDate =>
      validatedThisDate.andThen { thisLocalDate =>
        if (beforeAfterPrecisely.datePredicate(thisLocalDate, otherLocalDate.plusDays(offset.value.toLong)))
          validationSuccess
        else {
          val messageKeyWithVars: MessageKeyWithVars =
            DateValidationLogic.incorrectDateMessage(
              beforeAfterPrecisely,
              DateValidationLogic.localDateToConcreteDate(otherLocalDate),
              offset
            )
          val placeholder = fieldValue.errorShortNameStart
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "Date", _ => "dyddiad").value())
          val vars = messageKeyWithVars.vars.map(placeholder :: _)
          validationFailed(fieldValue, messageKeyWithVars.messageKey, vars)
        }
      }
    }
  }
  @nowarn
  private def validateConcreteDate(
    concreteDate: ConcreteDate,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    date: LocalDate,
    offset: OffsetDate
  ): Boolean = {

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
    def a7(year: Int, month: Int, day: Int): Boolean = op2(date, LocalDate.of(year, month, day).plusDays(offset.value.toLong))
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
  )(implicit
    now: Now[LocalDate]
  ): ValidatedType[Unit] = {
    val nowWithOffset = now.apply().plusDays(offset.value.toLong)
    if (beforeAfterPrecisely.datePredicate(date, nowWithOffset)) validationSuccess
    else {
      val placeholder = formComponent.errorShortNameStart
        .flatMap(_.nonBlankValue())
        .getOrElse(SmartString.blank.transform(_ => "Date", _ => "dyddiad").value())
      (beforeAfterPrecisely, offset) match {
        case (Before, OffsetDate(0)) =>
          validationFailed(formComponent, "generic.error.date.today.before", Some(placeholder :: Nil))
        case (Before, OffsetDate(1)) =>
          validationFailed(formComponent, "generic.error.date.today.offset.before", Some(placeholder :: Nil))
        case (After, OffsetDate(0)) =>
          validationFailed(formComponent, "generic.error.date.today.after", Some(placeholder :: Nil))
        case (After, OffsetDate(-1)) =>
          validationFailed(formComponent, "generic.error.date.today.offset.after", Some(placeholder :: Nil))
        case _ =>
          val messageKeyWithVars: MessageKeyWithVars =
            DateValidationLogic.incorrectDateMessage(
              beforeAfterPrecisely,
              DateValidationLogic.localDateToConcreteDate(LocalDate.now()),
              offset
            )
          val vars = messageKeyWithVars.vars.map(placeholder :: _)
          validationFailed(formComponent, messageKeyWithVars.messageKey, vars)
      }
    }

  }

  private def validateInputDate(
    formComponent: FormComponent
  ): ValidatedType[LocalDate] = {

    val fieldIdList = formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one).toList

    fieldIdList match {
      case Some(day) :: Some(month) :: Some(year) :: Nil =>
        validateLocalDate(formComponent, day, month, year).andThen { case SomeDate(concYear, concMonth, concDay) =>
          Try(LocalDate.of(concYear, concMonth, concDay)) match {
            case Success(date) => Valid(date)
            case Failure(ex) =>
              val placeholder1 = formComponent.errorShortNameStart
                .flatMap(_.nonBlankValue())
                .getOrElse(SmartString.blank.transform(_ => "Date", _ => "dyddiad").value())
              val placeholder2 = formComponent.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
              validationFailed(formComponent, "generic.error.date.real", Some(placeholder1 :: placeholder2 :: Nil))
          }
        }

      case _ =>
        validationFailure(formComponent.firstAtomModelComponentId, formComponent, "date.isMissing", None, "")
    }
  }

  private def validateLocalDate(
    formComponent: FormComponent,
    day: String,
    month: String,
    year: String
  ): ValidatedType[SomeDate] = {

    val errorMessage = formComponent.errorMessage.map(_.value())

    def errorGranularity(suffix: Atom): ModelComponentId =
      formComponent.atomicFormComponentId(suffix)

    val label = fieldDescriptor(formComponent, "")
    val dayLabel = label + " " + messages("date.day")
    val monthLabel = label + " " + messages("date.month")
    val yearLabel = label + " " + messages("date.year")
    val d = DateValidationLogic
      .isNotEmpty(day, dayLabel)
      .andThen(_ => DateValidationLogic.hasMaximumLength(day, 2, dayLabel))
      .andThen(_ => DateValidationLogic.isNumeric(day, dayLabel, label))
      .andThen(d => DateValidationLogic.isWithinBounds(d, 31, dayLabel))
      .leftMap(_ =>
        Map(
          errorGranularity(Date.day) -> errorMessage
            .map(Set(_))
            .getOrElse(errors(formComponent, "generic.error.day.required", None))
        )
      )
    val m = DateValidationLogic
      .isNotEmpty(month, monthLabel)
      .andThen(_ => DateValidationLogic.hasMaximumLength(month, 2, monthLabel))
      .andThen(_ => DateValidationLogic.isNumeric(month, monthLabel, label))
      .andThen(m => DateValidationLogic.isWithinBounds(m, 12, monthLabel))
      .leftMap(_ =>
        Map(
          errorGranularity(Date.month) -> errorMessage
            .map(Set(_))
            .getOrElse(errors(formComponent, "generic.error.month.required", None))
        )
      )
    val y = DateValidationLogic
      .isNotEmpty(year, yearLabel)
      .andThen(_ => DateValidationLogic.hasMaximumLength(year, 4, yearLabel))
      .andThen(_ => DateValidationLogic.isNumeric(year, yearLabel, label))
      .andThen(y => DateValidationLogic.hasValidNumberOfDigits(y, 4, yearLabel))
      .leftMap(_ =>
        Map(
          errorGranularity(Date.year) -> errorMessage
            .map(Set(_))
            .getOrElse(errors(formComponent, "generic.error.year.required", None))
        )
      )

    DateValidationLogic.parallelWithApplicative(d, m, y)(SomeDate.apply)
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
