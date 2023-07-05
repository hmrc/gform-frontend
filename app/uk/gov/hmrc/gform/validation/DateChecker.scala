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

import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors

import java.time.LocalDate
import scala.annotation.nowarn
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import GformError._
import ComponentChecker._
import DateValidationLogic._

case class SomeDate(year: Int, month: Int, day: Int)

class DateChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent = context.formComponent
    formComponent match {
      case IsDate(date) =>
        validateDate(context.formComponent, context.formModelVisibilityOptics, date)
      case _ => throw new IllegalArgumentException("FormComponent is not a Date")
    }
  }

  implicit val concreteDateValueForReport = new ValueForReport[ConcreteDate] {
    def valueForReport(): ConcreteDate = localDateToConcreteDate(LocalDate.now())
  }

  implicit val someDateValueForReport = new ValueForReport[SomeDate] {
    def valueForReport(): SomeDate = SomeDate(1, 1, 1970)
  }
  implicit val localDateValueForReport = new ValueForReport[LocalDate] {
    def valueForReport(): LocalDate = LocalDate.now()
  }

  implicit val stringValueForReport = new ValueForReport[(String, String, String)] {
    def valueForReport(): (String, String, String) = ("1", "1", "1970")
  }
  implicit val ListDateconstraintValueForReport = new ValueForReport[List[DateConstraint]] {
    def valueForReport(): List[DateConstraint] = List()
  }

  def validateDate(
    fieldValue: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    date: Date
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] =
    ifProgram(
      cond = fieldValue.mandatory,
      thenProgram = List(
        checkAnyFieldsEmpty(fieldValue, formModelVisibilityOptics),
        validateDateImpl(fieldValue, formModelVisibilityOptics, date)
      ).nonShortCircuitProgram,
      elseProgram = checkAllFieldsEmpty(fieldValue, formModelVisibilityOptics) orElse validateDateImpl(
        fieldValue,
        formModelVisibilityOptics,
        date
      )
    )

  private def validationFailed[T](
    formComponent: FormComponent,
    messageKey: String,
    vars: Option[List[String]]
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[T] =
    errorProgram[T](
      Map[ModelComponentId, Set[String]](
        formComponent.firstAtomModelComponentId -> errors(formComponent, messageKey, vars)
      )
    )

  private def requiredError(formComponent: FormComponent, modelComponentId: ModelComponentId)(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] = {
    val placeholder1 = formComponent.errorShortName
      .flatMap(_.nonBlankValue())
      .getOrElse(SmartString.blank.transform(_ => "a date", _ => "ddyddiad").value())
    val placeholder2 = formComponent.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
    errorProgram[Unit](
      Map[ModelComponentId, Set[String]](
        modelComponentId -> errors(
          formComponent,
          "generic.error.date.required",
          Some(List(placeholder1, placeholder2)),
          ""
        )
      )
    )
  }

  private def checkAnyFieldsEmpty(
    fieldValue: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] = {
    val validatedResult =
      fieldValue.multiValueId.atomsModelComponentIds.map { modelComponentId =>
        val answer = formModelVisibilityOptics.data.one(modelComponentId)
        answer.filterNot(_.isEmpty()).fold(requiredError(fieldValue, modelComponentId))(_ => successProgram(()))
      }
    validatedResult.nonShortCircuitProgram
  }

  private def checkAllFieldsEmpty(
    fieldValue: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] = {
    val answers = fieldValue.multiValueId.atomsModelComponentIds
      .map { modelComponentId =>
        formModelVisibilityOptics.data.one(modelComponentId).filter(_.trim.nonEmpty)
      }
    if (answers.exists(_.nonEmpty)) {
      requiredError(fieldValue, fieldValue.firstAtomModelComponentId)
    } else successProgram(())
  }

  private def validateDateImpl(
    fieldValue: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    date: Date
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] = {

    val mayBeDateConstraintList = date.constraintType match {
      case AnyDate                             => None
      case DateConstraints(dateConstraintList) => Some(dateConstraintList)
    }

    mayBeDateConstraintList
      .toProgram(
        errorProgram = validateInputDate(fieldValue, formModelVisibilityOptics).map(_.map(_ => List()))
      )
      .andThen(
        andThenFun = dateConstraintList => {
          val constraintsPrograms = dateConstraintList.map { dateConstraint =>
            val DateConstraint(beforeOrAfterOrPrecisely, dateConstrInfo, offset) = dateConstraint
            val isToday = dateConstrInfo match {
              case _: Today.type => true
              case _             => false
            }
            val maybeConcreteDate = dateConstrInfo match {
              case d: ConcreteDate => Some(d)
              case _               => None
            }
            val maybeDateField = dateConstrInfo match {
              case d: DateField => Some(d)
              case _            => None
            }
            switchProgram(
              switchCase(
                cond = isToday,
                thenProgram =
                  validateTodayWithMessages(fieldValue, formModelVisibilityOptics, beforeOrAfterOrPrecisely, offset)
              ),
              switchCase(
                cond = maybeConcreteDate.nonEmpty,
                thenProgram = maybeConcreteDate.foldProgram(
                  onNone = successProgram(()),
                  onSomeFun = d =>
                    validateConcreteDateWithMessages(
                      fieldValue,
                      formModelVisibilityOptics,
                      beforeOrAfterOrPrecisely,
                      d,
                      offset
                    )
                )
              ),
              switchCase(
                cond = maybeDateField.nonEmpty,
                thenProgram = validateDateFieldWithMessages(
                  fieldValue,
                  formModelVisibilityOptics,
                  beforeOrAfterOrPrecisely,
                  maybeDateField.getOrElse(DateField(fieldValue.id)),
                  offset
                )
              )
            )(elseProgram = successProgram(()))
          }
          constraintsPrograms.nonShortCircuitProgram
        }
      )
  }

  private def validateConcreteDateWithMessages(
    formComponent: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    beforeAfterPrecisely: BeforeAfterPrecisely,
    concreteDate: ConcreteDate,
    offset: OffsetDate
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] =
    validateInputDate(formComponent, formModelVisibilityOptics).andThen(
      andThenFun = inputDate =>
        ifProgram(
          cond = validateConcreteDate(concreteDate, beforeAfterPrecisely, inputDate, offset),
          thenProgram = successProgram(()),
          elseProgram = {
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
        )
    )

  private def validateTodayWithMessages(
    fieldValue: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] =
    validateInputDate(fieldValue, formModelVisibilityOptics).andThen(
      andThenFun = date => validateToday(fieldValue, beforeAfterPrecisely, offset)(date)
    )

  private def validateDateFieldWithMessages(
    fieldValue: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    beforeAfterPrecisely: BeforeAfterPrecisely,
    dateField: DateField,
    offset: OffsetDate
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[Unit] = {

    val dateFieldValue = fieldValue.modelComponentId.indexedComponentId
      .fold(_ => dateField.value)(indexed => dateField.value.withIndex(indexed.index))

    val otherFieldValue = formModelVisibilityOptics.fcLookup.getOrElse(
      dateFieldValue,
      throw new IllegalArgumentException(s"Cannot find ${dateField.value} in visibility model.")
    )

    val validateOtherDate = validateInputDate(otherFieldValue, formModelVisibilityOptics)
    val validatedThisDate = validateInputDate(fieldValue, formModelVisibilityOptics)

    validateOtherDate.andThen(andThenFun =
      otherLocalDate =>
        validatedThisDate.andThen(andThenFun =
          thisLocalDate =>
            ifProgram(
              cond = beforeAfterPrecisely.datePredicate(
                thisLocalDate,
                otherLocalDate.plusDays(offset.value.toLong)
              ),
              thenProgram = successProgram(()),
              elseProgram = {
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
            )
        )
    )
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
    sse: SmartStringEvaluator,
    messages: Messages,
    now: Now[LocalDate]
  ): CheckProgram[Unit] = {
    val nowWithOffset = now.apply().plusDays(offset.value.toLong)
    ifProgram(
      cond = beforeAfterPrecisely.datePredicate(date, nowWithOffset),
      thenProgram = successProgram(()),
      elseProgram = {
        val placeholder = formComponent.errorShortNameStart
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.transform(_ => "Date", _ => "dyddiad").value())
        val OffsetDate(offsetDate) = offset
        switchProgram(
          switchCase(
            cond = beforeAfterPrecisely === Before && offsetDate === 0,
            thenProgram = validationFailed(formComponent, "generic.error.date.today.before", Some(placeholder :: Nil))
          ),
          switchCase(
            cond = beforeAfterPrecisely === Before && offsetDate === 1,
            thenProgram =
              validationFailed(formComponent, "generic.error.date.today.offset.before", Some(placeholder :: Nil))
          ),
          switchCase(
            cond = beforeAfterPrecisely === After && offsetDate === 0,
            thenProgram = validationFailed(formComponent, "generic.error.date.today.after", Some(placeholder :: Nil))
          ),
          switchCase(
            cond = beforeAfterPrecisely === After && offsetDate === -1,
            thenProgram =
              validationFailed(formComponent, "generic.error.date.today.offset.after", Some(placeholder :: Nil))
          )
        )(elseProgram = {
          val messageKeyWithVars: MessageKeyWithVars =
            DateValidationLogic.incorrectDateMessage(
              beforeAfterPrecisely,
              DateValidationLogic.localDateToConcreteDate(date),
              offset
            )
          val vars = messageKeyWithVars.vars.map(placeholder :: _)
          validationFailed(formComponent, messageKeyWithVars.messageKey, vars)
        })

      }
    )

  }

  private def validateInputDate(
    formComponent: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[LocalDate] = {

    val fieldIdList = formComponent.multiValueId.atomsModelComponentIds.map(formModelVisibilityOptics.data.one).toList

    val (day, month, year) = fieldIdList match {
      case Some(d) :: Some(m) :: Some(y) :: Nil => (Some(d), Some(m), Some(y))
      case _                                    => (None, None, None)
    }

    val dateProgram = (day, month, year)
      .mapN((d, m, y) => (d, m, y))
      .toProgram(
        errorProgram = validationFailed[(String, String, String)](formComponent, "date.isMissing", None)
      )

    dateProgram
      .andThen { case (d, m, y) => validateLocalDate(formComponent, d, m, y) }
      .andThen { someDate =>
        val SomeDate(concYear, concMonth, concDay) = someDate
        val maybeDate: Option[LocalDate] = Try(LocalDate.of(concYear, concMonth, concDay)) match {
          case Success(date) => Some(date)
          case Failure(ex)   => None
        }
        maybeDate.toProgram(errorProgram = {
          val placeholder1 = formComponent.errorShortNameStart
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "Date", _ => "dyddiad").value())
          val placeholder2 =
            formComponent.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
          validationFailed[LocalDate](
            formComponent,
            "generic.error.date.real",
            Some(placeholder1 :: placeholder2 :: Nil)
          )
        })
      }
  }

  import DateValidationLogic._
  private def validateLocalDate(
    formComponent: FormComponent,
    day: String,
    month: String,
    year: String
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): CheckProgram[SomeDate] = {

    val errorMessage = formComponent.errorMessage.map(_.value())

    def errorGranularity(suffix: Atom): ModelComponentId =
      formComponent.atomicFormComponentId(suffix)

    val maybeDay: Option[Int] =
      if (notExceedMaxLength(day, 2))
        parseToInt(day).filter(d => isWithinBounds(d, 31))
      else None

    val dayProgram: CheckProgram[Int] =
      ifProgram[Int](
        cond = maybeDay.isDefined,
        thenProgram = successProgram(maybeDay.getOrElse(1)),
        elseProgram = errorProgram[Int](
          Map(
            errorGranularity(Date.day) -> errorMessage
              .map(Set(_))
              .getOrElse(errors(formComponent, "generic.error.day.required", None))
          )
        )
      )
    val maybeMonth: Option[Int] =
      if (notExceedMaxLength(month, 2))
        parseToInt(month).filter(m => isWithinBounds(m, 12))
      else None

    val monthProgram: CheckProgram[Int] =
      ifProgram[Int](
        cond = maybeMonth.isDefined,
        thenProgram = successProgram(maybeMonth.getOrElse(1)),
        elseProgram = errorProgram[Int](
          Map(
            errorGranularity(Date.month) -> errorMessage
              .map(Set(_))
              .getOrElse(errors(formComponent, "generic.error.month.required", None))
          )
        )
      )

    val maybeYear: Option[Int] =
      if (notExceedMaxLength(year, 4))
        parseToInt(year).filter(y => hasNumberOfDigits(y, 4))
      else None

    val yearProgram: CheckProgram[Int] =
      ifProgram[Int](
        cond = maybeYear.isDefined,
        thenProgram = successProgram(maybeYear.getOrElse(1970)),
        elseProgram = errorProgram[Int](
          Map(
            errorGranularity(Date.year) -> errorMessage
              .map(Set(_))
              .getOrElse(errors(formComponent, "generic.error.year.required", None))
          )
        )
      )

    for {
      day   <- dayProgram
      month <- monthProgram
      year  <- yearProgram
    } yield (day, month, year) match {
      case (Right(d), Right(m), Right(y)) => Right(SomeDate(y, m, d))
      case (d, m, y)                      => (foldEitherType(d) |+| foldEitherType(m) |+| foldEitherType(y)).asLeft[SomeDate]
    }

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
