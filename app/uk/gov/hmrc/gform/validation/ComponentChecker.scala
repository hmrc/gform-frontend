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

import cats.Monoid
import cats.free.Free
import cats.implicits._
import cats.~>
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.EitherType
import uk.gov.hmrc.gform.validation.ValidationUtil.GformError
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

/*
 * ComponentChecker Framework
 *
 * A framework for creating custom checkers on different components of an application.
 *
 * In order to create a new checker, one needs to extend the `ComponentChecker` trait, overriding the `checkProgram` method.
 *
 * Core Concepts:
 *
 * - CheckProgram[A]: Represents a check program that can either be successful (Right) or fail with one or many errors (Left).
 *
 * - CheckInterpreter: Transforms CheckOp to an Either type.
 *
 * - CheckOp[A]: Contains all operations a check program can do.
 *
 * - ShortCircuitOp, NonShortCircuitOp: Represents a group of check programs being combined using
 *                                       short-circuiting or non-short-circuiting logic, respectively.
 *
 * - IfOp: Represents an if-then-else construct in the DSL. It's created using the ifProgram function.
 *         - cond is ignored by the  report. Should provide a check on user input.
 *         - andCond is checked by all interpreters. Should provide a condition on something
 *           that is not related to user input, like form's mandatory property, etc.
 *
 * - AndThenOp, OrElseOp: Combinators allowing the composition of multiple check programs.
 *
 * - LeftMapOp: Transforms the error of a check program.
 *
 * - ErrorOp, SuccessOp: Represents a failing or successful check program, respectively.
 *
 * - ValueForReport[A]: A type class that should be implemented for certain operations when user input is
 *                      unavailable and a default value needs to be provided for the report.
 *
 * Process for creating a Checker:
 *
 * 1. Extend ComponentChecker: Create a class that extends ComponentChecker.
 *    Example: `class OverseasAddressChecker[D <: DataOrigin]() extends ComponentChecker[D] {...}`
 *
 * 2. Override checkProgram: This method will hold the core logic of the checker.
 *    Example: `override protected def checkProgram(context: CheckerDependency[D])...`
 *
 * 3. Create Check Programs: Create a set of `CheckProgram` operations based on the business rules and the specific checks you want to perform.
 *
 * 4. Combine Check Programs: Combine them to create the overall check for the component.
 *    You can choose to use combinators: "andThen", "orElse", "leftMap", "voidProgram", "toProgram", or "foldProgram" based on your requirements.
 *
 * 5. Run the Checker: Instantiate the checker and call the `runCheck` method, providing the necessary context.
 *    It runs all checks and return a result that can be validated to reflect either a success or appropriate errors.
 */

trait ComponentChecker[A, D <: DataOrigin] {
  import ComponentChecker._

  def runCheck(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator,
    interpreter: CheckInterpreter
  ): ValidatedType[Unit] =
    (checkProgram(context).foldMap(interpreter)) match {
      case Right(Right(u)) => ().valid
      case Right(Left(e))  => e.invalid
      case Left(e)         => e.invalid
    }

  protected def checkProgram(componentCheckerContext: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[A]

}

object ComponentChecker {
  import GformError._
  type CheckProgram[A] = Free[CheckOp, EitherType[A]]
  type CheckInterpreter = CheckOp ~> EitherType

  // Define the DSL
  sealed trait CheckOp[A]

  case class ShortCircuitOp[A](program: CheckProgram[A]) extends CheckOp[EitherType[A]]

  case class NonShortCircuitOp[A](program: CheckProgram[A]) extends CheckOp[EitherType[A]]

  case class IfOp[A](
    cond: Boolean,
    andCond: Option[Boolean],
    thenProgram: CheckProgram[A],
    elseProgram: CheckProgram[A],
    isSwitch: Boolean = false
  ) extends CheckOp[EitherType[A]]

  case class AndThenOp[A, B](
    program: CheckProgram[A],
    andThenFun: A => CheckProgram[B],
    reportA: A
  ) extends CheckOp[EitherType[B]]

  case class OrElseOp[A, B](
    program: CheckProgram[A],
    orElseProgram: CheckProgram[B]
  ) extends CheckOp[EitherType[B]]

  case class LeftMapOp[A](
    program: CheckProgram[A],
    f: GformError => GformError
  ) extends CheckOp[EitherType[A]]

  case class ErrorOp[A](gformError: GformError) extends CheckOp[EitherType[A]]
  case class SuccessOp[A](a: A) extends CheckOp[EitherType[A]]

  def successProgram[A](a: A): CheckProgram[A] = Free.liftF[CheckOp, EitherType[A]](SuccessOp[A](a))
  def errorProgram[A](gformError: GformError): CheckProgram[A] =
    Free.liftF[CheckOp, EitherType[A]](ErrorOp[A](gformError))

  def ifProgram[A](
    cond: Boolean,
    thenProgram: CheckProgram[A],
    elseProgram: CheckProgram[A]
  ): CheckProgram[A] =
    Free.liftF[CheckOp, EitherType[A]](IfOp[A](cond, None, thenProgram, elseProgram))

  def ifProgram[A](
    cond: Boolean = true,
    andCond: Boolean,
    thenProgram: CheckProgram[A],
    elseProgram: CheckProgram[A]
  ): CheckProgram[A] =
    Free.liftF[CheckOp, EitherType[A]](IfOp[A](cond, Some(andCond), thenProgram, elseProgram))

  case class SwitchCase[A](
    cond: Boolean,
    andCond: Option[Boolean],
    thenProgram: () => CheckProgram[A]
  )

  def switchCase[A](
    cond: Boolean,
    thenProgram: => CheckProgram[A]
  ) = SwitchCase[A](cond, None, () => thenProgram)

  def switchCase[A](
    cond: Boolean = true,
    andCond: Boolean,
    thenProgram: => CheckProgram[A]
  ) = SwitchCase[A](cond, Some(andCond), () => thenProgram)

  def switchProgram[A](switchCases: SwitchCase[A]*)(elseProgram: => CheckProgram[A]): CheckProgram[A] =
    if (switchCases.isEmpty) elseProgram
    else
      Free.liftF[CheckOp, EitherType[A]](
        IfOp(
          cond = switchCases.head.cond,
          andCond = switchCases.head.andCond,
          thenProgram = switchCases.head.thenProgram(),
          elseProgram = switchProgram(switchCases.tail: _*)(elseProgram),
          isSwitch = true
        )
      )

  /*
   This interpreter stops at the first error.
   It short-circuits on the first error by returning Left(error).
   */
  object ShortCircuitInterpreter extends CheckInterpreter {
    def apply[A](op: CheckOp[A]) = op match {
      case ShortCircuitOp(program)    => program.foldMap(this).leftErrors
      case NonShortCircuitOp(program) => program.foldMap(NonShortCircuitInterpreter).leftErrors
      case IfOp(cond, andCond, thenProgram, elseProgram, _) =>
        if (cond && andCond.getOrElse(true)) thenProgram.foldMap(this).leftErrors
        else elseProgram.foldMap(this).leftErrors
      case ErrorOp(gformError) => gformError.asLeft[A]
      case SuccessOp(a: Any)   => a.asRight.asRight
      case AndThenOp(program, andThenFun, _) =>
        program.foldMap(this) match {
          case Left(error)        => error.asLeft
          case Right(Right(a))    => andThenFun(a).foldMap(this).leftErrors
          case Right(Left(error)) => error.asLeft
        }
      case OrElseOp(program, orElseProgram) =>
        program.foldMap(this) match {
          case Left(_)                              => orElseProgram.foldMap(this).leftErrors
          case Right(Right(a))                      => a.asRight.asRight
          case Right(Left(error)) if error.nonEmpty => orElseProgram.foldMap(this).leftErrors
          case Right(Left(error))                   => error.asLeft.asRight
        }
      case LeftMapOp(program, f) =>
        program
          .foldMap(this)
          .fold(
            error => Left(f(error)),
            eitherError => Right(eitherError.leftMap(f))
          )
          .leftErrors
    }
  }

  /*
   This interpreter always transforms the operation to
   Right(gformError), ensuring that it won't short-circuit in the for comprehension.
   */
  object NonShortCircuitInterpreter extends CheckInterpreter {
    def apply[A](op: CheckOp[A]) = op match {
      // Converts Left(gformError) to Right(gformError) to make sure
      // there's no short circuit in the for comprehension.
      case ShortCircuitOp(program) =>
        program.foldMap(ShortCircuitInterpreter).rightErrors
      case NonShortCircuitOp(program) => program.foldMap(this).rightErrors
      case IfOp(cond, andCond, thenProgram, elseProgram, _) =>
        if (cond && andCond.getOrElse(true)) thenProgram.foldMap(this).rightErrors
        else elseProgram.foldMap(this).rightErrors
      case ErrorOp(gformError) => Right(gformError.asLeft[A])
      case SuccessOp(a: Any)   => a.asRight.asRight
      case AndThenOp(program, andThenFun, _) =>
        program.foldMap(this) match {
          case Left(error)        => error.asLeft.asRight
          case Right(Right(a))    => andThenFun(a).foldMap(this).rightErrors
          case Right(Left(error)) => error.asLeft.asRight
        }
      case OrElseOp(program, orElseProgram) =>
        program.foldMap(this) match {
          case Left(error)        => orElseProgram.foldMap(this).rightErrors
          case Right(Right(a))    => a.asRight.asRight
          case Right(Left(error)) => orElseProgram.foldMap(this).rightErrors
        }
      case LeftMapOp(program, f) =>
        program
          .foldMap(this)
          .fold(
            error => Left(f(error)),
            eitherError => Right(eitherError.leftMap(f))
          )
          .rightErrors
    }
  }

  object ErrorReportInterpreter extends CheckInterpreter {
    def apply[A](op: CheckOp[A]) = op match {
      case ShortCircuitOp(program)    => program.foldMap(this).rightErrors
      case NonShortCircuitOp(program) => program.foldMap(this).rightErrors
      case IfOp(_, andCond, thenProgram, elseProgram, true) => // isSwitch
        (
          andCond match {
            case None        => (getError(thenProgram.foldMap(this)) |+| getError(elseProgram.foldMap(this))).asLeft[A]
            case Some(true)  => (getError(thenProgram.foldMap(this)) |+| getError(elseProgram.foldMap(this))).asLeft[A]
            case Some(false) => elseProgram.foldMap(this)
          }
        ).rightErrors
      case IfOp(_, andCond, thenProgram, elseProgram, _) =>
        (
          andCond match {
            case None        => (getError(thenProgram.foldMap(this)) |+| getError(elseProgram.foldMap(this))).asLeft[A]
            case Some(true)  => thenProgram.foldMap(this)
            case Some(false) => elseProgram.foldMap(this)
          }
        ).rightErrors
      case ErrorOp(gformError) => gformError.asLeft.asRight
      case SuccessOp(a)        => a.asRight.asRight
      case AndThenOp(program, andThenFun, reportA) =>
        program.foldMap(this) match {
          case Left(error)        => (error |+| getError(andThenFun(reportA).foldMap(this))).asLeft.asRight
          case Right(Right(a))    => andThenFun(a).foldMap(this).rightErrors
          case Right(Left(error)) => (error |+| getError(andThenFun(reportA).foldMap(this))).asLeft.asRight
        }
      case OrElseOp(program, orElseProgram) =>
        program.foldMap(this) match {
          case Left(_)         => orElseProgram.foldMap(this).rightErrors
          case Right(Right(a)) => a.asRight.asRight
          case Right(Left(_))  => orElseProgram.foldMap(this).rightErrors
        }
      case LeftMapOp(program, f) =>
        program
          .foldMap(this)
          .fold(
            error => Left(f(error)),
            eitherError => Right(eitherError.leftMap(f))
          )
          .rightErrors
    }
  }

  trait ValueForReport[A] {
    def valueForReport(): A
  }

  implicit val unitValueForReport = new ValueForReport[Unit] {
    def valueForReport(): Unit = ()
  }

  private def getError[A](ee: EitherType[EitherType[A]]): GformError =
    ee.fold(
      identity,
      {
        case Right(_) => emptyGformError
        case Left(e)  => e
      }
    )

  private def combinePrograms[A](programs: List[CheckProgram[A]])(implicit ev: Monoid[A]): CheckProgram[A] =
    programs.foldLeft(Free.pure[CheckOp, EitherType[A]](ev.empty.asRight)) { (acc, program) =>
      for {
        accValue  <- acc
        progValue <- program
      } yield (accValue, progValue) match {
        case (Right(a), Right(b)) => Right(b) // take the last one
        case (Left(a), Left(b))   => Left(a |+| b)
        case (Right(_), Left(b))  => Left(b)
        case (Left(a), Right(_))  => Left(a)
      }
    }

  implicit class CheckProgramListOps[A](val list: List[CheckProgram[A]]) extends AnyVal {
    def shortCircuitProgram(implicit ev: Monoid[A]): CheckProgram[A] =
      Free.liftF[CheckOp, EitherType[A]](ShortCircuitOp[A](combinePrograms(list)))

    def nonShortCircuitProgram(implicit ev: Monoid[A]): CheckProgram[A] =
      Free.liftF[CheckOp, EitherType[A]](NonShortCircuitOp(combinePrograms(list)))

  }

  implicit class CheckProgramOps[A](val checkProgram: CheckProgram[A]) extends AnyVal {
    def leftMap(f: GformError => GformError): CheckProgram[A] =
      Free.liftF[CheckOp, EitherType[A]](LeftMapOp(checkProgram, f))

    def andThen[B](andThenFun: A => CheckProgram[B])(implicit ev: ValueForReport[A]): CheckProgram[B] =
      Free.liftF[CheckOp, EitherType[B]](AndThenOp[A, B](checkProgram, andThenFun, ev.valueForReport()))

    def orElse[B](orElseProgram: CheckProgram[B]): CheckProgram[B] =
      Free.liftF[CheckOp, EitherType[B]](OrElseOp[A, B](checkProgram, orElseProgram))

    def voidProgram: CheckProgram[Unit] = checkProgram.map(x => x.map(_ => ()))
  }

  implicit class OptionOps[A](val op: Option[A]) extends AnyVal {
    def toProgram(errorProgram: CheckProgram[A])(implicit ev: ValueForReport[A]): CheckProgram[A] =
      ifProgram[A](
        cond = op.isDefined,
        thenProgram = successProgram[A](op.getOrElse(ev.valueForReport())),
        elseProgram = errorProgram
      )
    def foldProgram[B](onNone: CheckProgram[B], onSomeFun: A => CheckProgram[B])(implicit
      ev: ValueForReport[A]
    ): CheckProgram[B] =
      ifProgram[B](
        cond = op.isDefined,
        thenProgram = onSomeFun(op.getOrElse(ev.valueForReport())),
        elseProgram = onNone
      )
  }

  implicit class EitherTypeOps[A](val e: EitherType[EitherType[A]]) extends AnyVal {
    def leftErrors: EitherType[EitherType[A]] = e match {
      case Left(e)                                => Left(e)
      case Right(Left(errors)) if errors.nonEmpty => Left(errors)
      case otherwise                              => otherwise
    }
    def rightErrors: EitherType[EitherType[A]] = e match {
      case Left(a)   => a.asLeft.asRight
      case otherwise => otherwise
    }
  }
}

trait CheckerDependency[D <: DataOrigin] {
  def formModelVisibilityOptics: FormModelVisibilityOptics[D]
  def formComponent: FormComponent
  def cache: CacheData
  def envelope: EnvelopeWithMapping
  def lookupRegistry: LookupRegistry
  def getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher

  def atomicFcId(atom: Atom) = formComponent.atomicFormComponentId(atom)
  def valueOf(atomicFcId: ModelComponentId.Atomic): Seq[String] =
    formModelVisibilityOptics.data
      .get(atomicFcId)
      .toSeq
      .flatMap(_.toSeq)

  def nonBlankValueOf(atomicFcId: ModelComponentId.Atomic): Option[String] =
    valueOf(atomicFcId).filterNot(_.trim.isEmpty()).headOption

  def isAtomicValueBlank(atomicFcId: ModelComponentId.Atomic): Boolean =
    nonBlankValueOf(atomicFcId).isEmpty

  def isAtomicValueExceedMaxLength(atomicFcId: ModelComponentId.Atomic, maxLength: Int): Boolean =
    nonBlankValueOf(atomicFcId).exists(_.length > maxLength)

}

object GformError {

  def gformError(
    modelComponentId: ModelComponentId,
    fieldValue: FormComponent,
    messageKey: String,
    vars: Option[List[String]]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): GformError =
    Map(modelComponentId -> {
      val varsList: List[String] = vars.getOrElse(Nil)
      Set(
        fieldValue.errorMessage
          .map(ls => ls.value())
          .getOrElse(messages(messageKey, varsList: _*))
      )
    })

  val emptyGformError: GformError = Map.empty[ModelComponentId, Set[String]]

  def foldEitherType[A](e: EitherType[A]): GformError =
    e.fold(error => error, either => emptyGformError)
}
