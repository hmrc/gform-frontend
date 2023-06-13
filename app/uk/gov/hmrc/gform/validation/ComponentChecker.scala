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
 *  Creating a New Checker in ComponentChecker Framework
 *
 *  This guide will help you understand how to create a new checker in the ComponentChecker framework.
 *  As an example, we will use the OverseasAddressChecker.
 *
 *  Prerequisites
 *
 *  You should have a good understanding of CheckProgram, CheckInterpreter, and CheckingOp, as they form the base
 *  of the Domain Specific Language (DSL) for checkers.
 *
 *  Steps to Create a New Checker
 *
 *  1. Extend ComponentChecker:
 *     Create a new class that extends ComponentChecker. This class will encapsulate
 *     all the logic for checking a specific component. For example, if we want to create a checker for an OverseasAddress component,
 *     we could create a new class named OverseasAddressChecker.
 *
 *     class OverseasAddressChecker[D <: DataOrigin]() extends ComponentChecker[D] {...}
 *
 *  2. Override checkProgram:
 *     Inside this new class, you should override the checkProgram method. This method
 *     will hold the core logic of the checker.
 *
 *     override protected def checkProgram(context: CheckerDependency[D])(implicit
 *         langADT: LangADT,
 *         messages: Messages,
 *         sse: SmartStringEvaluator
 *     ): CheckProgram[GformError] = {...}
 *
 *  3. Create Check Programs:
 *     Inside checkProgram, create a set of CheckProgram operations based on the business
 *     rules and the specific checks you want to perform. For a simple boolean check, use the ifThenOp function to create a
 *     check program. For a branching boolean check, use the ifThenElseOp function to create a check program. To create an
 *     error, use the gformErrorOp function.
 *
 *  4. Combine Check Programs:
 *     After creating the check programs, combine them to create the overall check for the
 *     component. You can choose to combine them using shortCircuitProgram or nonShortCircuitProgram based on whether you
 *     want to short-circuit (stop at the first error) or not.
 *
 *  5. Run the Checker:
 *     To run the checker, simply create an instance of the checker and call the runCheck method, providing the necessary context.
 *     This will run all the checks and return a result that can be validated. If there are any issues, it will return
 *     the appropriate errors that were defined in your check programs.
 */

trait ComponentChecker[D <: DataOrigin] {
  import ComponentChecker._

  def runCheck(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator,
    interpreter: CheckInterpreter
  ): ValidatedType[Unit] = {
    (checkProgram(context).foldMap(interpreter)) match {
      case Right(e) if e.isEmpty => e.valid
      case Right(e)              => e.invalid
      case Left(e)               => e.invalid
    }
  }.void

  protected def checkProgram(componentCheckerContext: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): ComponentChecker.CheckProgram[GformError]
}

object ComponentChecker {
  import GformError._
  type CheckProgram[A] = Free[CheckingOp, A]
  type CheckInterpreter = CheckingOp ~> EitherType

  // Define the DSL
  sealed trait CheckingOp[A]

  case class ShortCircuitProgram(program: CheckProgram[GformError]) extends CheckingOp[GformError]

  case class NonShortCircuitProgram(program: CheckProgram[GformError]) extends CheckingOp[GformError]

  case class IfThenElseOp(
    cond: Boolean,
    thenProgram: CheckProgram[GformError],
    elseProgram: CheckProgram[GformError]
  ) extends CheckingOp[GformError]

  case class IfThenOp(
    cond: Boolean,
    thenProgram: CheckProgram[GformError]
  ) extends CheckingOp[GformError]

  case class GformErrorOp(gformError: GformError) extends CheckingOp[GformError]

  def gformErrorOp(gfromError: GformError) = Free.liftF(GformErrorOp(gfromError))

  def ifThenElseOp(
    cond: Boolean,
    thenProgram: CheckProgram[GformError],
    elseProgram: CheckProgram[GformError]
  ): CheckProgram[GformError] =
    Free.liftF(IfThenElseOp(cond, thenProgram, elseProgram))

  def ifThenOp(
    cond: Boolean,
    thenProgram: CheckProgram[GformError]
  ): CheckProgram[GformError] =
    Free.liftF(IfThenOp(cond, thenProgram))

  /*
   This interpreter stops at the first error.
   It short-circuits on the first error by returning Left(error).
   */
  object ShortCircuitInterpreter extends CheckInterpreter {
    def apply[A](op: CheckingOp[A]) = op match {
      case ShortCircuitProgram(program)    => program.foldMap(this)
      case NonShortCircuitProgram(program) => program.foldMap(NonShortCircuitInterpreter)
      case IfThenElseOp(cond, thenProgram, elseProgram) =>
        if (cond) thenProgram.foldMap(this) else elseProgram.foldMap(this)
      case IfThenOp(cond, thenProgram) =>
        if (cond) thenProgram.foldMap(this) else Right(emptyGformError)
      case GformErrorOp(gformError) => Left(gformError)
    }
  }

  /*
   This interpreter always transforms the operation to
   Right(gformError), ensuring that it won't short-circuit in the for comprehension.

   Right(gformError) will be considered invalid by runCheck if gformError is not an empty error.
   */
  object NonShortCircuitInterpreter extends CheckInterpreter {
    def apply[A](op: CheckingOp[A]) = op match {
      // Converts Left(gformError) to Right(gformError) to make sure
      // there's no short circuit in the for comprehension.
      case ShortCircuitProgram(program)    => program.foldMap(ShortCircuitInterpreter).fold(Right(_), Right(_))
      case NonShortCircuitProgram(program) => program.foldMap(this)
      case IfThenElseOp(cond, thenProgram, elseProgram) =>
        if (cond) thenProgram.foldMap(this) else elseProgram.foldMap(this)
      case IfThenOp(cond, thenProgram) =>
        if (cond) thenProgram.foldMap(this) else Right(emptyGformError)
      case GformErrorOp(gformError) => Right(gformError)
    }
  }

  object ErrorReportInterpreter extends CheckInterpreter {
    def apply[A](op: CheckingOp[A]) = op match {
      case ShortCircuitProgram(program)    => program.foldMap(this)
      case NonShortCircuitProgram(program) => program.foldMap(this)
      case IfThenElseOp(_, thenProgram, elseProgram) =>
        Monoid[EitherType[GformError]].combine(thenProgram.foldMap(this), elseProgram.foldMap(this))
      case IfThenOp(_, thenProgram) => thenProgram.foldMap(this)
      case GformErrorOp(gformError) => Right(gformError)
    }
  }

  private def createProgram(operations: List[CheckProgram[GformError]]): CheckProgram[GformError] =
    operations.foldLeft(Free.pure[CheckingOp, GformError](emptyGformError)) { (acc, op) =>
      for {
        a <- acc
        b <- op
      } yield Monoid[GformError].combine(a, b)
    }

  implicit class CheckProgramListOps(val list: List[CheckProgram[GformError]]) extends AnyVal {
    def shortCircuitProgram: CheckProgram[GformError] =
      Free.liftF(ShortCircuitProgram(createProgram(list)))

    def nonShortCircuitProgram: CheckProgram[GformError] =
      Free.liftF(NonShortCircuitProgram(createProgram(list)))
  }

}

trait CheckerDependency[D <: DataOrigin] {
  def formModelVisibilityOptics: FormModelVisibilityOptics[D]
  def formComponent: FormComponent
  def cache: CacheData
  def envelope: EnvelopeWithMapping
  def lookupRegistry: LookupRegistry

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
}
