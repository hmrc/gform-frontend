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

import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.free.Free
import org.mockito.scalatest.IdiomaticMockito
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.i18n._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.ids._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.validation.ValidationUtil.GformError
import ComponentChecker._
import IndexedComponentId._

class ComponentCheckerSpec extends AnyFlatSpecLike with Matchers with IdiomaticMockito {

  val checkerDependencyMock: CheckerDependency[DataOrigin.Mongo] = mock[CheckerDependency[DataOrigin.Mongo]]
  implicit val messages: Messages = mock[Messages]
  implicit val l: LangADT = mock[LangADT]
  implicit val smartStringEvaluator: SmartStringEvaluator = mock[SmartStringEvaluator]
  implicit val checkInterpreter: ComponentChecker.CheckInterpreter = ComponentChecker.NonShortCircuitInterpreter
  def successfulOperation(atom: String, error: String) = ComponentChecker.ifThenOp(
    cond = false,
    thenProgram = gformErrorOp(
      Map(
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom(atom)) -> Set(
          error
        )
      )
    )
  )
  def failedOperation(atom: String, error: String) = ComponentChecker.ifThenOp(
    cond = true,
    thenProgram = gformErrorOp(
      Map(
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom(atom)) -> Set(
          error
        )
      )
    )
  )

  val checkerWithFailure = new ComponentChecker[DataOrigin.Mongo] {
    override protected def checkProgram(componentCheckerContext: CheckerDependency[DataOrigin.Mongo])(implicit
      langADT: LangADT,
      messages: Messages,
      sse: SmartStringEvaluator
    ): ComponentChecker.CheckProgram[GformError] =
      List(
        failedOperation("atom1", "Error for atom1"),
        successfulOperation("atom2", "Error for atom2")
      ).nonShortCircuitProgram
  }

  val checkerWithAllSuccesses = new ComponentChecker[DataOrigin.Mongo] {
    override protected def checkProgram(componentCheckerContext: CheckerDependency[DataOrigin.Mongo])(implicit
      langADT: LangADT,
      messages: Messages,
      sse: SmartStringEvaluator
    ): ComponentChecker.CheckProgram[GformError] =
      List(
        successfulOperation("atom1", "Error for atom1"),
        successfulOperation("atom2", "Error for atom2")
      ).nonShortCircuitProgram
  }
  val checkerWithNoOperations = new ComponentChecker[DataOrigin.Mongo] {
    override protected def checkProgram(componentCheckerContext: CheckerDependency[DataOrigin.Mongo])(implicit
      langADT: LangADT,
      messages: Messages,
      sse: SmartStringEvaluator
    ): ComponentChecker.CheckProgram[GformError] =
      Free.pure(Map.empty[ModelComponentId, Set[String]])
  }

  "runCheck" should "return valid for checker with no operations" in {
    val expected = Valid(())
    val result = checkerWithNoOperations.runCheck(checkerDependencyMock)
    result should be(expected)
  }

  it should "stop at the first failed operation" in {
    val expected = Invalid(
      Map(
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom1")) -> Set(
          "Error for atom1"
        )
      )
    )
    val result = checkerWithFailure.runCheck(checkerDependencyMock)
    result should be(expected)
  }

  it should "return valid for all successful checks" in {
    val expected = Valid(())
    val result = checkerWithAllSuccesses.runCheck(checkerDependencyMock)
    result should be(expected)
  }

  it should "return the first error among multiple failures" in {
    val checkerWithMultipleFailures = new ComponentChecker[DataOrigin.Mongo] {
      override protected def checkProgram(componentCheckerContext: CheckerDependency[DataOrigin.Mongo])(implicit
        langADT: LangADT,
        messages: Messages,
        sse: SmartStringEvaluator
      ): ComponentChecker.CheckProgram[GformError] =
        List(
          failedOperation("atom1", "Error for atom1"),
          failedOperation("atom2", "Error for atom2")
        ).nonShortCircuitProgram
    }

    val expected = Invalid(
      Map(
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom1")) -> Set(
          "Error for atom1"
        ),
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom2")) -> Set(
          "Error for atom2"
        )
      )
    )
    val result = checkerWithMultipleFailures.runCheck(checkerDependencyMock)
    result should be(expected)
  }

  it should "work properly for nested operations" in {

    val checkerWithMultipleFailures = new ComponentChecker[DataOrigin.Mongo] {
      override protected def checkProgram(componentCheckerContext: CheckerDependency[DataOrigin.Mongo])(implicit
        langADT: LangADT,
        messages: Messages,
        sse: SmartStringEvaluator
      ): ComponentChecker.CheckProgram[GformError] =
        List(
          List(
            failedOperation("atom11", "Error for atom11"),
            failedOperation("atom12", "Error for atom12")
          ).shortCircuitProgram,
          failedOperation("atom2", "Error for atom2")
        ).nonShortCircuitProgram

    }

    val expectedRunCheck = Invalid(
      Map(
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom11")) -> Set(
          "Error for atom11"
        ),
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom2")) -> Set(
          "Error for atom2"
        )
      )
    )
    val result = checkerWithMultipleFailures.runCheck(checkerDependencyMock)
    result should be(expectedRunCheck)

  }

  "runCheckReport" should "return return combination of all errors in the check" in {

    implicit val checkInterpreter: ComponentChecker.CheckInterpreter = ComponentChecker.ErrorReportInterpreter
    val expected = Invalid(
      Map(
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom1")) -> Set("Error for atom1"),
        ModelComponentId
          .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom("atom2")) -> Set("Error for atom2")
      )
    )

    val result = checkerWithFailure.runCheck(checkerDependencyMock)
    result should be(expected)

  }

  it should "work properly for nested operations" in {

    implicit val checkInterpreter: ComponentChecker.CheckInterpreter = ComponentChecker.ErrorReportInterpreter
    val checkerWithMultipleFailures = new ComponentChecker[DataOrigin.Mongo] {
      override protected def checkProgram(componentCheckerContext: CheckerDependency[DataOrigin.Mongo])(implicit
        langADT: LangADT,
        messages: Messages,
        sse: SmartStringEvaluator
      ): ComponentChecker.CheckProgram[GformError] =
        List(
          List(
            failedOperation("atom11", "Error for atom11"),
            failedOperation("atom12", "Error for atom12")
          ).shortCircuitProgram,
          failedOperation("atom2", "Error for atom2")
        ).nonShortCircuitProgram

    }

    val expectedRunCheckReport = Invalid(
      Map(
        ModelComponentId.Atomic(Pure(BaseComponentId("comp1")), Atom("atom11")) -> Set("Error for atom11"),
        ModelComponentId.Atomic(Pure(BaseComponentId("comp1")), Atom("atom12")) -> Set("Error for atom12"),
        ModelComponentId.Atomic(Pure(BaseComponentId("comp1")), Atom("atom2"))  -> Set("Error for atom2")
      )
    )
    val resultReport = checkerWithMultipleFailures.runCheck(checkerDependencyMock)
    resultReport should be(expectedRunCheckReport)
  }
}
