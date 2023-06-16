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
import cats.Monoid
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.ids._
import uk.gov.hmrc.gform.validation.ComponentChecker._
import uk.gov.hmrc.gform.validation.ValidationUtil._

class ComponentCheckerSwitchOpSpec extends AnyFlatSpec with Matchers {

  val atom = "atom"
  val gformError1: GformError = Map(
    ModelComponentId
      .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom(atom)) -> Set(
      "error"
    )
  )
  val gformError2: GformError = Map(
    ModelComponentId
      .Atomic(IndexedComponentId.Pure(BaseComponentId("comp2")), Atom(atom)) -> Set(
      "error"
    )
  )
  val gformError3: GformError = Map(
    ModelComponentId
      .Atomic(IndexedComponentId.Pure(BaseComponentId("comp3")), Atom(atom)) -> Set(
      "error"
    )
  )
  val failedOp1 = gformErrorOp(gformError1)
  val failedOp2 = gformErrorOp(gformError2)
  val failedOp3 = gformErrorOp(gformError3)
  "switchOp" should "return the first matching thenProgram if condition is true" in {
    val falseCase = switchCase(
      cond = false,
      thenProgram = failedOp1
    )
    val trueCase = switchCase(
      cond = true,
      thenProgram = failedOp2
    )

    val result = switchOp(falseCase, trueCase) {
      failedOp3
    }

    withClue("the  ErrorReportInterpreter should return all errors") {
      val validationResult = result.foldMap(ErrorReportInterpreter)
      val expectedResult = Monoid[GformError].combineAll(List(gformError1, gformError2, gformError3)).asRight
      validationResult shouldBe expectedResult
    }
    withClue("the  FulErrorReportInterpreter should return all errors") {
      val validationResult = result.foldMap(FullErrorReportInterpreter)
      val expectedResult = Monoid[GformError].combineAll(List(gformError1, gformError2, gformError3)).asRight
      validationResult shouldBe expectedResult
    }
    withClue("the  ShortCircuitProgram should return first matching error") {
      val validationResult = result.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError2.asLeft
    }
    withClue("the  NonShortCircuitProgram should return first matching error") {
      val validationResult = result.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError2.asRight
    }
  }

  it should "return elseBlock if no conditions are true" in {
    val falseCase = switchCase(
      cond = false,
      thenProgram = failedOp1
    )
    val trueCase = switchCase(
      cond = false,
      thenProgram = failedOp2
    )

    val result = switchOp(falseCase, trueCase) {
      failedOp3
    }

    withClue("the  ErrorReportInterpreter should return all errors") {
      val validationResult = result.foldMap(ErrorReportInterpreter)
      val expectedResult = Monoid[GformError].combineAll(List(gformError1, gformError2, gformError3)).asRight
      validationResult shouldBe expectedResult
    }
    withClue("the  FulErrorReportInterpreter should return all errors") {
      val validationResult = result.foldMap(FullErrorReportInterpreter)
      val expectedResult = Monoid[GformError].combineAll(List(gformError1, gformError2, gformError3)).asRight
      validationResult shouldBe expectedResult
    }
    withClue("the  ShortCircuitProgram should return elseBlock error") {
      val validationResult = result.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError3.asLeft
    }
    withClue("the  NonShortCircuitProgram should return elseBlock error") {
      val validationResult = result.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError3.asRight
    }
  }
}
