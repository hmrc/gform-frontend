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
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.ids._
import uk.gov.hmrc.gform.validation.ComponentChecker._
import uk.gov.hmrc.gform.validation.ValidationUtil._
import scala.collection.mutable.LinkedHashSet

import GformError.linkedHashSetMonoid

class ComponentCheckerSwitchOpSpec extends AnyFunSpec with Matchers {

  val atom = "atom"
  val gformError1: GformError = Map(
    ModelComponentId
      .Atomic(IndexedComponentId.Pure(BaseComponentId("comp1")), Atom(atom)) -> LinkedHashSet(
      "error"
    )
  )
  val gformError2: GformError = Map(
    ModelComponentId
      .Atomic(IndexedComponentId.Pure(BaseComponentId("comp2")), Atom(atom)) -> LinkedHashSet(
      "error"
    )
  )
  val gformError3: GformError = Map(
    ModelComponentId
      .Atomic(IndexedComponentId.Pure(BaseComponentId("comp3")), Atom(atom)) -> LinkedHashSet(
      "error"
    )
  )
  val failedOp1 = errorProgram[Unit](gformError1)
  val failedOp2 = errorProgram[Unit](gformError2)
  val failedOp3 = errorProgram[Unit](gformError3)

  describe("for a switch operation where the 'cond' of one case is true and there is no 'andCond'") {
    val falseCase = switchCase(
      cond = false,
      thenProgram = failedOp1
    )
    val trueCase = switchCase(
      cond = true,
      thenProgram = failedOp2
    )

    val result = switchProgram(falseCase, trueCase)(
      elseProgram = failedOp3
    )

    it("the  ErrorReportInterpreter should return all errors") {
      val validationResult = result.foldMap(ErrorReportInterpreter)
      val expectedResult = (gformError1 |+| gformError2 |+| gformError3).asLeft.asRight
      validationResult shouldBe expectedResult
    }
    it("the  ShortCircuitProgram should return first matching error") {
      val validationResult = result.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError2.asLeft
    }
    it("the  NonShortCircuitProgram should return first matching error") {
      val validationResult = result.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError2.asLeft.asRight
    }
  }

  describe("for a switch operation where the 'cond' of all cases are false and there is no 'andCond'") {
    val falseCase = switchCase(
      cond = false,
      thenProgram = failedOp1
    )
    val trueCase = switchCase(
      cond = false,
      thenProgram = failedOp2
    )

    val result = switchProgram(falseCase, trueCase)(
      elseProgram = failedOp3
    )

    it("the  ErrorReportInterpreter should return all errors") {
      val validationResult = result.foldMap(ErrorReportInterpreter)
      val expectedResult = (gformError1 |+| gformError2 |+| gformError3).asLeft.asRight
      validationResult shouldBe expectedResult
    }
    it("the  ShortCircuitProgram should return elseBlock error") {
      val validationResult = result.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError3.asLeft
    }
    it("the  NonShortCircuitProgram should return elseBlock error") {
      val validationResult = result.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError3.asLeft.asRight
    }
  }

  describe("for a switch operation where the 'cond' of one case is true and there is one 'andCond' set to false") {
    val falseCase = switchCase(
      cond = false,
      andCond = false,
      thenProgram = failedOp1
    )
    val trueCase = switchCase(
      cond = false,
      thenProgram = failedOp2
    )

    val result = switchProgram(falseCase, trueCase)(
      elseProgram = failedOp3
    )

    it("the  ErrorReportInterpreter should return all errors without first one that is excluded by andCond") {
      val validationResult = result.foldMap(ErrorReportInterpreter)
      val expectedResult = (gformError2 |+| gformError3).asLeft.asRight
      validationResult shouldBe expectedResult
    }
    it("the  ShortCircuitProgram should return elseBlock error") {
      val validationResult = result.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError3.asLeft
    }
    it("the  NonShortCircuitProgram should return elseBlock error") {
      val validationResult = result.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError3.asLeft.asRight
    }
  }
}
