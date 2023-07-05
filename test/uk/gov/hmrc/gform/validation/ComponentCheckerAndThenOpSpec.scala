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

class ComponentCheckerAndThenOpSpec extends AnyFunSpec with Matchers {

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
  val failedOp1 = errorProgram[Unit](gformError1)
  val failedOp2 = errorProgram[Unit](gformError2)
  val failedOp3 = errorProgram[Unit](gformError3)

  implicit val intValueForReport = new ValueForReport[Int] {
    def valueForReport(): Int = 576
  }
  describe("andThen on successful operation followed by successful operation") {

    val testProgram: CheckProgram[Int] = successProgram(1).andThen { i =>
      successProgram(i + 1)
    }

    it("the  ErrorReportInterpreter should return value") {
      val validationResult = testProgram.foldMap(ErrorReportInterpreter)
      val expectedResult = 2.asRight.asRight
      validationResult shouldBe expectedResult
    }
    it("the  ShortCircuitProgram should value of the andThen operation") {
      val validationResult = testProgram.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe 2.asRight.asRight
    }
    it("the  NonShortCircuitProgram should return value of the andThen operation") {
      val validationResult = testProgram.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe 2.asRight.asRight
    }
  }

  describe("andThen on successful operation followed by failed operation") {

    val testProgram: CheckProgram[Int] = successProgram(1).andThen { i =>
      errorProgram[Int](gformError1)
    }

    it("the  ErrorReportInterpreter should return failed operation error") {
      val validationResult = testProgram.foldMap(ErrorReportInterpreter)
      val expectedResult = gformError1.asLeft.asRight
      validationResult shouldBe expectedResult
    }
    it("the  ShortCircuitProgram should return failed operation error") {
      val validationResult = testProgram.foldMap(ShortCircuitInterpreter)
      val expectedResult = gformError1.asLeft
      validationResult shouldBe expectedResult
    }
    it("the  NonShortCircuitProgram should return failed operation error") {
      val validationResult = testProgram.foldMap(NonShortCircuitInterpreter)
      val expectedResult = gformError1.asLeft.asRight
      validationResult shouldBe expectedResult
    }
  }

  describe("andThen on failed operation followed by failed operation") {

    val testProgram: CheckProgram[Int] = errorProgram[Int](gformError1).andThen { i =>
      errorProgram[Int](gformError2)
    }

    it("the  ErrorReportInterpreter should return all failed operations error") {
      val validationResult = testProgram.foldMap(ErrorReportInterpreter)
      val expectedResult = (gformError1 |+| gformError2).asLeft.asRight
      validationResult shouldBe expectedResult
    }
    it("the  ShortCircuitProgram should return first failed operation error") {
      val validationResult = testProgram.foldMap(ShortCircuitInterpreter)
      val expectedResult = gformError1.asLeft
      validationResult shouldBe expectedResult
    }
    it("the  NonShortCircuitProgram should return first failed operation error") {
      val validationResult = testProgram.foldMap(NonShortCircuitInterpreter)
      val expectedResult = gformError1.asLeft.asRight
      validationResult shouldBe expectedResult
    }
  }

}
