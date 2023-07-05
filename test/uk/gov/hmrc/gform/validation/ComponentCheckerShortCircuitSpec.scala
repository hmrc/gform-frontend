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

class ComponentCheckerShortCircuitSpec extends AnyFunSpec with Matchers {

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
  val successProg = successProgram(())

  describe("shortCircuitProgram combining no programs") {
    val program = List[CheckProgram[Unit]](successProg).shortCircuitProgram
    it("the  ErrorReportInterpreter should return success") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  ShortCircuitProgram should return success") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  NonShortCircuitProgram should return success") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
  }
  describe("nonShortCircuitProgram combining no programs") {
    val program = List[CheckProgram[Unit]](successProg).nonShortCircuitProgram
    it("the  ErrorReportInterpreter should return success") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  ShortCircuitProgram should return success") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  NonShortCircuitProgram should return success") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
  }

  describe("shortCircuitProgram combining one successful program") {
    val program = List(successProg).shortCircuitProgram
    it("the  ErrorReportInterpreter should return success") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  ShortCircuitProgram should return success") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  NonShortCircuitProgram should return success") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
  }

  describe("nonShortCircuitProgram combining one successful program") {
    val program = List(successProg).nonShortCircuitProgram
    it("the  ErrorReportInterpreter should return success") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  ShortCircuitProgram should return success") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
    it("the  NonShortCircuitProgram should return success") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe ().asRight.asRight
    }
  }

  describe("shortCircuitProgram combining successful and failed programs") {
    val failedProg = errorProgram[Unit](gformError1)
    val program = List(successProg, failedProg).shortCircuitProgram
    it("the  ErrorReportInterpreter should return failed error") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit].asRight[GformError]
    }
    it("the  ShortCircuitProgram should return failed error") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit]
    }
    it("the  NonShortCircuitProgram should return failed error") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit].asRight[GformError]
    }
  }

  describe("nonShortCircuitProgram combining successful and failed programs") {
    val failedProg = errorProgram[Unit](gformError1)
    val program = List(successProg, failedProg).nonShortCircuitProgram
    it("the  ErrorReportInterpreter should return failed error") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit].asRight[GformError]
    }
    it("the  ShortCircuitProgram should return failed error") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit]
    }
    it("the  NonShortCircuitProgram should return failed error") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit].asRight[GformError]
    }
  }

  describe("shortCircuitProgram combining successful and 2 failed programs") {
    val failedProg1 = errorProgram[Unit](gformError1)
    val failedProg2 = errorProgram[Unit](gformError2)
    val program = List(successProg, failedProg1, failedProg2).shortCircuitProgram
    it("the  ErrorReportInterpreter should return all failed errors") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe (gformError1 |+| gformError2).asLeft[Unit].asRight[GformError]
    }
    it("the  ShortCircuitProgram should return failed error") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit]
    }
    it("the  NonShortCircuitProgram should return failed error") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe gformError1.asLeft[Unit].asRight[GformError]
    }
  }

  describe("nonShortCircuitProgram combining successful and 2 failed programs") {
    val failedProg1 = errorProgram[Unit](gformError1)
    val failedProg2 = errorProgram[Unit](gformError2)
    val program = List(successProg, failedProg1, failedProg2).nonShortCircuitProgram
    it("the  ErrorReportInterpreter should return all failed error") {
      val validationResult = program.foldMap(ErrorReportInterpreter)
      validationResult shouldBe (gformError1 |+| gformError2).asLeft[Unit].asRight[GformError]
    }
    it("the  ShortCircuitProgram should return all failed error") {
      val validationResult = program.foldMap(ShortCircuitInterpreter)
      validationResult shouldBe (gformError1 |+| gformError2).asLeft[Unit]
    }
    it("the  NonShortCircuitProgram should return all failed error") {
      val validationResult = program.foldMap(NonShortCircuitInterpreter)
      validationResult shouldBe (gformError1 |+| gformError2).asLeft[Unit].asRight[GformError]
    }
  }
}
