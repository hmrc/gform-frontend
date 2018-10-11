/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.graph

import cats.implicits._
import org.scalactic.source.Position
import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import FormTemplateBuilder._
import uk.gov.hmrc.http.HeaderCarrier

class RecalculationSpec extends FlatSpec with Matchers with GraphSpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  type EitherEffect[A] = Either[GraphException, A]

  val recalculation: Recalculation[EitherEffect, GraphException] =
    new Recalculation[EitherEffect, GraphException](booleanExprEval, ((s: GraphException) => s))

  "recalculation" should "recalculate single dependency" in {

    val inputData = mkData(
      "a" -> "123",
      "b" -> "eee"
    )

    val expectedOutputData = mkData(
      "a" -> "123",
      "b" -> "123"
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", FormCtx("a"))
        )
      )
    )

    verify(inputData, expectedOutputData, sections)

  }

  it should "detect cycle in dependencies (graph cannot be sorted)" in {

    val inputData = mkData(
      "a" -> "1",
      "b" -> "2"
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", FormCtx("b")),
          mkFormComponent("b", FormCtx("a"))
        )
      )
    )

    val res = recalculation.recalculateFormData(inputData, mkFormTemplate(sections), ExampleData.authContext)

    res match {
      case Left(NoTopologicalOrder(_, _)) => succeed
      case otherwise                      => fail
    }
  }

  it should "handle AuthCtx" in {

    val inputData = mkData()

    val sections =
      mkSection(
        mkFormComponent("a", AuthCtx(PayeNino)) :: Nil
      ) :: Nil

    val res = recalculation.recalculateFormData(
      inputData,
      mkFormTemplate(sections),
      ExampleData.authContext.copy(
        enrolments =
          Enrolments(Set(Enrolment("NINO").copy(identifiers = List(EnrolmentIdentifier("NINO", "AA111111A"))))))
    )

    res match {
      case Right(formDataRecalculated) =>
        formDataRecalculated.data shouldBe Map((FormComponentId("a"), Seq("AA111111A")))
      case otherwise => fail
    }
  }

  it should "handle EeiitCtx" in {

    val inputData = mkData()

    val sections =
      mkSection(
        mkFormComponent("a", EeittCtx(BusinessUser)) :: Nil
      ) :: Nil

    val res = recalculation.recalculateFormData(inputData, mkFormTemplate(sections), ExampleData.authContext)

    res match {
      case Right(formDataRecalculated) =>
        formDataRecalculated.data shouldBe Map((FormComponentId("a"), Seq("data-returned-from-eeitt")))
      case otherwise => fail
    }
  }

  it should "handle UserCtx" in {

    val inputData = mkData()

    val sections =
      mkSection(
        mkFormComponent("a", UserCtx(AffinityGroup)) :: Nil
      ) :: Nil

    val res = recalculation.recalculateFormData(
      inputData,
      mkFormTemplate(sections),
      ExampleData.authContext.copy(
        affinityGroup = Some(uk.gov.hmrc.auth.core.AffinityGroup.Individual)
      )
    )

    res match {
      case Right(formDataRecalculated) =>
        formDataRecalculated.data shouldBe Map((FormComponentId("a"), Seq("individual")))
      case otherwise => fail
    }
  }

  it should "missing submission data is treated as if submission has been empty string" in {

    val inputData = mkData(
      "b" -> "2"
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", FormCtx("a"))
        )
      )
    )

    val res = recalculation.recalculateFormData(inputData, mkFormTemplate(sections), ExampleData.authContext)

    res match {
      case Right(formDataRecalculated) =>
        formDataRecalculated.data shouldBe Map((FormComponentId("a"), Seq("")), (FormComponentId("b"), Seq("")))
      case otherwise => fail
    }
  }

  it should "detect invalid submission data and report error" in {

    val inputData = mkData(
      "a" -> "1",
      "b" -> "2",
      "c" -> "3"
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", FormCtx("c"))
        )
      )
    )

    val res = recalculation.recalculateFormData(inputData, mkFormTemplate(sections), ExampleData.authContext)

    res match {
      case Left(NoFormComponent(fcId, map)) =>
        fcId shouldBe FormComponentId("c")
        map.keys should contain theSameElementsAs (FormComponentId("a") :: FormComponentId("b") :: Nil)
      case _ => fail
    }
  }

  it should "recalculate chain of dependencies" in {
    val inputData = mkData(
      "a" -> "100",
      "b" -> "100",
      "c" -> "100",
      "d" -> "100"
    )

    val expectedOutputData = mkData(
      "a" -> "100",
      "b" -> "110",
      "c" -> "220",
      "d" -> "330"
    )

    val sections = List(
      mkSection(mkFormComponent("a", Value) :: Nil),
      mkSection(mkFormComponent("b", Add(FormCtx("a"), Constant("10"))) :: Nil),
      mkSection(mkFormComponent("c", Multiply(FormCtx("b"), Constant("2"))) :: Nil),
      mkSection(mkFormComponent("d", Add(FormCtx("b"), FormCtx("c"))) :: Nil)
    )

    verify(inputData, expectedOutputData, sections)

  }

  it should "recalculate trees of chain of dependencies" in {
    val inputData = mkData(
      "a" -> "100",
      "b" -> "100",
      "c" -> "200",
      "d" -> "200"
    )

    val expectedOutputData = mkData(
      "a" -> "100",
      "b" -> "110",
      "c" -> "200",
      "d" -> "220"
    )

    val sections = List(
      mkSection(mkFormComponent("a", Value) :: Nil),
      mkSection(mkFormComponent("b", Add(FormCtx("a"), Constant("10"))) :: Nil),
      mkSection(mkFormComponent("c", Value) :: Nil),
      mkSection(mkFormComponent("d", Add(FormCtx("c"), Constant("20"))) :: Nil)
    )

    verify(inputData, expectedOutputData, sections)

  }

  it should "disregard invisible parts" in {
    val inputData = mkData(
      "a" -> "1",
      "b" -> "10",
      "c" -> "100"
    )

    val expectedOutputData = mkData(
      "a" -> "1",
      "b" -> "10",
      "c" -> "1"
    )

    val includeIf = IncludeIf(Equals(FormCtx("a"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value)), includeIf) ::
        mkSection(List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))) :: Nil

    verify(inputData, expectedOutputData, sections)

  }

  it should "do not disregard when no invisible parts" in {

    val inputData = mkData(
      "a" -> "1",
      "b" -> "10"
    )

    val expectedOutputData = mkData(
      "a" -> "1",
      "b" -> "10",
      "c" -> "11"
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSection(List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))) :: Nil

    verify(inputData, expectedOutputData, sections)

  }

  it should "recalculation with no complete submission" in {

    val inputData = mkData(
      "a" -> "1"
    )

    val expectedOutputData = mkData(
      "a" -> "1",
      "b" -> "",
      "c" -> ""
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSection(List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))) :: Nil

    verify(inputData, expectedOutputData, sections)

  }

  it should "recalculate editable field" in {

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1"),                           mkData("a" -> "1", "b" -> "",   "c" -> "")),
      (mkData("a" -> "1", "b" -> "",   "c" -> ""  ), mkData("a" -> "1", "b" -> "",   "c" -> "")),
      (mkData("a" -> "1", "b" -> "10"             ), mkData("a" -> "1", "b" -> "10", "c" -> "11")),
      (mkData("a" -> "1", "b" -> "10", "c" -> "12"), mkData("a" -> "1", "b" -> "10", "c" -> "11"))
      // format: on
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSection(List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
  }

  it should "not recalculate editable field" in {

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1"),                           mkData("a" -> "1", "b" -> "",   "c" -> "")),
      (mkData("a" -> "1", "b" -> "",   "c" -> ""  ), mkData("a" -> "1", "b" -> "",   "c" -> "")),
      (mkData("a" -> "1", "b" -> "10"             ), mkData("a" -> "1", "b" -> "10", "c" -> "11")),
      (mkData("a" -> "1", "b" -> "10", "c" -> "12"), mkData("a" -> "1", "b" -> "10", "c" -> "12"))
      // format: on
    )

    val sections =
      mkSection(List(mkFormComponentEditable("a", Value))) ::
        mkSection(List(mkFormComponentEditable("b", Value))) ::
        mkSection(List(mkFormComponentEditable("c", Add(FormCtx("a"), FormCtx("b"))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
  }

  it should "complex recalculation with 2 simple sections containing includeIf" in {
    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1"),                           mkData("a" -> "1", "b" -> "",  "c" -> "",  "d" -> "")),
      (mkData("a" -> "0"),                           mkData("a" -> "0", "b" -> "",  "c" -> "",  "d" -> "")),
      (mkData("a" -> "0", "b" -> "1"),               mkData("a" -> "0", "b" -> "1", "c" -> "",  "d" -> "1")),
      (mkData("a" -> "0", "b" -> "1", "c" -> "1"),   mkData("a" -> "0", "b" -> "1", "c" -> "1", "d" -> "1")),
      (mkData("a" -> "0", "b" -> "0", "c" -> "1"),   mkData("a" -> "0", "b" -> "0", "c" -> "1", "d" -> "1"))
      // format: on
    )

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), Constant("0")))
    val includeIf2 = IncludeIf(Equals(FormCtx("b"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value)), includeIf1) ::
        mkSectionIncludeIf(List(mkFormComponent("c", Value)), includeIf2) ::
        mkSection(List(mkFormComponent("d", Add(FormCtx("b"), FormCtx("c"))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }

  }
  it should "complex recalculation with 2 complex sections containing includeIf" in {
    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "0", "b" -> "1", "bb" -> "10",               "d" -> "10"), mkData("a" -> "0", "b" -> "1", "bb" -> "10", "cc" -> "",   "d" -> "10")),
      (mkData("a" -> "0", "b" -> "0", "bb" -> "10", "cc" -> "10", "d" -> "10"), mkData("a" -> "0", "b" -> "0", "bb" -> "10", "cc" -> "10", "d" -> "20")),
      (mkData("a" -> "0", "b" -> "1", "bb" -> "10", "cc" -> "10", "d" -> "10"), mkData("a" -> "0", "b" -> "1", "bb" -> "10", "cc" -> "10", "d" -> "10"))
      // format: on
    )

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), Constant("0")))
    val includeIf2 = IncludeIf(Equals(FormCtx("b"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value), mkFormComponent("bb", Value)), includeIf1) ::
        mkSectionIncludeIf(List(mkFormComponent("cc", Value)), includeIf2) ::
        mkSection(List(mkFormComponent("d", Add(FormCtx("bb"), FormCtx("cc"))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }

  }

  it should "complex recalculation with 3 complex sections containing includeIf" in {
    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1", "b" -> "2", "bb" -> "10",                           "d" -> "10"             ),
       mkData("a" -> "1", "b" -> "2", "bb" -> "10", "c" -> "",  "cc" -> "",   "d" -> "10", "e" -> "0" )),
      (mkData("a" -> "1", "b" -> "0", "bb" -> "10", "c" -> "0", "cc" -> "10", "d" -> "10"             ),
       mkData("a" -> "1", "b" -> "0", "bb" -> "10", "c" -> "0", "cc" -> "10", "d" -> "10", "e" -> "0" )),
      (mkData("a" -> "1", "b" -> "1", "bb" -> "10", "c" -> "0", "cc" -> "10", "d" -> "10"             ),
       mkData("a" -> "1", "b" -> "1", "bb" -> "10", "c" -> "0", "cc" -> "10", "d" -> "10", "e" -> "10")),
      (mkData("a" -> "1", "b" -> "1", "bb" -> "10", "c" -> "1", "cc" -> "10", "d" -> "10"             ),
       mkData("a" -> "1", "b" -> "1", "bb" -> "10", "c" -> "1", "cc" -> "10", "d" -> "10", "e" -> "20")),
      (mkData("a" -> "0", "b" -> "1", "bb" -> "10", "c" -> "1", "cc" -> "10", "d" -> "10"             ),
       mkData("a" -> "0", "b" -> "1", "bb" -> "10", "c" -> "1", "cc" -> "10", "d" -> "10", "e" -> "0" ))
      // format: on
    )

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), Constant("1")))
    val includeIf2 = IncludeIf(Equals(FormCtx("b"), Constant("1")))
    val includeIf3 = IncludeIf(Equals(FormCtx("c"), Constant("1")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value), mkFormComponent("bb", Value)), includeIf1) ::
        mkSectionIncludeIf(List(mkFormComponent("c", Value), mkFormComponent("cc", Value)), includeIf2) ::
        mkSectionIncludeIf(List(mkFormComponent("d", Value)), includeIf3) ::
        mkSection(List(mkFormComponent("e", Add(FormCtx("cc"), FormCtx("d"))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }

  }

  private def verify(input: Data, expectedOutput: Data, sections: List[Section])(implicit position: Position) = {
    val output = recalculation.recalculateFormData(input, mkFormTemplate(sections), ExampleData.authContext)
    Right(expectedOutput) shouldBe output.map(_.data)

  }

  private def mkData(fields: (String, String)*): Data =
    fields.map { case (fcId, value) => FormComponentId(fcId) -> Seq(value) }.toMap

}
