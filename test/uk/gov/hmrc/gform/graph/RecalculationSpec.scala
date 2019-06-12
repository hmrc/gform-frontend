/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.data.NonEmptyList
import cats.implicits._
import org.scalactic.source.Position
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.GraphSpec
import uk.gov.hmrc.gform.Helpers._
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
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

  it should "change to upper case when toUpperCase is true" in {
    val inputData = mkData(
      "a" -> "abc",
      "b" -> "def",
      "c" -> "ghi"
    )

    val expectedOutputData = mkData(
      "a" -> "ABC",
      "b" -> "def",
      "c" -> "ghi"
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Text(BasicText, Value, DisplayWidth.DEFAULT, IsUpperCase)),
          mkFormComponent("b", Text(BasicText, Value, DisplayWidth.DEFAULT, IsNotUpperCase)),
          mkFormComponent("c", Text(BasicText, Value, DisplayWidth.DEFAULT))
        )
      )
    )

    verify(inputData, expectedOutputData, sections)
  }

  it should "recalculate IdNumberValue for HmrcTaxPeriod component" in {

    val inputData = mkData(
      "a" -> "123",
      "b" -> "eee"
    )

    val expectedOutputData = mkData(
      "a" -> "123",
      "b" -> "eee"
    )

    val hmrcTaxPeriod = HmrcTaxPeriod(IdType("idType"), FormCtx("a"), RegimeType("RegimeType"))

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", hmrcTaxPeriod)
        )
      )
    )

    val expectedRecData = RecData(
      expectedOutputData,
      Map(RecalculatedTaxPeriodKey(FormComponentId("b"), hmrcTaxPeriod) -> IdNumberValue("123")))

    verifyRecData(inputData, expectedRecData, sections)
  }

  it should "handle group component" in {

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData(), mkData()),
      (mkData("a" -> "2"),                                                     mkData("a" -> "2", "b" -> "2")),
      (mkData("a" -> "2", "b" -> "",                "c" -> ""),                mkData("a" -> "2", "b" -> "2",               "c" -> "")),
      (mkData("a" -> "2", "b" -> "",  "1_b" -> "",  "c" -> "",  "1_c" -> ""),  mkData("a" -> "2", "b" -> "2", "1_b" -> "2", "c" -> "", "1_c" -> "")),
      (mkData("a" -> "2", "b" -> "2", "1_b" -> "2", "c" -> "3", "1_c" -> "3"), mkData("a" -> "2", "b" -> "2", "1_b" -> "2", "c" -> "3", "1_c" -> "3"))
      // format: on
    )
    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("group", mkGroup(5, List(mkFormComponent("b", FormCtx("a")), mkFormComponent("c", Value))))
        )
      )
    )

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
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

    val res =
      recalculation.recalculateFormData(
        inputData,
        mkFormTemplate(sections),
        ExampleData.authContext,
        ThirdPartyData.empty,
        EnvelopeId(""))

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
          Enrolments(Set(Enrolment("NINO").copy(identifiers = List(EnrolmentIdentifier("NINO", "AA111111A")))))),
      ThirdPartyData.empty,
      EnvelopeId("")
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

    val res =
      recalculation.recalculateFormData(
        inputData,
        mkFormTemplate(sections),
        ExampleData.authContext,
        ThirdPartyData.empty,
        EnvelopeId(""))

    res match {
      case Right(formDataRecalculated) =>
        formDataRecalculated.data shouldBe Map((FormComponentId("a"), Seq("data-returned-from-eeitt")))
      case _ => fail
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
      ),
      ThirdPartyData.empty,
      EnvelopeId("")
    )

    res match {
      case Right(formDataRecalculated) =>
        formDataRecalculated.data shouldBe Map((FormComponentId("a"), Seq("individual")))
      case _ => fail
    }
  }

  it should "handle submitMode = derived" in {

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1"),             mkData("a" -> "1", "b" -> "3")),
      (mkData("a" -> "1", "b" -> "2"), mkData("a" -> "1", "b" -> "2"))
      // format: on
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", Add(FormCtx("a"), Constant("2"))).copy(derived = true)
        )
      )
    )

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
  }

  it should "not recompute values with missing submission data" in {

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1", "b" -> "2"), mkData("a" -> "1", "b" -> "1")),
      (mkData(            "b" -> "2"), mkData(            "b" -> "2"))
      // format: on
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", FormCtx("a"))
        )
      )
    )

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
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

    val res =
      recalculation.recalculateFormData(
        inputData,
        mkFormTemplate(sections),
        ExampleData.authContext,
        ThirdPartyData.empty,
        EnvelopeId(""))

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

  it should "not recalculate sections which are invisible based on choice component" in {

    val text = Text(AnyText, Value)
    val choice =
      Choice(YesNo, NonEmptyList.of(toLocalisedString("yes"), toLocalisedString("no")), Vertical, List.empty, None)

    val formComponentIds = Table(
      // format: off
      ("component", "input", "output"),
      (choice, mkData("a" -> "0", "b" -> "0", "c" -> "100", "d" -> "200"), mkData("a" -> "0", "b" -> "0", "c" -> "0",   "d" -> "0")),
      (choice, mkData("a" -> "0", "b" -> "1", "c" -> "100", "d" -> "200"), mkData("a" -> "0", "b" -> "1", "c" -> "1",   "d" -> "1")),
      (choice, mkData("a" -> "1", "b" -> "0", "c" -> "100", "d" -> "200"), mkData("a" -> "1", "b" -> "0", "c" -> "100", "d" -> "200")),
      (choice, mkData("a" -> "1", "b" -> "1", "c" -> "100", "d" -> "200"), mkData("a" -> "1", "b" -> "1", "c" -> "100", "d" -> "200")),
      (text,   mkData("a" -> "0", "b" -> "0", "c" -> "100", "d" -> "200"), mkData("a" -> "0", "b" -> "0", "c" -> "0",   "d" -> "0")),
      (text,   mkData("a" -> "0", "b" -> "1", "c" -> "100", "d" -> "200"), mkData("a" -> "0", "b" -> "1", "c" -> "1",   "d" -> "1")),
      (text,   mkData("a" -> "1", "b" -> "0", "c" -> "100", "d" -> "200"), mkData("a" -> "1", "b" -> "0", "c" -> "1",   "d" -> "1")),
      (text,   mkData("a" -> "1", "b" -> "1", "c" -> "100", "d" -> "200"), mkData("a" -> "1", "b" -> "1", "c" -> "1",   "d" -> "1"))
      // format: on
    )

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), Constant("0")))
    val includeIf2 = IncludeIf(Equals(FormCtx("b"), Constant("0")))
    val includeIf3 = IncludeIf(Equals(FormCtx("b"), Constant("1")))

    forAll(formComponentIds) { (component, input, expectedOutput) ⇒
      val sections =
        mkSection(List(mkFormComponent("a", component))) ::
          mkSectionIncludeIf(List(mkFormComponent("b", component)), includeIf1) ::
          mkSectionIncludeIf(List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b")))), includeIf2) ::
          mkSectionIncludeIf(List(mkFormComponent("d", Add(FormCtx("a"), FormCtx("b")))), includeIf3) :: Nil

      verify(input, expectedOutput, sections)
    }
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
      "a" -> "1"
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
      (mkData("a" -> "1"),                           mkData("a" -> "1")),
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

  it should "recalculate fields on repeated sections" in {

    val data = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1", "b" -> "1", "c" -> "0", "d" -> "0", "e" -> "0"), mkData("a" -> "1", "b" -> "1", "c" -> "0", "d" -> "2", "e" -> "2"))
      // format: on
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSection(
        List(mkFormComponent(
          "c",
          RevealingChoice(NonEmptyList.of(
            RevealingChoiceElement(
              toLocalisedString("Yes"),
              mkFormComponent("d", Add(FormCtx("a"), FormCtx("b"))) :: Nil,
              false),
            RevealingChoiceElement(
              toLocalisedString("No"),
              mkFormComponent("e", Add(FormCtx("a"), FormCtx("b"))) :: Nil,
              false)
          ))
        ))) :: Nil

    forAll(data) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }

  }

  it should "detect non-selected fields on revealing choice component" in {
    val data = Table(
      // format: off
      ("input", "output"),
      (mkData("rc" -> "0", "a" -> "10", "b" -> "11"), mkData("rc" -> "0", "a" -> "10", "b" -> "11", "res" -> "10")),
      (mkData("rc" -> "1", "a" -> "10", "b" -> "11"), mkData("rc" -> "1", "a" -> "10", "b" -> "11", "res" -> "11"))
      // format: on
    )

    val sections =
      mkSection(
        List(mkFormComponent(
          "rc",
          RevealingChoice(NonEmptyList.of(
            RevealingChoiceElement(toLocalisedString("Yes"), mkFormComponent("a", Value) :: Nil, false),
            RevealingChoiceElement(toLocalisedString("No"), mkFormComponent("b", Value) :: Nil, false)
          ))
        ))) ::
        mkSection(List(mkFormComponent("res", Add(FormCtx("a"), FormCtx("b"))))) ::
        Nil

    forAll(data) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
  }

  it should "not recalculate editable field" in {

    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "1"),                           mkData("a" -> "1")),
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
      (mkData("a" -> "1"),                           mkData("a" -> "1")),
      (mkData("a" -> "0"),                           mkData("a" -> "0")),
      (mkData("a" -> "0", "b" -> "1"),               mkData("a" -> "0", "b" -> "1"            , "d" -> "1")),
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
      (mkData("a" -> "0", "b" -> "1", "bb" -> "10",               "d" -> "10"), mkData("a" -> "0", "b" -> "1", "bb" -> "10",               "d" -> "10")),
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
       mkData("a" -> "1", "b" -> "2", "bb" -> "10",                           "d" -> "10", "e" -> "0" )),
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

  it should "handle else expression" in {
    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "A", "b" -> "B", "c" -> "C"), mkData("a" -> "A", "b" -> "B", "c" -> "C", "z" -> "A")),
      (mkData(            "b" -> "B", "c" -> "C"), mkData(            "b" -> "B", "c" -> "C", "z" -> "B")),
      (mkData(                        "c" -> "C"), mkData(                        "c" -> "C", "z" -> "C")),

      (mkData("a" -> "A", "b" -> "B", "c" -> "C"), mkData("a" -> "A", "b" -> "B", "c" -> "C", "z" -> "A")),
      (mkData("a" -> "",  "b" -> "B", "c" -> "C"), mkData("a" -> "",  "b" -> "B", "c" -> "C", "z" -> "B")),
      (mkData("a" -> "",  "b" -> "",  "c" -> "C"), mkData("a" -> "",  "b" -> "",  "c" -> "C", "z" -> "C"))
      // format: on
    )

    val sections =
      mkSection(List(mkFormComponent("a", Value), mkFormComponent("b", Value), mkFormComponent("c", Value))) ::
        mkSection(List(mkFormComponent("z", Else(FormCtx("a"), Else(FormCtx("b"), FormCtx("c")))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
  }

  it should "handle invisible fields in else expression" in {
    val formComponentIds = Table(
      // format: off
      ("input", "output"),
      (mkData("a" -> "A", "b" -> "B", "c" -> "C"), mkData("a" -> "A", "b" -> "B", "c" -> "C", "z" -> "B")),
      (mkData(            "b" -> "B", "c" -> "C"), mkData(            "b" -> "B", "c" -> "C", "z" -> "C"))
      // format: on
    )

    val includeIf1 = IncludeIf(Equals(FormCtx("a"), Constant("A")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("b", Value)), includeIf1) ::
        mkSection(List(mkFormComponent("c", Value))) ::
        mkSection(List(mkFormComponent("z", Else(FormCtx("b"), FormCtx("c"))))) :: Nil

    forAll(formComponentIds) { (input, expectedOutput) ⇒
      verify(input, expectedOutput, sections)
    }
  }

  private def verify(input: Data, expectedOutput: Data, sections: List[Section])(implicit position: Position) = {
    val output =
      recalculation.recalculateFormData(
        input,
        mkFormTemplate(sections),
        ExampleData.authContext,
        ThirdPartyData.empty,
        EnvelopeId(""))
    Right(expectedOutput) shouldBe output.map(_.data)
  }

  private def verifyRecData(input: Data, expectedRecData: RecData, sections: List[Section])(
    implicit position: Position) = {
    val output =
      recalculation.recalculateFormData(
        input,
        mkFormTemplate(sections),
        ExampleData.authContext,
        ThirdPartyData.empty,
        EnvelopeId(""))
    Right(expectedRecData) shouldBe output.map(_.recData)

  }
}
