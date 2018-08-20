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

import org.scalactic.source.Position
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class DependencyGraphSpec extends Spec {

  "Recalculation" should "recalculate single dependency" in {

    val inputData = mkFormData(
      "a" -> "123",
      "b" -> "eee"
    )

    val expectedOutputData = mkFormData(
      "a" -> "123",
      "b" -> "123"
    )

    val sections =
      mkSection("page 1", mkFormComponent("a", Value) :: Nil) ::
        mkSection("page 2", mkFormComponent("b", FormCtx("a")) :: Nil) :: Nil

    verify(inputData, expectedOutputData, sections)

  }

  it should "detect cycle in dependencies (graph cannot be sorted)" in {

    val inputData = mkFormData(
      "a" -> "1",
      "b" -> "2"
    )

    val sections =
      mkSection("page 1", mkFormComponent("a", FormCtx("b")) :: mkFormComponent("b", FormCtx("a")) :: Nil) :: Nil

    val res = Recalculation.recalculateFormData(inputData, mkFormTemplate(sections))

    res match {
      case Left(NoTopologicalOrder(_, _)) => succeed
      case otherwise                      => fail
    }
  }

  it should "detect missing submission data" in {

    val inputData = mkFormData(
      "b" -> "2"
    )

    val sections =
      mkSection("page 1", mkFormComponent("a", Value) :: mkFormComponent("b", FormCtx("a")) :: Nil) :: Nil

    val res = Recalculation.recalculateFormData(inputData, mkFormTemplate(sections))

    res match {
      case Left(NoDataFound(FormComponentId("a"), _)) => succeed
      case otherwise                                  => fail
    }
  }

  it should "detect missing FormComponent data" in {

    val inputData = mkFormData(
      "a" -> "1",
      "b" -> "2"
    )

    val sections =
      mkSection("page 1", mkFormComponent("a", Value) :: mkFormComponent("b", FormCtx("c")) :: Nil) :: Nil

    val res = Recalculation.recalculateFormData(inputData, mkFormTemplate(sections))

    res match {
      case Left(NoFormComponent(FormComponentId("c"), _)) => succeed
      case otherwise                                      => fail
    }
  }

  it should "recalculate chain of dependencies" in {
    val inputData = mkFormData(
      "a" -> "100",
      "b" -> "100",
      "c" -> "100",
      "d" -> "100"
    )

    val expectedOutputData = mkFormData(
      "a" -> "100",
      "b" -> "110",
      "c" -> "220",
      "d" -> "330"
    )

    val sections =
      mkSection("page 1", mkFormComponent("a", Value) :: Nil) ::
        mkSection("page 2", mkFormComponent("b", Add(FormCtx("a"), Constant("10"))) :: Nil) ::
        mkSection("page 3", mkFormComponent("c", Multiply(FormCtx("b"), Constant("2"))) :: Nil) ::
        mkSection("page 4", mkFormComponent("d", Add(FormCtx("b"), FormCtx("c"))) :: Nil) :: Nil

    verify(inputData, expectedOutputData, sections)

  }

  it should "recalculate trees of chain of dependencies" in {
    val inputData = mkFormData(
      "a" -> "100",
      "b" -> "100",
      "c" -> "200",
      "d" -> "200"
    )

    val expectedOutputData = mkFormData(
      "a" -> "100",
      "b" -> "110",
      "c" -> "200",
      "d" -> "220"
    )

    val sections =
      mkSection("page 1", mkFormComponent("a", Value) :: Nil) ::
        mkSection("page 2", mkFormComponent("b", Add(FormCtx("a"), Constant("10"))) :: Nil) ::
        mkSection("page 3", mkFormComponent("c", Value) :: Nil) ::
        mkSection("page 4", mkFormComponent("d", Add(FormCtx("c"), Constant("20"))) :: Nil) :: Nil

    verify(inputData, expectedOutputData, sections)

  }

  private def verify(input: FormData, expectedOutput: FormData, sections: List[Section])(
    implicit position: Position) = {
    val output = Recalculation.recalculateFormData(input, mkFormTemplate(sections))

    Right(expectedOutput) shouldBe output

  }

  private def mkFormData(fields: (String, String)*): FormData =
    FormData(fields.map { case (fcId, value) => FormField(FormComponentId(fcId), value) })

  private def mkSection(name: String, formComponents: List[FormComponent]) =
    Section(
      name,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      formComponents,
      None
    )

  private def mkFormComponent(name: String, expr: Expr) =
    FormComponent(
      FormComponentId(name),
      Text(Sterling, expr),
      name,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None)

  private def mkFormTemplate(sections: List[Section]) = FormTemplate(
    FormTemplateId("tst1"),
    "Dependecy heavy experiment",
    "",
    Some(BetaBanner),
    None,
    None,
    DmsSubmission("R&D", TextExpression(FormCtx("utrRepComp")), "CCG-CT-RandDreports", "CCG", None),
    HMRCAuthConfigWithAuthModule(AuthConfigModule("hmrc")),
    "randd_confirmation_submission",
    "http://www.google.co.uk",
    "http://www.yahoo.co.uk",
    sections,
    AcknowledgementSection(
      "Acknowledgement Page",
      Some("this page is to acknowledge submission"),
      Some("shortName for acknowledgement"),
      List(`fieldValue - info`)
    ),
    DeclarationSection("Declaration", None, None, Nil)
  )
}
