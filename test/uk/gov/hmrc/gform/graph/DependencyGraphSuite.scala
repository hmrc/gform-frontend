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

package uk.gov.hmrc.gform.graph

import cats.Id
import cats.data.NonEmptyList
import munit.FunSuite
import play.api.i18n.Messages

import scala.language.implicitConversions
import uk.gov.hmrc.gform.Helpers.{ toSmartString, toSmartStringExpression }
import uk.gov.hmrc.gform.eval.{ AllFormTemplateExpressions, ExprMetadata }
import uk.gov.hmrc.gform.models.{ Basic, FormModelBuilder, Interim, VariadicFormDataSupport }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ DataExpanded, FormModel, FormModelSupport, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AtlDescription, Choice, Constant, Contains, FormComponent, FormComponentId, FormComponentValidator, FormCtx, FormTemplate, GreaterThan, IncludeIf, IndexOf, Number, Page, Radio, RevealingChoice, RevealingChoiceElement, Section, Sum, ValidIf, Value, Vertical }
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.graph.GraphNode
import FormTemplateBuilder._

class DependencyGraphSuite extends FunSuite with FormModelSupport with VariadicFormDataSupport {

  implicit def simpleSyntax(s: String): FormComponentId = FormComponentId(s)
  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = play.api.test.Helpers.stubMessages(play.api.test.Helpers.stubMessagesApi(Map.empty))

  private val emptyPage: Page[Basic] = Page(
    toSmartString(""),
    None,
    None,
    None,
    None,
    None,
    None,
    List.empty[FormComponent],
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )
  private val emptyAddToList: Section.AddToList = Section.AddToList(
    toSmartString(""),
    Some(toSmartString("")),
    Some(toSmartString("")),
    AtlDescription.SmartStringBased(toSmartString("")),
    toSmartString(""),
    toSmartString(""),
    toSmartString(""),
    None,
    NonEmptyList.one(emptyPage),
    None,
    None,
    None,
    mkFormComponent("choice", Value),
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None,
    None
  )

  test("DependencyGraph must allow sum in ValidIf in Add To List") {
    val sections = List(
      emptyAddToList.copy(pages =
        NonEmptyList.of(
          emptyPage.copy(fields =
            List(
              mkFormComponent("offset", Value, Number()).copy(validators =
                List(
                  FormComponentValidator(
                    ValidIf(GreaterThan(Sum(FormCtx(FormComponentId("offset"))), Constant("1000"))),
                    toSmartString("Offset error")
                  )
                )
              )
            )
          )
        )
      )
    )
    val variadicData: VariadicFormData[SourceOrigin.OutOfDate] =
      variadicFormDataMany(
        "1_choice" -> List("0"),
        "2_choice" -> List.empty[String]
      ) ++
        variadicFormData(
          "1_offset" -> "1000",
          "2_offset" -> "1000"
        )

    val expected: List[(Int, Set[GraphNode])] = List(
      (0, Set(GraphNode.Simple("offset"))),
      (1, Set(GraphNode.Expr(FormCtx("2_offset")), GraphNode.Expr(FormCtx("1_offset")))),
      (2, Set(GraphNode.Simple("2_offset"), GraphNode.Simple("1_offset"))),
      (3, Set(GraphNode.Expr(Constant("1000"))))
    )

    val res: List[(Int, Set[GraphNode])] = layers(sections, variadicData)

    assertEquals(res, expected)
  }

  test("DependencyGraph must allow sum in erroMessage in Add To List") {
    val sections = List(
      emptyAddToList.copy(pages =
        NonEmptyList.of(
          emptyPage.copy(fields =
            List(
              mkFormComponent("offset", Value, Number()).copy(
                errorMessage = Some(toSmartStringExpression("", Sum(FormCtx(FormComponentId("offset")))))
              )
            )
          )
        )
      )
    )
    val variadicData: VariadicFormData[SourceOrigin.OutOfDate] =
      variadicFormDataMany(
        "1_choice" -> List("0"),
        "2_choice" -> List.empty[String]
      ) ++
        variadicFormData(
          "1_offset" -> "1000",
          "2_offset" -> "1000"
        )

    val expected: List[(Int, Set[GraphNode])] = List(
      (0, Set(GraphNode.Simple("offset"))),
      (1, Set(GraphNode.Expr(FormCtx("2_offset")), GraphNode.Expr(FormCtx("1_offset")))),
      (2, Set(GraphNode.Simple("2_offset"), GraphNode.Simple("1_offset")))
    )

    val res: List[(Int, Set[GraphNode])] = layers(sections, variadicData)

    assertEquals(res, expected)
  }

  test("DependencyGraph must allow add-to-list field to be referred outside add-to-list") {

    val choice =
      Choice(
        Radio,
        toOptionData(NonEmptyList.of("yes", "no", "maybe")),
        Vertical,
        List.empty,
        None,
        None,
        None,
        LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
        None,
        None,
        false
      )

    val sections: List[Section] = List(
      emptyAddToList.copy(
        pages = NonEmptyList.of(
          emptyPage.copy(fields =
            List(
              mkFormComponent("benefitTypeChoice", choice)
            )
          )
        )
      ),
      Section.NonRepeatingPage(
        emptyPage.copy(
          includeIf = Some(IncludeIf(Contains(FormCtx(FormComponentId("benefitTypeChoice")), Constant("1")))),
          fields = List(
            mkFormComponent("test", Value, Number())
          )
        )
      )
    )
    val variadicData: VariadicFormData[SourceOrigin.OutOfDate] = variadicFormDataMany(
      "1_benefitTypeChoice" -> List("1")
    )

    val expected: List[(Int, Set[GraphNode])] = List(
      (0, Set(GraphNode.Simple("test"))),
      (1, Set(GraphNode.Expr(FormCtx("benefitTypeChoice")), GraphNode.Expr(Constant("1")))),
      (2, Set(GraphNode.Simple("benefitTypeChoice"))),
      (3, Set(GraphNode.Expr(FormCtx("1_benefitTypeChoice")))),
      (4, Set(GraphNode.Simple("1_benefitTypeChoice")))
    )

    val res: List[(Int, Set[GraphNode])] = layers(sections, variadicData)

    assertEquals(res, expected)
  }

  test("DependencyGraph must allow add-to-list with Revealingchoice fields to be referred outside add-to-list") {

    val rc1 = mkFormComponent(
      "revealingChoice1",
      RevealingChoice(
        List(
          RevealingChoiceElement(
            toOptionData("Label1"),
            List(mkFormComponent("revealingChoiceField1", Value)),
            None,
            false
          ),
          RevealingChoiceElement(
            toOptionData("Label2"),
            List(mkFormComponent("revealingChoiceField2", Value)),
            None,
            false
          )
        ),
        true
      )
    )

    val sections = List(
      emptyAddToList.copy(pages =
        NonEmptyList.of(
          emptyPage.copy(fields = List(rc1))
        )
      ),
      Section.NonRepeatingPage(
        emptyPage.copy(
          includeIf = Some(IncludeIf(Contains(FormCtx(FormComponentId("revealingChoiceField1")), Constant("1")))),
          fields = List()
        )
      )
    )

    val variadicData: VariadicFormData[SourceOrigin.OutOfDate] =
      variadicFormDataMany(
        "1_choice" -> List("0"),
        "2_choice" -> List.empty[String]
      )

    val expected: List[(Int, Set[GraphNode])] = List(
      (
        0,
        Set(
          GraphNode.Expr(FormCtx(FormComponentId("revealingChoiceField1")))
        )
      ),
      (
        1,
        Set(
          GraphNode.Simple(FormComponentId("revealingChoiceField1"))
        )
      ),
      (
        2,
        Set(
          GraphNode.Expr(FormCtx(FormComponentId("2_revealingChoiceField1"))),
          GraphNode.Expr(FormCtx(FormComponentId("1_revealingChoiceField1")))
        )
      ),
      (
        3,
        Set(
          GraphNode.Simple(FormComponentId("2_revealingChoiceField1")),
          GraphNode.Simple(FormComponentId("1_revealingChoiceField1"))
        )
      )
    )

    val res: List[(Int, Set[GraphNode])] = layers(sections, variadicData)

    assertEquals(res, expected)
  }

  test("DependencyGraph must support add-to-list fields to be referred by IndexOf expression only") {

    val sections = List(
      emptyAddToList.copy(pages =
        NonEmptyList.of(
          emptyPage.copy(fields =
            List(
              mkFormComponent("amount", Value, Number())
            )
          )
        )
      ),
      Section.NonRepeatingPage(
        emptyPage.copy(
          fields = List(
            mkFormComponent("test", Value, Number()).copy(
              label = toSmartStringExpression("", IndexOf(FormComponentId("amount"), 2))
            )
          )
        )
      )
    )

    val variadicData: VariadicFormData[SourceOrigin.OutOfDate] =
      variadicFormData(
        "1_amount" -> "1000",
        "2_amount" -> "2000"
      ) ++ variadicFormDataMany(
        "1_choice" -> List("0"),
        "2_choice" -> List("1")
      )

    val expected: List[(Int, Set[GraphNode])] = List(
      (0, Set(GraphNode.Simple("test"))),
      (1, Set(GraphNode.Expr(FormCtx("amount")))),
      (2, Set(GraphNode.Simple("amount"))),
      (3, Set(GraphNode.Expr(FormCtx("1_amount")), GraphNode.Expr(FormCtx("2_amount")))),
      (4, Set(GraphNode.Simple("1_amount"), GraphNode.Simple("2_amount")))
    )

    val res: List[(Int, Set[GraphNode])] = layers(sections, variadicData)

    assertEquals(res, expected)
  }

  private def layers(
    sections: List[Section],
    variadicData: VariadicFormData[SourceOrigin.OutOfDate]
  ): List[(Int, Set[GraphNode])] =
    layers(mkFormTemplate(sections), variadicData)

  private def layers(
    formTemplate: FormTemplate,
    variadicData: VariadicFormData[SourceOrigin.OutOfDate]
  ): List[(Int, Set[GraphNode])] = {
    val fmb: FormModelBuilder[Throwable, Id] = mkFormModelBuilder(formTemplate)

    implicit val fmdsdso: FormModelVisibilityOptics[DataOrigin.Browser] =
      fmb.visibilityModel[DataOrigin.Browser, SectionSelectorType.Normal](variadicData, None, None)

    val fm: FormModel[DataExpanded] =
      fmb.expand[DataExpanded, SectionSelectorType.Normal](variadicData)

    val formTemplateExprs: Set[ExprMetadata] = AllFormTemplateExpressions(formTemplate)

    DependencyGraph.constructDependencyGraph(
      DependencyGraph.toGraph(fm.asInstanceOf[FormModel[Interim]], formTemplateExprs)
    ) match {

      case Left(node) => throw new CycleDetectedException(node.outer)
      case Right(topOrder) =>
        topOrder.toList.map { case (index, items) => (index, items.toSet) }
    }
  }
}
