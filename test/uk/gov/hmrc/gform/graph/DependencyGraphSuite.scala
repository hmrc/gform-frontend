/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Constant, FormComponent, FormComponentId, FormComponentValidator, FormCtx, GreaterThan, Number, Page, Section, Sum, ValidIf, Value }
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
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
    None,
    List.empty[FormComponent],
    None,
    None,
    None,
    None,
    None
  )
  private val emptyAddToList: Section.AddToList = Section.AddToList(
    toSmartString(""),
    Some(toSmartString("")),
    toSmartString(""),
    toSmartString(""),
    toSmartString(""),
    None,
    None,
    NonEmptyList.one(emptyPage),
    mkFormComponent("choice", Value),
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
        "1_choice" -> List(0),
        "2_choice" -> List.empty[Int]
      ) ++
        variadicFormData(
          "1_offset" -> "1000",
          "2_offset" -> "1000"
        )

    val expected: List[(Int, Set[GraphNode])] = List(
      (0, Set(GraphNode.Expr(FormCtx("2_offset")), GraphNode.Expr(FormCtx("1_offset")))),
      (1, Set(GraphNode.Simple("2_offset"), GraphNode.Simple("1_offset"))),
      (2, Set(GraphNode.Expr(Constant("1000"))))
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
        "1_choice" -> List(0),
        "2_choice" -> List.empty[Int]
      ) ++
        variadicFormData(
          "1_offset" -> "1000",
          "2_offset" -> "1000"
        )

    val expected: List[(Int, Set[GraphNode])] = List(
      (0, Set(GraphNode.Expr(FormCtx("2_offset")), GraphNode.Expr(FormCtx("1_offset")))),
      (1, Set(GraphNode.Simple("2_offset"), GraphNode.Simple("1_offset")))
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
      fmb.visibilityModel[DataOrigin.Browser, SectionSelectorType.Normal](variadicData, None)

    val fm: FormModel[DataExpanded] =
      fmb.expand[DataExpanded, SectionSelectorType.Normal](variadicData)

    val formTemplateExprs: List[ExprMetadata] = AllFormTemplateExpressions(formTemplate)

    DependencyGraph.constructDependencyGraph(
      DependencyGraph.toGraph(fm.asInstanceOf[FormModel[Interim]], formTemplateExprs)
    ) match {

      case Left(node) => throw new CycleDetectedException(node.toOuter)
      case Right(topOrder) =>
        topOrder.toList.map { case (index, items) => (index, items.toSet) }
    }
  }
}
