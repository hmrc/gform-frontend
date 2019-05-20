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
import org.scalactic.source.Position
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.graph.DependencyGraph._
import FormTemplateBuilder._

class DependencyGraphSpec extends FlatSpec with Matchers {

  "Dependency Graph" should "handle simple case of two dependencies" in {
    val sections = List(
      mkSection(List(mkFormComponent("a", Value))),
      mkSection(List(mkFormComponent("b", FormCtx("a"))))
    )
    layers(sections) shouldBe List(
      (0, List("b")),
      (1, List("a"))
    )
  }

  it should "handle AuthCtx EeittCtx and UserCtx" in {
    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", AuthCtx(GG)),
          mkFormComponent("b", EeittCtx(BusinessUser)),
          mkFormComponent("c", UserCtx(AffinityGroup))))
    )
    layers(sections) shouldBe List(
      (0, List("a", "b", "c"))
    )
  }

  it should "handle HmrcTaxPeriod component" in {
    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent("b", HmrcTaxPeriod(IdType("idType"), FormCtx("a"), RegimeType("RegimeType")))
        )
      )
    )
    layers(sections) shouldBe List(
      (0, List("b")),
      (1, List("a"))
    )
  }

  it should "handle RevealingChoice component" in {
    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Value),
          mkFormComponent(
            "b",
            RevealingChoice(
              NonEmptyList.one(
                RevealingChoiceElement(toLocalisedString("Yes"), mkFormComponent("c", FormCtx("a")) :: Nil, false)))),
          mkFormComponent("d", FormCtx("c"))
        )
      )
    )
    layers(sections) shouldBe List(
      (0, List("d")),
      (1, List("c")),
      (2, List("a"))
    )
  }

  it should "handle long chain of dependencies" in {

    val sections = List(
      mkSection(List(mkFormComponent("a", Value))),
      mkSection(List(mkFormComponent("b", FormCtx("a")))),
      mkSection(List(mkFormComponent("c", FormCtx("b")))),
      mkSection(List(mkFormComponent("d", FormCtx("c")))),
      mkSection(List(mkFormComponent("e", FormCtx("d")))),
      mkSection(List(mkFormComponent("f", FormCtx("e"))))
    )

    layers(sections) shouldBe List(
      (0, List("f")),
      (1, List("e")),
      (2, List("d")),
      (3, List("c")),
      (4, List("b")),
      (5, List("a"))
    )
  }

  it should "handle Group deps" in {
    val sections = List(
      mkSection(List(mkFormComponent("a", Value))),
      mkSection(
        List(
          mkFormComponent("b", FormCtx("a")),
          mkFormComponent("group", mkGroup(5, List(mkFormComponent("c", FormCtx("a")))))
        )
      )
    )

    layers(sections) shouldBe List(
      (0, List("1_c", "2_c", "3_c", "4_c", "b", "c")),
      (1, List("a"))
    )
  }

  it should "Group deps Addition" in {
    val sections = List(
      mkSection(List(mkFormComponent("a", Value))),
      mkSection(
        List(
          mkFormComponent("b", FormCtx("a")),
          mkFormComponent("group", mkGroup(5, List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))))
        )
      )
    )

    layers(sections) shouldBe List(
      (0, List("1_c", "2_c", "3_c", "4_c", "c")),
      (1, List("b")),
      (2, List("a"))
    )
  }

  it should "handle dependencies between Groups" in {
    val sections = List(
      mkSection(List(mkFormComponent("a", Value))),
      mkSection(
        List(
          mkFormComponent("b", FormCtx("a")),
          mkFormComponent("group", mkGroup(5, List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))))
        )),
      mkSection(
        List(
          mkFormComponent("group2", mkGroup(3, List(mkFormComponent("d", FormCtx("c")))))
        ))
    )

    layers(sections) shouldBe List(
      (0, List("1_c", "1_d", "2_c", "2_d", "3_c", "4_c", "d")),
      (1, List("c")),
      (2, List("b")),
      (3, List("a"))
    )
  }

  it should "handle dependencies between Groups for Sum" in {
    val sections = List(
      mkSection(List(mkFormComponent("a", Value))),
      mkSection(
        List(
          mkFormComponent("b", FormCtx("a")),
          mkFormComponent("group", mkGroup(5, List(mkFormComponent("c", Add(FormCtx("a"), FormCtx("b"))))))
        )),
      mkSection(
        List(
          mkFormComponent("group2", mkGroup(3, List(mkFormComponent("d", Sum(FormCtx("c"))))))
        ))
    )

    layers(sections) shouldBe List(
      (0, List("1_d", "2_d", "d")),
      (1, List("1_c", "2_c", "3_c", "4_c", "c")),
      (2, List("b")),
      (3, List("a"))
    )
  }

  it should "handle includeIf's boolean expression" in {

    val includeIf = IncludeIf(Equals(Add(FormCtx("a"), FormCtx("b")), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("c", Value)), includeIf) :: Nil

    layers(sections) shouldBe List(
      (0, List("c")),
      (1, List("includeIf_2")),
      (2, List("a", "b"))
    )
  }

  it should "handle includeIf's boolean expression - chain of dependencies (version A)" in {

    val includeIf = IncludeIf(Equals(Add(FormCtx("a"), FormCtx("b")), Constant("0")))

    val includeIf2 = IncludeIf(Equals(FormCtx("c"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("c", Value)), includeIf) ::
        mkSectionIncludeIf(List(mkFormComponent("d", Value)), includeIf2) ::
        mkSection(List(mkFormComponent("e", Add(FormCtx("a"), FormCtx("b"))))) :: Nil

    layers(sections) shouldBe List(
      (0, List("d", "e")),
      (1, List("includeIf_3")),
      (2, List("c")),
      (3, List("includeIf_2")),
      (4, List("a", "b"))
    )
  }

  it should "handle includeIf's boolean expression - chain of dependencies with expression dependent on possible hidden section" in {

    val includeIf = IncludeIf(Equals(Add(FormCtx("a"), FormCtx("b")), Constant("0")))

    val includeIf2 = IncludeIf(Equals(FormCtx("c"), Constant("0")))

    val sections =
      mkSection(List(mkFormComponent("a", Value))) ::
        mkSection(List(mkFormComponent("b", Value))) ::
        mkSectionIncludeIf(List(mkFormComponent("c", Value)), includeIf) ::
        mkSectionIncludeIf(List(mkFormComponent("d", Value)), includeIf2) ::
        mkSection(List(mkFormComponent("e", Add(FormCtx("a"), FormCtx("d"))))) :: Nil

    layers(sections) shouldBe List(
      (0, List("e")),
      (1, List("d")),
      (2, List("includeIf_3")),
      (3, List("c")),
      (4, List("includeIf_2")),
      (5, List("a", "b"))
    )
  }

  it should "handle includeIf's boolean expression with revealingChoice" in {

    val includeIf = IncludeIf(Equals(FormCtx("a"), Constant("0")))

    val sections =
      mkSection(
        List(
          mkFormComponent(
            "a",
            RevealingChoice(NonEmptyList.one(
              RevealingChoiceElement(toLocalisedString("Yes"), mkFormComponent("b", Value) :: Nil, false)))))) ::
        mkSectionIncludeIf(List(mkFormComponent("c", Value)), includeIf) :: Nil

    layers(sections) shouldBe List(
      (0, List("c")),
      (1, List("includeIf_1")),
      (2, List("a"))
    )
  }

  private def layers(sections: List[Section])(implicit position: Position): List[(Int, List[String])] =
    constructDependencyGraph(toGraphFull(mkFormTemplate(sections))) match {
      case Left(e) => fail
      case Right(topOrder) =>
        topOrder.toList.map {
          case (index, items) =>
            (
              index,
              items.map(_.formComponentId.value)
            )
        }
    }
}
