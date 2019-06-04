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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.prop.TableFor4
import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.SectionGen
import uk.gov.hmrc.gform.Helpers._
class SectionSpec extends Spec with GeneratorDrivenPropertyChecks {

  "Section" should "round trip derived JSON" in {
    forAll(SectionGen.sectionGen) { section =>
      Section.format.reads(Section.format.writes(section)) should beJsSuccess(section)
    }
  }

  it should "support various form of expansions for RevealingChoice" in {
    val section: Section = mkSection(
      List(
        mkFormComponent(
          "a",
          RevealingChoice(
            NonEmptyList.of(
              RevealingChoiceElement(
                toLocalisedString("Yes"),
                List(
                  mkFormComponent("b", Value),
                  mkFormComponent("c", Value)
                ),
                true
              ),
              RevealingChoiceElement(
                toLocalisedString("No"),
                List(
                  mkFormComponent("d", Value),
                  mkFormComponent("e", Value)
                ),
                false
              )
            )
          )
        ),
        mkFormComponent("f", Value)
      )
    )

    val all = List("a", "b", "c", "d", "e", "f")

    val dataAndExpectations = Table(
      // format: off
      ("input", "full", "data", "rc"),
      (mkData(),                      all, all, List("a", "f")),
      (mkData("a" -> "not_a_number"), all, all, List("a", "f")),
      (mkData("a" -> "0"),            all, all, List("a", "b", "c", "f")),
      (mkData("a" -> "1"),            all, all, List("a", "d", "e", "f"))
      // format: on
    )

    verifyTable(dataAndExpectations, section)

  }
  it should "support various form of expansions for Group" in {
    val section: Section = mkSection(
      List(
        mkFormComponent("group", mkGroup(3, List(mkFormComponent("a", Value), mkFormComponent("b", Value)))),
        mkFormComponent("c", Value)
      )
    )

    val all = List("a", "b", "1_a", "1_b", "2_a", "2_b", "c")

    val dataAndExpectations = Table(
      // format: off
      ("input", "full", "data", "rc"),
      (mkData(),                           all, List("a", "b", "c"), List("a", "b", "c")),
      (mkData("a"   -> "A", "b"   -> "B"), all, List("a", "b", "c"), List("a", "b", "c")),
      (mkData("a"   -> "A", "b"   -> "B",
              "1_a" -> "A", "1_b" -> "B"), all, List("a", "b", "1_a", "1_b", "c"), List("a", "b", "1_a", "1_b", "c")),
      (mkData("a"   -> "A", "b"   -> "B",
              "1_a" -> "A", "1_b" -> "B",
              "2_a" -> "A", "2_b" -> "B"), all, all, all)
      // format: on
    )

    verifyTable(dataAndExpectations, section)

  }

  private def verifyTable(
    dataAndExpectations: TableFor4[Data, List[String], List[String], List[String]],
    section: Section) = {

    def allIds(es: ExpandedSection): List[String] = es.allFCs.map(_.id.value)

    forAll(dataAndExpectations) { (data, expectedFull, expected, expectedRc) =>
      val expandedSectionRc: ExpandedSection = section.expandSectionRc(data)
      val expandedSection: ExpandedSection = section.expandSection(data)
      val expandedSectionFull: ExpandedSection = section.expandSectionFull

      allIds(expandedSectionFull) shouldBe expectedFull
      allIds(expandedSection) shouldBe expected
      allIds(expandedSectionRc) shouldBe expectedRc
    }
  }
}
