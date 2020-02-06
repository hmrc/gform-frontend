/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

/* import org.scalatest.{ FlatSpec, Matchers }
 * import org.scalatest.prop.TableDrivenPropertyChecks
 * import uk.gov.hmrc.gform.sharedmodel.form.VisitIndex
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 * import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
 *
 * class ProcessDataSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks with FormModelSupport {
 *
 *   "updateSectionVisits" should "reindex visits when repeated section shrinked" in {
 *
 *     val sections = List(
 *       mkSection(mkFormComponent("a", Value) :: Nil),
 *       mkSection(mkFormComponent("1_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("d", Value) :: Nil)
 *     )
 *
 *     val mongoSections = List(
 *       mkSection(mkFormComponent("a", Value) :: Nil),
 *       mkSection(mkFormComponent("1_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("2_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("d", Value) :: Nil)
 *     )
 *
 *     val visibilityIndices = Table(
 *       // format: off
 *       ("input",  "expected"),
 *       (Set(0),       Set(0)),
 *       (Set(0,1),     Set(0,1)),
 *       (Set(0,1,2),   Set(0,1)),
 *       (Set(0,1,2,3), Set(0,1,2)),
 *       (Set(1,2,3),   Set(1,2)),
 *       (Set(2,3),     Set(2)),
 *       (Set(3),       Set(2)),
 *       (Set(3,100),   Set(2))
 *       // format: on
 *     )
 *
 *     val formModel = mkFormModel(sections)
 *     val mongoFormModel = mkFormModel(mongoSections)
 *
 *     forAll(visibilityIndices) { (input, expectedOuput) ⇒
 *       val res = VisitIndex.updateSectionVisits(formModel, mongoFormModel, VisitIndex(input))
 *       res shouldBe expectedOuput
 *     }
 *   }
 *
 *   it should "reindex visits when repeated section grows" in {
 *
 *     val sections = List(
 *       mkSection(mkFormComponent("a", Value) :: Nil),
 *       mkSection(mkFormComponent("1_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("2_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("3_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("d", Value) :: Nil)
 *     )
 *
 *     val mongoSections = List(
 *       mkSection(mkFormComponent("a", Value) :: Nil),
 *       mkSection(mkFormComponent("1_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("2_repeated", Value) :: Nil),
 *       mkSection(mkFormComponent("d", Value) :: Nil)
 *     )
 *
 *     val visibilityIndices = Table(
 *       // format: off
 *       ("input",  "expected"),
 *       (Set(0),       Set(0)),
 *       (Set(0,1),     Set(0,1)),
 *       (Set(0,1,2),   Set(0,1,2)),
 *       (Set(0,1,2,3), Set(0,1,2,4)),
 *       (Set(1,2,3),   Set(1,2,4)),
 *       (Set(2,3),     Set(2,4)),
 *       (Set(3),       Set(4))
 *       // format: on
 *     )
 *
 *     val formModel = mkFormModel(sections)
 *     val mongoFormModel = mkFormModel(mongoSections)
 *
 *     forAll(visibilityIndices) { (input, expectedOuput) ⇒
 *       val res = VisitIndex.updateSectionVisits(formModel, mongoFormModel, VisitIndex(input))
 *       res shouldBe expectedOuput
 *     }
 *   }
 * } */
