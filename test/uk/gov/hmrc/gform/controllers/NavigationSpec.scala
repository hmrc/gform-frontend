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

package uk.gov.hmrc.gform.controllers

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class NavitagionSpec extends Spec {

  def mkFormComponent(formComponentId: FormComponentId): FormComponent = FormComponent(
    id = formComponentId,
    `type` = Text(AnyText, Constant("")),
    label = "",
    helpText = None,
    shortName = None,
    validIf = None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = false,
    errorMessage = None,
    presentationHint = None
  )

  def makeSection(title: String, formComponent: FormComponent, includeIf: Option[IncludeIf] = None): Section = Section(
    title = title,
    description = None,
    shortName = None,
    includeIf = includeIf,
    repeatsMax = None,
    repeatsMin = None,
    validators = None,
    fields = formComponent :: Nil,
    continueLabel = None
  )

  def getAvailableSectionNumbers(sectionsData: List[Section], formData: Map[FormComponentId, String]) =
    new Navigation {
      val sections: List[Section] = sectionsData
      val data: Map[FormComponentId, Seq[String]] = formData.mapValues(_ :: Nil)
      val retrievals = authContext
    }.availableSectionNumbers

  def dependsOn(fcId: FormComponentId): Option[IncludeIf] = Some(IncludeIf(Equals(FormCtx(fcId.value), Constant("0"))))

  val fcId1 = FormComponentId("fcId1")
  val fcId2 = FormComponentId("fcId2")
  val fcId3 = FormComponentId("fcId3")
  val fcId4 = FormComponentId("fcId4")
  val fcId5 = FormComponentId("fcId5")

  val section1 = makeSection("Page 1", mkFormComponent(fcId1))
  val section2 = makeSection("Page 2", mkFormComponent(fcId2), dependsOn(fcId1))
  val section3 = makeSection("Page 3", mkFormComponent(fcId3), dependsOn(fcId2))
  val section4 = makeSection("Page 4", mkFormComponent(fcId4), dependsOn(fcId3))
  val section5 = makeSection("Page 5", mkFormComponent(fcId5))

  val sections = section1 :: section2 :: section3 :: section4 :: section5 :: Nil

  "Single page form" should "have section number 0 visible" in {
    val single = FormComponentId("single")
    val singleSection = makeSection("Single Page", mkFormComponent(single))
    val result = getAvailableSectionNumbers(singleSection :: Nil, Map.empty)

    result shouldBe List(SectionNumber(0))
  }

  "Chain of section" should "hide all dependent section in the chain" in {
    val result1 = getAvailableSectionNumbers(sections, Map(fcId1 -> "0", fcId2 -> "0", fcId3 -> "0"))
    val result2 = getAvailableSectionNumbers(sections, Map(fcId1 -> "0", fcId2 -> "0", fcId3 -> "1"))
    val result3 = getAvailableSectionNumbers(sections, Map(fcId1 -> "0", fcId2 -> "1", fcId3 -> "0"))
    val result4 = getAvailableSectionNumbers(sections, Map(fcId1 -> "1", fcId2 -> "0", fcId3 -> "0"))

    result1 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(2), SectionNumber(3), SectionNumber(4))
    result2 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(2), SectionNumber(4))
    result3 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(4))
    result4 shouldBe List(SectionNumber(0), SectionNumber(4))
  }

}
