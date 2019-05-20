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

package uk.gov.hmrc.gform.controllers

import cats.instances.option._
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier

class NavitagionSpec extends Spec with GraphSpec {

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val recalculation: Recalculation[Option, Unit] =
    new Recalculation[Option, Unit](booleanExprEval, ((s: GraphException) => ()))

  def mkFormComponent(formComponentId: FormComponentId): FormComponent = FormComponent(
    id = formComponentId,
    `type` = Text(AnyText, Value),
    label = toLocalisedString(""),
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

  def makeSection(title: LocalisedString, formComponent: FormComponent, includeIf: Option[IncludeIf] = None): Section =
    Section(
      title = title,
      description = None,
      shortName = None,
      includeIf = includeIf,
      repeatsMax = None,
      repeatsMin = None,
      validators = None,
      fields = formComponent :: Nil,
      continueLabel = None,
      continueIf = None
    )

  def getAvailableSectionNumbers(sectionsData: List[Section], formData: Map[FormComponentId, String]) = {
    val res = recalculation.recalculateFormData(
      formData.map { case (k, v) => k -> Seq(v) },
      mkFormTemplate(sectionsData),
      ExampleData.authContext,
      ThirdPartyData.empty,
      EnvelopeId("")
    )
    new Navigation {
      val sections: List[Section] = sectionsData
      val data: FormDataRecalculated = res.get
    }.availableSectionNumbers
  }

  def dependsOn(fcId: FormComponentId): Option[IncludeIf] = Some(IncludeIf(Equals(FormCtx(fcId.value), Constant("1"))))

  val fcId1 = FormComponentId("fcId1")
  val fcId2 = FormComponentId("fcId2")
  val fcId3 = FormComponentId("fcId3")
  val fcId4 = FormComponentId("fcId4")
  val fcId5 = FormComponentId("fcId5")

  val section1 = makeSection(toLocalisedString("Page 1"), mkFormComponent(fcId1))
  val section2 = makeSection(toLocalisedString("Page 2"), mkFormComponent(fcId2), dependsOn(fcId1))
  val section3 = makeSection(toLocalisedString("Page 3"), mkFormComponent(fcId3), dependsOn(fcId2))
  val section4 = makeSection(toLocalisedString("Page 4"), mkFormComponent(fcId4), dependsOn(fcId3))
  val section5 = makeSection(toLocalisedString("Page 5"), mkFormComponent(fcId5))

  val sections = section1 :: section2 :: section3 :: section4 :: section5 :: Nil

  "Single page form" should "have section number 0 visible" in {
    val single = FormComponentId("single")
    val singleSection = makeSection(toLocalisedString("Single Page"), mkFormComponent(single))
    val result = getAvailableSectionNumbers(singleSection :: Nil, Map.empty)

    result shouldBe List(SectionNumber(0))
  }

  "Chain of section" should "hide all dependent section in the chain" in {
    val result1 = getAvailableSectionNumbers(sections, Map(fcId1 -> "1", fcId2 -> "1", fcId3 -> "1"))
    val result2 = getAvailableSectionNumbers(sections, Map(fcId1 -> "1", fcId2 -> "1", fcId3 -> "2"))
    val result3 = getAvailableSectionNumbers(sections, Map(fcId1 -> "1", fcId2 -> "2", fcId3 -> "1"))
    val result4 = getAvailableSectionNumbers(sections, Map(fcId1 -> "2", fcId2 -> "1", fcId3 -> "1"))

    result1 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(2), SectionNumber(3), SectionNumber(4))
    result2 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(2), SectionNumber(4))
    result3 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(4))
    result4 shouldBe List(SectionNumber(0), SectionNumber(4))
  }

}
