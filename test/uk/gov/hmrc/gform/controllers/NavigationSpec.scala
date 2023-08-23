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

package uk.gov.hmrc.gform.controllers

import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.{ SmartString, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.One

import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.models.FormModelSupport
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.SectionSelectorType
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.FormModel
import uk.gov.hmrc.gform.models.Visibility
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SourceOrigin
import uk.gov.hmrc.gform.models.VariadicFormDataSupport
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import SectionNumber.Classic
class NavigationSpec extends Spec with FormModelSupport with VariadicFormDataSupport with GraphSpec {

  override val envelopeId: EnvelopeId = EnvelopeId("dummy")

  def mkFormComponent(formComponentId: FormComponentId): FormComponent = FormComponent(
    id = formComponentId,
    `type` = Text(ShortText(1, 20), Value),
    label = toSmartString(""),
    helpText = None,
    shortName = None,
    validIf = None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = false,
    errorMessage = None,
    presentationHint = None,
    includeIf = None
  )

  def makeSection(title: SmartString, formComponent: FormComponent, includeIf: Option[IncludeIf] = None): Section =
    Section.NonRepeatingPage(
      Page(
        id = None,
        title = title,
        description = None,
        shortName = None,
        includeIf = includeIf,
        validators = None,
        fields = formComponent :: Nil,
        continueLabel = None,
        continueIf = None,
        noPIITitle = None,
        caption = None,
        instruction = None,
        presentationHint = None,
        dataRetrieve = None,
        confirmation = None,
        redirects = None,
        hideSaveAndComeBackButton = None,
        removeItemIf = None
      )
    )

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = play.api.test.Helpers.stubMessages(play.api.test.Helpers.stubMessagesApi(Map.empty))
  def getFormModel(sectionsData: List[Section], formData: VariadicFormData[SourceOrigin.OutOfDate]) = {
    val formTemplate = mkFormTemplate(sectionsData)
    mkFormModelBuilder(formTemplate)
      .visibilityModel[DataOrigin.Browser, SectionSelectorType.Normal](formData, None)
      .formModel
  }
  def getNavigation(sectionsData: List[Section], formData: VariadicFormData[SourceOrigin.OutOfDate]) =
    new Navigation {
      override def formModel: FormModel[Visibility] = getFormModel(sectionsData, formData)
    }

  def getAvailableSectionNumbers(sectionsData: List[Section], formData: VariadicFormData[SourceOrigin.OutOfDate]) =
    getNavigation(sectionsData, formData).availableSectionNumbers

  def dependsOn(fcId: FormComponentId): Option[IncludeIf] = Some(IncludeIf(Equals(FormCtx(fcId), Constant("1"))))

  val fcId1 = FormComponentId("fcId1")
  val fcId2 = FormComponentId("fcId2")
  val fcId3 = FormComponentId("fcId3")
  val fcId4 = FormComponentId("fcId4")
  val fcId5 = FormComponentId("fcId5")
  val fcIdATL = FormComponentId("fcIdATL")

  val section1 = makeSection(toSmartString("Page 1"), mkFormComponent(fcId1))
  val section2 = makeSection(toSmartString("Page 2"), mkFormComponent(fcId2), dependsOn(fcId1))
  val section3 = makeSection(toSmartString("Page 3"), mkFormComponent(fcId3), dependsOn(fcId2))
  val section4 = makeSection(toSmartString("Page 4"), mkFormComponent(fcId4), dependsOn(fcId3))
  val section5 = makeSection(toSmartString("Page 5"), mkFormComponent(fcId5))

  val sections = section1 :: section2 :: section3 :: section4 :: section5 :: Nil

  val sectionATL = mkAddToListSection(
    "someQuestion",
    None,
    List(mkFormComponent(fcIdATL))
  )

  "Single page form" should "have section number 0 visible" in {
    val single = FormComponentId("single")
    val singleSection = makeSection(toSmartString("Single Page"), mkFormComponent(single))
    val result = getAvailableSectionNumbers(singleSection :: Nil, mkVariadicFormData())

    result shouldBe List(Classic(0))
  }

  "Chain of section" should "hide all dependent section in the chain" in {
    val result1 =
      getAvailableSectionNumbers(
        sections,
        mkVariadicFormData("fcId1" -> One("1"), "fcId2" -> One("1"), "fcId3" -> One("1"))
      )
    val result2 = getAvailableSectionNumbers(
      sections,
      mkVariadicFormData("fcId1" -> One("1"), "fcId2" -> One("1"), "fcId3" -> One("2"))
    )
    val result3 = getAvailableSectionNumbers(
      sections,
      mkVariadicFormData("fcId1" -> One("1"), "fcId2" -> One("2"), "fcId3" -> One("1"))
    )
    val result4 = getAvailableSectionNumbers(
      sections,
      mkVariadicFormData("fcId1" -> One("2"), "fcId2" -> One("1"), "fcId3" -> One("1"))
    )

    result1 shouldBe List(
      Classic(0),
      Classic(1),
      Classic(2),
      Classic(3),
      Classic(4)
    )
    result2 shouldBe List(
      Classic(0),
      Classic(1),
      Classic(2),
      Classic(4)
    )
    result3 shouldBe List(Classic(0), Classic(1), Classic(4))
    result4 shouldBe List(Classic(0), Classic(4))
  }

  "Navigator.nextSectionNumber" should "skip ATL non repeater section and jump to RepeaterSection" in {
    val formModel =
      getFormModel(
        section1 :: section2 :: sectionATL :: Nil,
        mkVariadicFormData("fcId1" -> One("1"), "fcId2" -> One("1"), "fcIdATL" -> One("1"))
      )

    val ffNavigator = Navigator(Classic(0), formModel)
    ffNavigator.availableSectionNumbers shouldBe List(Classic(0), Classic(1), Classic(2), Classic(3))
    ffNavigator.addToListRepeaterSectionNumbers shouldBe List(Classic(3))
    ffNavigator.addToListNonRepeaterSectionNumbers shouldBe List(Classic(2))
    ffNavigator.addToListSectionNumbers shouldBe List(Classic(2), Classic(3))
    Navigator(Classic(0), formModel).nextSectionNumber shouldBe (Classic(1))
    Navigator(Classic(1), formModel).nextSectionNumber shouldBe (Classic(3))
  }

}
