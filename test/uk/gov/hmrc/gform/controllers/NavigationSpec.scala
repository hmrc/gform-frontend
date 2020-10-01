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

package uk.gov.hmrc.gform.controllers

/* import cats.instances.option._
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.graph.{ GraphException, Recalculation }
 * import uk.gov.hmrc.gform.models.{ FormModel, FormModelSupport }
 * import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, SmartString, VariadicFormData, VariadicValue }
 * import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
 * import uk.gov.hmrc.gform.{ GraphSpec, Spec }
 * import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
 * import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate._
 * import uk.gov.hmrc.http.HeaderCarrier
 *
 * class NavitagionSpec extends Spec with GraphSpec with FormModelSupport {
 *
 *   implicit val hc: HeaderCarrier = HeaderCarrier()
 *
 *   val recalculation: Recalculation[Option, Unit] =
 *     new Recalculation[Option, Unit](booleanExprEval, ((s: GraphException) => ()))
 *
 *   def mkFormComponent(formComponentId: FormComponentId): FormComponent = FormComponent(
 *     id = formComponentId,
 *     `type` = Text(BasicText, Value),
 *     label = toSmartString(""),
 *     helpText = None,
 *     shortName = None,
 *     validIf = None,
 *     mandatory = true,
 *     editable = true,
 *     submissible = true,
 *     derived = false,
 *     errorMessage = None,
 *     presentationHint = None
 *   )
 *
 *   def makeSection(title: SmartString, formComponent: FormComponent, includeIf: Option[IncludeIf] = None): Section =
 *     Section.NonRepeatingPage(
 *       Page(
 *         title = title,
 *         description = None,
 *         shortName = None,
 *         includeIf = includeIf,
 *         validators = None,
 *         fields = formComponent :: Nil,
 *         continueLabel = None,
 *         continueIf = None
 *       ))
 *
 *   def getAvailableSectionNumbers(sectionsData: List[Section], formData: VariadicFormData) = {
 *     val res = recalculation.recalculateFormData(
 *       formData,
 *       mkFormTemplate(sectionsData),
 *       ExampleData.authContext,
 *       ThirdPartyData.empty,
 *       EnvelopeId("")
 *     )
 *     new Navigation {
 *       val formModel: FormModel[FullyExpanded] = mkFormModel(sectionsData)
 *       val data: FormDataRecalculated = res.get
 *     }.availableSectionNumbers
 *   }
 *
 *   def dependsOn(fcId: FormComponentId): Option[IncludeIf] = Some(IncludeIf(Equals(FormCtx(fcId.value), Constant("1"))))
 *
 *   val fcId1 = FormComponentId("fcId1")
 *   val fcId2 = FormComponentId("fcId2")
 *   val fcId3 = FormComponentId("fcId3")
 *   val fcId4 = FormComponentId("fcId4")
 *   val fcId5 = FormComponentId("fcId5")
 *
 *   val section1 = makeSection(toSmartString("Page 1"), mkFormComponent(fcId1))
 *   val section2 = makeSection(toSmartString("Page 2"), mkFormComponent(fcId2), dependsOn(fcId1))
 *   val section3 = makeSection(toSmartString("Page 3"), mkFormComponent(fcId3), dependsOn(fcId2))
 *   val section4 = makeSection(toSmartString("Page 4"), mkFormComponent(fcId4), dependsOn(fcId3))
 *   val section5 = makeSection(toSmartString("Page 5"), mkFormComponent(fcId5))
 *
 *   val sections = section1 :: section2 :: section3 :: section4 :: section5 :: Nil
 *
 *   "Single page form" should "have section number 0 visible" in {
 *     val single = FormComponentId("single")
 *     val singleSection = makeSection(toSmartString("Single Page"), mkFormComponent(single))
 *     val result = getAvailableSectionNumbers(singleSection :: Nil, variadic())
 *
 *     result shouldBe List(SectionNumber(0))
 *   }
 *
 *   "Chain of section" should "hide all dependent section in the chain" in {
 *     val result1 = getAvailableSectionNumbers(sections, variadic(fcId1 -> "1", fcId2 -> "1", fcId3 -> "1"))
 *     val result2 = getAvailableSectionNumbers(sections, variadic(fcId1 -> "1", fcId2 -> "1", fcId3 -> "2"))
 *     val result3 = getAvailableSectionNumbers(sections, variadic(fcId1 -> "1", fcId2 -> "2", fcId3 -> "1"))
 *     val result4 = getAvailableSectionNumbers(sections, variadic(fcId1 -> "2", fcId2 -> "1", fcId3 -> "1"))
 *
 *     result1 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(2), SectionNumber(3), SectionNumber(4))
 *     result2 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(2), SectionNumber(4))
 *     result3 shouldBe List(SectionNumber(0), SectionNumber(1), SectionNumber(4))
 *     result4 shouldBe List(SectionNumber(0), SectionNumber(4))
 *   }
 *
 *   private def variadic(data: (FormComponentId, String)*) =
 *     VariadicFormData(data.toMap.mapValues(VariadicValue.One(_)))
 * } */
