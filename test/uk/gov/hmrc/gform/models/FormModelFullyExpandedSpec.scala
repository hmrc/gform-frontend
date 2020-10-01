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
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
 * import uk.gov.hmrc.gform.graph.RecData
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormCtx
 * import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
 * import uk.gov.hmrc.gform.sharedmodel.VariadicValue.One
 * import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Basic, FormComponent, FormComponentId, GroupExpanded, Page, Section, Value }
 *
 * class FormModelFullyExpandedSpec extends FlatSpec with Matchers with RecalculatedFormDataSupport with FormModelSupport {
 *   "FormModel.expand" should "expand groups and repeatedSections in Section.NonRepeatingPage" in {
 *     val fcA = mkFormComponent("a", Value)
 *     val fcB = mkFormComponent("b", Value)
 *     val group: FormComponent = mkFormComponent("group", mkGroup(5, List(fcA, fcB)))
 *     val nonRepeatingPage: Section.NonRepeatingPage = mkSection(group)
 *     //val formTemplate = mkFormTemplate(List(nonRepeatingPage))
 *     //val basicFormModel = FormModel.basic(formTemplate)
 *     val data = FormDataRecalculated.empty
 *     val formModel = mkFormModel(List(nonRepeatingPage), data)
 *
 *     val expectedPage = mkPage(fcA :: fcB :: Nil)
 *
 *     val expected = FormModel(List(Singleton(expectedPage, nonRepeatingPage)))
 *
 *     formModel shouldBe expected
 *   }
 *
 *   it should "expand repeatedSections base on constant" in {
 *     val fcA = mkFormComponent("a", Value)
 *     val expectedFcA = mkFormComponent("1_a", Value)
 *     val repeatingPage: Section.RepeatingPage = mkRepeatingPageSection(fcA :: Nil)
 *     val formModel = mkFormModel(List(repeatingPage))
 *
 *     val expectedPage = mkPage(expectedFcA)
 *
 *     val expected = FormModel(List(Singleton(expectedPage, repeatingPage)))
 *
 *     formModel shouldBe expected
 *   }
 *
 *   it should "expand repeatedSections base on expr" in {
 *     val fcA = mkFormComponent("a", Value)
 *     val fcB = mkFormComponent("b", Value)
 *     val expectedFcB1 = mkFormComponent("1_b", Value)
 *     val expectedFcB2 = mkFormComponent("2_b", Value)
 *     val expectedFcB3 = mkFormComponent("3_b", Value)
 *     val nonRepeatingPage: Section.NonRepeatingPage = mkSection(fcA)
 *     val repeatingPage: Section.RepeatingPage = mkRepeatingPageSection(fcB :: Nil, FormCtx("a"))
 *
 *     val data = mkFormDataRecalculated(
 *       "a" -> "3"
 *     )
 *
 *     val formModel = mkFormModel(List(nonRepeatingPage, repeatingPage), data)
 *
 *     val expectedPageA = mkPage(fcA)
 *     val expectedPageB1 = mkPage(expectedFcB1)
 *     val expectedPageB2 = mkPage(expectedFcB2)
 *     val expectedPageB3 = mkPage(expectedFcB3)
 *
 *     val expected = FormModel(
 *       List(
 *         Singleton(expectedPageA, nonRepeatingPage),
 *         Singleton(expectedPageB1, repeatingPage),
 *         Singleton(expectedPageB2, repeatingPage),
 *         Singleton(expectedPageB3, repeatingPage)
 *       )
 *     )
 *
 *     formModel shouldBe expected
 *   }
 *
 *   private def mkPage(formComponent: FormComponent): Page[GroupExpanded] = mkPage(formComponent :: Nil)
 *
 *   private def mkPage(formComponents: List[FormComponent]): Page[GroupExpanded] = Page[GroupExpanded](
 *     toSmartString("Section Name"),
 *     None,
 *     None,
 *     None,
 *     None,
 *     None,
 *     formComponents,
 *     None,
 *     None
 *   )
 *
 * } */
