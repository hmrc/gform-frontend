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
 * import uk.gov.hmrc.gform.sharedmodel.{ VariadicFormData, VariadicValue }
 * import uk.gov.hmrc.gform.sharedmodel.VariadicValue.One
 * import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Basic, FormComponent, FormComponentId, GroupExpanded, Page, Section, Value }
 *
 * class FormModelGroupExpandedSpec extends FlatSpec with Matchers with RecalculatedFormDataSupport with FormModelSupport {
 *   "FormModel.expandGroup" should "expand groups in Section.NonRepeatingPage" in {
 *     val fcA = mkFormComponent("a", Value)
 *     val fcB = mkFormComponent("b", Value)
 *     val group: FormComponent = mkFormComponent("group", mkGroup(5, List(fcA, fcB)))
 *     val nonRepeatingPage: Section.NonRepeatingPage = mkSection(group)
 *     val formModel = mkFormModelExpandGroups(List(nonRepeatingPage))
 *
 *     val expectedPage = Page[GroupExpanded](
 *       toSmartString("Section Name"),
 *       None,
 *       None,
 *       None,
 *       None,
 *       None,
 *       fcA :: fcB :: Nil,
 *       None,
 *       None
 *     )
 *
 *     val expected = FormModel(List(Singleton(expectedPage, nonRepeatingPage)))
 *
 *     formModel shouldBe expected
 *   }
 *
 *   it should "expand groups in Section.NonRepeatingPage based on data" in {
 *     val fcA = mkFormComponent("a", Value)
 *     val fcB = mkFormComponent("b", Value)
 *
 *     val group: FormComponent = mkFormComponent("group", mkGroup(5, List(fcA, fcB)))
 *     val nonRepeatingPage: Section.NonRepeatingPage = mkSection(group)
 *
 *     val data = mkFormDataRecalculated(
 *       "a"         -> "1_A",
 *       "b"         -> "1_B",
 *       "1_a"       -> "2_A",
 *       "1_b"       -> "2_B",
 *       "2_a"       -> "3_A",
 *       "2_b"       -> "3_B",
 *       "unrelated" -> "UNRELATED"
 *     )
 *
 *     val formModel = mkFormModelExpandGroups(List(nonRepeatingPage), data)
 *     val expectedFCs = ("1_a" :: "1_b" :: "2_a" :: "2_b" :: Nil).map(mkFormComponent(_, Value))
 *
 *     val expectedPage = Page[GroupExpanded](
 *       toSmartString("Section Name"),
 *       None,
 *       None,
 *       None,
 *       None,
 *       None,
 *       fcA :: fcB :: expectedFCs,
 *       None,
 *       None
 *     )
 *
 *     val expected = FormModel(List(Singleton(expectedPage, nonRepeatingPage)))
 *
 *     formModel shouldBe expected
 *   }
 *
 *   // TODO JoVl - do we want to show group expanding in RepeatingPage and AddToList?
 *
 *   /\* it should "expand Section.RepeatingPage" in {
 *  *   val repeatingPage: Section.RepeatingPage = mkRepeatingPageSection(List.empty[FormComponent])
 *  *   val formTemplate = mkFormTemplate(List(repeatingPage))
 *  *   val formModel = FormModel.basic(formTemplate)
 *  *
 *  *   val expected = FormModel(List(Singleton(repeatingPage.page, repeatingPage)))
 *  *
 *  *   formModel shouldBe expected
 *  * }
 *  *
 *  * it should "expand Section.AddToList with one list-filler page" in {
 *  *   val addToList: Section.AddToList = mkAddToListSection(List.empty[FormComponent])
 *  *   val formTemplate = mkFormTemplate(List(addToList))
 *  *   val formModel = FormModel.basic(formTemplate)
 *  *
 *  *   val expected = FormModel[Basic](
 *  *     List(
 *  *       Singleton(addToList.pages.toList.head, addToList),
 *  *       Repeater(List.empty[AddToListRecord], toSmartString("Pet owner"), None, None, addToList)
 *  *     )
 *  *   )
 *  *
 *  *   formModel shouldBe expected
 *  * }
 *  * it should "expand Section.AddToList with two list-filler pages" in {
 *  *   val addToList: Section.AddToList = mkAddToListSection(List.empty[FormComponent], List.empty[FormComponent])
 *  *   val formTemplate = mkFormTemplate(List(addToList))
 *  *   val formModel = FormModel.basic(formTemplate)
 *  *
 *  *   val expected = FormModel[Basic](
 *  *     List(
 *  *       Singleton(addToList.pages.toList(0), addToList),
 *  *       Singleton(addToList.pages.toList(1), addToList),
 *  *       Repeater(List.empty[AddToListRecord], toSmartString("Pet owner"), None, None, addToList)
 *  *     )
 *  *   )
 *  *
 *  *   formModel shouldBe expected
 *  * }
 *  *
 *  * it should "expand Section.AddToList with three list-filler pages" in {
 *  *   val addToList: Section.AddToList =
 *  *     mkAddToListSection(List.empty[FormComponent], List.empty[FormComponent], List.empty[FormComponent])
 *  *   val formTemplate = mkFormTemplate(List(addToList))
 *  *   val formModel = FormModel.basic(formTemplate)
 *  *
 *  *   val expected = FormModel[Basic](
 *  *     List(
 *  *       Singleton(addToList.pages.toList(0), addToList),
 *  *       Singleton(addToList.pages.toList(1), addToList),
 *  *       Singleton(addToList.pages.toList(2), addToList),
 *  *       Repeater(List.empty[AddToListRecord], toSmartString("Pet owner"), None, None, addToList)
 *  *     )
 *  *   )
 *  *
 *  *   formModel shouldBe expected
 *  * } *\/
 *
 * } */
