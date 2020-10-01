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

import cats.data.NonEmptyList
import org.scalatest.{ FlatSpec, Matchers }
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

/* class FormModelBasicSpec extends FlatSpec with Matchers with FormModelSupport {
 *
 *   val expectedFc = addToListQuestion.copy(id = FormComponentId("1_ownerFc"))
 *
 *   "FormModel.basic" should "expand from formTemplate" in {
 *     val formModel = mkFormModelBasic(List.empty[Section])
 *
 *     formModel shouldBe FormModel[Basic](List.empty[PageModel[Basic]])
 *   }
 *
 *   it should "expand Section.NonRepeatingPage" in {
 *     val nonRepeatingPage: Section.NonRepeatingPage = mkSection(List.empty[FormComponent])
 *     val formModel = mkFormModelBasic(List(nonRepeatingPage))
 *
 *     val expected = FormModel(List(Singleton(nonRepeatingPage.page, nonRepeatingPage)))
 *
 *     formModel shouldBe expected
 *   }
 *
 *   it should "expand Section.RepeatingPage" in {
 *     val repeatingPage: Section.RepeatingPage = mkRepeatingPageSection(List.empty[FormComponent])
 *     val formModel = mkFormModelBasic(List(repeatingPage))
 *
 *     val expected = FormModel(List(Singleton(repeatingPage.page, repeatingPage)))
 *
 *     formModel shouldBe expected
 *   }
 *
 *   it should "expand Section.AddToList with one list-filler page" in {
 *     val addToList: Section.AddToList = mkAddToListSection(List.empty[FormComponent])
 *     val formModel = mkFormModelBasic(List(addToList))
 *
 *     val expected = FormModel[Basic](
 *       List(
 *         Singleton(addToList.pages.toList.head, addToList),
 *         Repeater(
 *           toSmartString("Pet owner"),
 *           Some(toSmartString("Pet owner description")),
 *           None,
 *           None,
 *           expectedFc,
 *           1,
 *           addToList)
 *       )
 *     )
 *     formModel shouldBe expected
 *   }
 *   it should "expand Section.AddToList with two list-filler pages" in {
 *     val addToList: Section.AddToList = mkAddToListSection(List.empty[FormComponent], List.empty[FormComponent])
 *     val formModel = mkFormModelBasic(List(addToList))
 *
 *     val expected = FormModel[Basic](
 *       List(
 *         Singleton(addToList.pages.toList(0), addToList),
 *         Singleton(addToList.pages.toList(1), addToList),
 *         Repeater(
 *           toSmartString("Pet owner"),
 *           Some(toSmartString("Pet owner description")),
 *           None,
 *           None,
 *           expectedFc,
 *           1,
 *           addToList)
 *       )
 *     )
 *
 *     formModel shouldBe expected
 *   }
 *
 *   it should "expand Section.AddToList with three list-filler pages" in {
 *     val addToList: Section.AddToList =
 *       mkAddToListSection(List.empty[FormComponent], List.empty[FormComponent], List.empty[FormComponent])
 *
 *     val formModel = mkFormModelBasic(List(addToList))
 *
 *     val expected = FormModel[Basic](
 *       List(
 *         Singleton(addToList.pages.toList(0), addToList),
 *         Singleton(addToList.pages.toList(1), addToList),
 *         Singleton(addToList.pages.toList(2), addToList),
 *         Repeater(
 *           toSmartString("Pet owner"),
 *           Some(toSmartString("Pet owner description")),
 *           None,
 *           None,
 *           expectedFc,
 *           1,
 *           addToList)
 *       )
 *     )
 *
 *     formModel shouldBe expected
 *   }
 *
 * } */
