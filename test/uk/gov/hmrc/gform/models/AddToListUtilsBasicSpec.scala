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

import org.scalatest.{ FlatSpec, Inside, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BasicText, FormComponent, FormComponentId, Section, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, Obligations }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormModelOptics, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AddToListId

/* class AddToListUtilsBacisSpec
 *     extends FlatSpec with Matchers with Inside with RecalculatedFormDataSupport with FormModelSupport {
 *   "AddToListUtils.removeRecord" should "remove instance by its index" in {
 *
 *     val data: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_a"       -> One("a_2"),
 *       "2_b"       -> One("b_2")
 *     )
 *
 *     val expectedData1: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular" -> One("r"),
 *       "1_a"     -> One("a_2"),
 *       "1_b"     -> One("b_2")
 *     )
 *
 *     val expectedData2: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular" -> One("r"),
 *       "1_a"     -> One("a_1"),
 *       "1_b"     -> One("b_1")
 *     )
 *
 *     val dataB: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_a"       -> One("a_2"),
 *       "2_b"       -> One("b_2"),
 *       "2_ownerFc" -> Many("0" :: Nil),
 *       "3_a"       -> One("a_3"),
 *       "3_b"       -> One("b_3")
 *     )
 *
 *     val expectedDataB1: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_2"),
 *       "1_b"       -> One("b_2"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_a"       -> One("a_3"),
 *       "2_b"       -> One("b_3")
 *     )
 *
 *     val expectedDataB2: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_a"       -> One("a_3"),
 *       "2_b"       -> One("b_3")
 *     )
 *
 *     val expectedDataB3: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_a"       -> One("a_2"),
 *       "2_b"       -> One("b_2")
 *       //"2_ownerFc" -> Many("0" :: Nil)
 *     )
 *
 *     val addToList = mkAddToListSection(
 *       List(
 *         mkFormComponent("a", Text(BasicText, Value))
 *       ),
 *       List(
 *         mkFormComponent("b", Text(BasicText, Value))
 *       )
 *     )
 *
 *     val sections: List[Section] =
 *       mkSection(mkFormComponent("regular", Text(BasicText, Value)) :: Nil) :: addToList :: Nil
 *
 *     val visitsIndex: VisitIndex = VisitIndex(Set.empty[Int])
 *
 *     val obligations: Obligations = NotChecked
 *
 *     val addToListId: AddToListId = addToList.id
 *
 *     val variations = Table(
 *       ("index", "data", "expected"),
 *       (1, data, expectedData1),
 *       (2, data, expectedData2),
 *       (1, dataB, expectedDataB1),
 *       (2, dataB, expectedDataB2),
 *       (3, dataB, expectedDataB3)
 *     )
 *
 *     forAll(variations) { (index, startingData, expectedData) ⇒
 *       val formModel: FormModel[FullyExpanded] = mkFormModel(sections, startingData)
 *       val processData: ProcessData = ProcessData(startingData, formModel, visitsIndex, obligations)
 *       val res = AddToListUtils.removeRecord(processData, index, addToListId)
 *
 *       res shouldBe expectedData.data
 *     }
 *
 *   }
 * } */
