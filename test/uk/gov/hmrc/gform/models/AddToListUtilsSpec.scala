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

/* import org.scalatest.{ FlatSpec, Inside, Matchers }
 * import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
 * import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
 * import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BasicText, FormComponent, FormComponentId, Section, Text, Value }
 * import uk.gov.hmrc.gform.sharedmodel.{ NotChecked, Obligations }
 * import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, VisitIndex }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, FullyExpanded }
 *
 * class AddToListUtilsSpec
 *     extends FlatSpec with Matchers with Inside with RecalculatedFormDataSupport with FormModelSupport {
 *   "AddToListUtils.removeRecord" should "remove instance by its index" in {
 *     val data: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_c"       -> One("c_1"),
 *       "1_e"       -> One("d_1"),
 *       "1_c"       -> One("e_1"),
 *       "2_a"       -> One("a_2"),
 *       "2_b"       -> One("b_2"),
 *       "2_c"       -> One("c_2"),
 *       "2_e"       -> One("d_2"),
 *       "2_c"       -> One("e_2"),
 *       "3_a"       -> One("a_3"),
 *       "3_b"       -> One("b_3"),
 *       "3_c"       -> One("c_3"),
 *       "3_e"       -> One("d_3"),
 *       "3_c"       -> One("e_3"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_ownerFc" -> Many("0" :: Nil)
 *     )
 *
 *     val expectedData1: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_2"),
 *       "1_b"       -> One("b_2"),
 *       "1_c"       -> One("c_2"),
 *       "1_e"       -> One("d_2"),
 *       "1_c"       -> One("e_2"),
 *       "2_a"       -> One("a_3"),
 *       "2_b"       -> One("b_3"),
 *       "2_c"       -> One("c_3"),
 *       "2_e"       -> One("d_3"),
 *       "2_c"       -> One("e_3"),
 *       "1_ownerFc" -> Many("0" :: Nil)
 *     )
 *
 *     val expectedData2: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_c"       -> One("c_1"),
 *       "1_e"       -> One("d_1"),
 *       "1_c"       -> One("e_1"),
 *       "2_a"       -> One("a_3"),
 *       "2_b"       -> One("b_3"),
 *       "2_c"       -> One("c_3"),
 *       "2_e"       -> One("d_3"),
 *       "2_c"       -> One("e_3"),
 *       "1_ownerFc" -> Many("0" :: Nil)
 *     )
 *
 *     val expectedData3: FormDataRecalculated = mkVariadicFormDataRecalculated(
 *       "regular"   -> One("r"),
 *       "1_a"       -> One("a_1"),
 *       "1_b"       -> One("b_1"),
 *       "1_c"       -> One("c_1"),
 *       "1_e"       -> One("d_1"),
 *       "1_c"       -> One("e_1"),
 *       "2_a"       -> One("a_2"),
 *       "2_b"       -> One("b_2"),
 *       "2_c"       -> One("c_2"),
 *       "2_e"       -> One("d_2"),
 *       "2_c"       -> One("e_2"),
 *       "1_ownerFc" -> Many("0" :: Nil),
 *       "2_ownerFc" -> Many("0" :: Nil)
 *     )
 *
 *     val sections: List[Section] =
 *       mkSection(mkFormComponent("regular", Text(BasicText, Value)) :: Nil) ::
 *         mkAddToListSection(
 *         List(
 *           mkFormComponent("a", Text(BasicText, Value)),
 *           mkFormComponent("b", Text(BasicText, Value)),
 *           mkFormComponent("c", Text(BasicText, Value))
 *         ),
 *         List(
 *           mkFormComponent("d", Text(BasicText, Value)),
 *           mkFormComponent("e", Text(BasicText, Value))
 *         )
 *       ) :: Nil
 *
 *     val formModel: FormModel[FullyExpanded] = mkFormModel(sections, data)
 *     //val expectedFormModel: FormModel[FullyExpanded] = mkFormModel(sections, expectedData)
 *     //formModel.pages.foreach(println)
 *     val visitsIndex: VisitIndex = VisitIndex(Set.empty[Int])
 *     val expectedVisitsIndex: VisitIndex = VisitIndex(Set.empty[Int]) // TODO JoVl visitindex needs to be updated
 *
 *     val obligations: Obligations = NotChecked
 *     val expectedObligations: Obligations = NotChecked
 *     val processData: ProcessData = ProcessData(data, formModel, visitsIndex, obligations)
 *
 *     //val idx: Int = 1
 *     val addToListId: AddToListId = AddToListId("owner")
 *
 *     /\* val res = AddToListUtils.removeRecord(processData, idx, addToListId)
 *      *
 *      * println("res: ")
 *      * res._1.data.foreach(println)
 *      * println("expectedData.data: ")
 *      * expectedData.data.data.foreach(println)
 *      *
 *      * res._1 shouldBe expectedData.data
 *      * res._2 shouldBe expectedVisitsIndex *\/
 *
 *     val variations = Table(
 *       ("index", "expected"),
 *       (1, expectedData1),
 *       (2, expectedData2),
 *       (3, expectedData3)
 *     )
 *
 *     forAll(variations) { (index, expected) ⇒
 *       val res = AddToListUtils.removeRecord(processData, index, addToListId)
 *
 *       res shouldBe expected.data
 *     }
 *
 *     /\* inside(res) {
 *    *   case ProcessData(data, formModel, visitsIndex, obligations) =>
 *    *     data shouldBe expectedData
 *    *     formModel shouldBe expectedFormModel
 *    *     visitsIndex shouldBe expectedVisitsIndex
 *    *     obligations shouldBe expectedObligations
 *    * } *\/
 *   }
 * } */
