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

import org.scalatest.{ FlatSpec, Matchers }
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, FormComponentId, Section, ShortText, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ SourceOrigin, VariadicFormData }

class AddToListUtilsSpec extends FlatSpec with Matchers with FormModelSupport with VariadicFormDataSupport {
  "AddToListUtils.removeRecord" should "remove instance by its index" in {
    val data: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular" -> One("r"),
      // Owner data
      "1_a"     -> One("a_1"),
      "1_b"     -> One("b_1"),
      "1_c"     -> One("c_1"),
      "1_d"     -> One("d_1"),
      "1_e"     -> One("e_1"),
      "2_a"     -> One("a_2"),
      "2_b"     -> One("b_2"),
      "2_c"     -> One("c_2"),
      "2_d"     -> One("d_2"),
      "2_e"     -> One("e_2"),
      "3_a"     -> One("a_3"),
      "3_b"     -> One("b_3"),
      "3_c"     -> One("c_3"),
      "3_d"     -> One("d_3"),
      "3_e"     -> One("e_3"),
      "1_owner" -> Many("0" :: Nil),
      "2_owner" -> Many("0" :: Nil),
      "3_owner" -> Many("1" :: Nil),
      // Fruit data
      "1_fruitA" -> One("apple_1"),
      "1_fruitB" -> One("banana_1"),
      "2_fruitA" -> One("apple_2"),
      "2_fruitB" -> One("banana_2"),
      "1_fruit"  -> Many("0" :: Nil)
    )

    val expectedData1: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular"  -> One("r"),
      "1_a"      -> One("a_2"),
      "1_b"      -> One("b_2"),
      "1_c"      -> One("c_2"),
      "1_d"      -> One("d_2"),
      "1_e"      -> One("e_2"),
      "2_a"      -> One("a_3"),
      "2_b"      -> One("b_3"),
      "2_c"      -> One("c_3"),
      "2_d"      -> One("d_3"),
      "2_e"      -> One("e_3"),
      "1_owner"  -> Many("0" :: Nil),
      "1_fruitA" -> One("apple_1"),
      "1_fruitB" -> One("banana_1"),
      "2_fruitA" -> One("apple_2"),
      "2_fruitB" -> One("banana_2"),
      "1_fruit"  -> Many("0" :: Nil)
    )

    val expectedData2: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular"  -> One("r"),
      "1_a"      -> One("a_1"),
      "1_b"      -> One("b_1"),
      "1_c"      -> One("c_1"),
      "1_d"      -> One("d_1"),
      "1_e"      -> One("e_1"),
      "2_a"      -> One("a_3"),
      "2_b"      -> One("b_3"),
      "2_c"      -> One("c_3"),
      "2_d"      -> One("d_3"),
      "2_e"      -> One("e_3"),
      "1_owner"  -> Many("0" :: Nil),
      "1_fruitA" -> One("apple_1"),
      "1_fruitB" -> One("banana_1"),
      "2_fruitA" -> One("apple_2"),
      "2_fruitB" -> One("banana_2"),
      "1_fruit"  -> Many("0" :: Nil)
    )

    val expectedData3: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular"  -> One("r"),
      "1_a"      -> One("a_1"),
      "1_b"      -> One("b_1"),
      "1_c"      -> One("c_1"),
      "1_d"      -> One("d_1"),
      "1_e"      -> One("e_1"),
      "2_a"      -> One("a_2"),
      "2_b"      -> One("b_2"),
      "2_c"      -> One("c_2"),
      "2_d"      -> One("d_2"),
      "2_e"      -> One("e_2"),
      "1_owner"  -> Many("0" :: Nil),
      "1_fruitA" -> One("apple_1"),
      "1_fruitB" -> One("banana_1"),
      "2_fruitA" -> One("apple_2"),
      "2_fruitB" -> One("banana_2"),
      "1_fruit"  -> Many("0" :: Nil)
    )

    val expectedData4: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular"  -> One("r"),
      "1_a"      -> One("a_1"),
      "1_b"      -> One("b_1"),
      "1_c"      -> One("c_1"),
      "1_d"      -> One("d_1"),
      "1_e"      -> One("e_1"),
      "2_a"      -> One("a_2"),
      "2_b"      -> One("b_2"),
      "2_c"      -> One("c_2"),
      "2_d"      -> One("d_2"),
      "2_e"      -> One("e_2"),
      "3_a"      -> One("a_3"),
      "3_b"      -> One("b_3"),
      "3_c"      -> One("c_3"),
      "3_d"      -> One("d_3"),
      "3_e"      -> One("e_3"),
      "1_owner"  -> Many("0" :: Nil),
      "2_owner"  -> Many("0" :: Nil),
      "3_owner"  -> Many("1" :: Nil),
      "1_fruitA" -> One("apple_2"),
      "1_fruitB" -> One("banana_2")
    )

    val expectedData5: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular"  -> One("r"),
      "1_a"      -> One("a_1"),
      "1_b"      -> One("b_1"),
      "1_c"      -> One("c_1"),
      "1_d"      -> One("d_1"),
      "1_e"      -> One("e_1"),
      "2_a"      -> One("a_2"),
      "2_b"      -> One("b_2"),
      "2_c"      -> One("c_2"),
      "2_d"      -> One("d_2"),
      "2_e"      -> One("e_2"),
      "3_a"      -> One("a_3"),
      "3_b"      -> One("b_3"),
      "3_c"      -> One("c_3"),
      "3_d"      -> One("d_3"),
      "3_e"      -> One("e_3"),
      "1_owner"  -> Many("0" :: Nil),
      "2_owner"  -> Many("0" :: Nil),
      "3_owner"  -> Many("1" :: Nil),
      "1_fruitA" -> One("apple_1"),
      "1_fruitB" -> One("banana_1")
    )

    val sections: List[Section] =
      mkSection(mkFormComponent("regular", Text(ShortText.default, Value)) :: Nil) ::
        mkAddToListSection(
        "owner",
        List(
          mkFormComponent("a", Text(ShortText.default, Value)),
          mkFormComponent("b", Text(ShortText.default, Value)),
          mkFormComponent("c", Text(ShortText.default, Value))
        ),
        List(
          mkFormComponent("d", Text(ShortText.default, Value)),
          mkFormComponent("e", Text(ShortText.default, Value))
        )
      ) :: mkAddToListSection(
        "fruit",
        List(
          mkFormComponent("fruitA", Text(ShortText.default, Value))
        ),
        List(
          mkFormComponent("fruitB", Text(ShortText.default, Value))
        )
      ) :: Nil

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(mkFormTemplate(sections), data)

    val processData: ProcessData = mkProcessData(formModelOptics)

    val ownerAddToListId: AddToListId = AddToListId(FormComponentId("owner"))
    val fruitAddToListId: AddToListId = AddToListId(FormComponentId("fruit"))

    val variations = Table(
      ("index", "addToListId", "expected"),
      (0, ownerAddToListId, expectedData1),
      (1, ownerAddToListId, expectedData2),
      (2, ownerAddToListId, expectedData3),
      (0, fruitAddToListId, expectedData4),
      (1, fruitAddToListId, expectedData5)
    )

    forAll(variations) { (index, addToListId, expected) ⇒
      val bracket: Bracket.AddToList[DataExpanded] =
        formModelOptics.formModelRenderPageOptics.formModel.brackets.addToListBracket(addToListId)
      val res = AddToListUtils.removeRecord(processData, bracket, index)

      res shouldBe expected
    }
  }
}
