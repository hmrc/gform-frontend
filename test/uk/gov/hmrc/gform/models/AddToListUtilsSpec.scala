/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.eval.FileIdsWithMapping
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.{ Many, One }
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormComponentIdToFileIdMapping, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, FileUpload, FormComponentId, Section, ShortText, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin, VariadicFormData }

class AddToListUtilsSpec extends AnyFlatSpecLike with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  "AddToListUtils.removeRecord" should "remove instance by its index" in {
    val data: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular" -> One("r"),
      // Owner data
      "1_a"     -> One("a_1"),
      "1_b"     -> One("b_1"),
      "1_c"     -> One("c_1"),
      "1_d"     -> One("d_1"),
      "1_e"     -> One("e_1"),
      "1_f"     -> One("f_1"),
      "2_a"     -> One("a_2"),
      "2_b"     -> One("b_2"),
      "2_c"     -> One("c_2"),
      "2_d"     -> One("d_2"),
      "2_e"     -> One("e_2"),
      "2_f"     -> One("f_2"),
      "3_a"     -> One("a_3"),
      "3_b"     -> One("b_3"),
      "3_c"     -> One("c_3"),
      "3_d"     -> One("d_3"),
      "3_e"     -> One("e_3"),
      "3_f"     -> One("f_3"),
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
      "1_f"      -> One("f_2"),
      "2_a"      -> One("a_3"),
      "2_b"      -> One("b_3"),
      "2_c"      -> One("c_3"),
      "2_d"      -> One("d_3"),
      "2_e"      -> One("e_3"),
      "2_f"      -> One("f_3"),
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
      "1_f"      -> One("f_1"),
      "2_a"      -> One("a_3"),
      "2_b"      -> One("b_3"),
      "2_c"      -> One("c_3"),
      "2_d"      -> One("d_3"),
      "2_e"      -> One("e_3"),
      "2_f"      -> One("f_3"),
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
      "1_f"      -> One("f_1"),
      "2_a"      -> One("a_2"),
      "2_b"      -> One("b_2"),
      "2_c"      -> One("c_2"),
      "2_d"      -> One("d_2"),
      "2_e"      -> One("e_2"),
      "2_f"      -> One("f_2"),
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
      "1_f"      -> One("f_1"),
      "2_a"      -> One("a_2"),
      "2_b"      -> One("b_2"),
      "2_c"      -> One("c_2"),
      "2_d"      -> One("d_2"),
      "2_e"      -> One("e_2"),
      "2_f"      -> One("f_2"),
      "3_a"      -> One("a_3"),
      "3_b"      -> One("b_3"),
      "3_c"      -> One("c_3"),
      "3_d"      -> One("d_3"),
      "3_e"      -> One("e_3"),
      "3_f"      -> One("f_3"),
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
      "1_f"      -> One("f_1"),
      "2_a"      -> One("a_2"),
      "2_b"      -> One("b_2"),
      "2_c"      -> One("c_2"),
      "2_d"      -> One("d_2"),
      "2_e"      -> One("e_2"),
      "2_f"      -> One("f_2"),
      "3_a"      -> One("a_3"),
      "3_b"      -> One("b_3"),
      "3_c"      -> One("c_3"),
      "3_d"      -> One("d_3"),
      "3_e"      -> One("e_3"),
      "3_f"      -> One("f_3"),
      "1_owner"  -> Many("0" :: Nil),
      "2_owner"  -> Many("0" :: Nil),
      "3_owner"  -> Many("1" :: Nil),
      "1_fruitA" -> One("apple_1"),
      "1_fruitB" -> One("banana_1")
    )

    val sections: List[Section] =
      mkSection(
        mkFormComponent("regular", Text(ShortText.default, Value)) ::
          mkFormComponent("regularFile", FileUpload()) :: Nil
      ) ::
        mkAddToListSection(
          "owner",
          None,
          List(
            mkFormComponent("a", Text(ShortText.default, Value)),
            mkFormComponent("b", Text(ShortText.default, Value)),
            mkFormComponent("c", Text(ShortText.default, Value))
          ),
          List(
            mkFormComponent("d", Text(ShortText.default, Value)),
            mkFormComponent("e", Text(ShortText.default, Value)),
            mkFormComponent("f", FileUpload())
          )
        ) :: mkAddToListSection(
          "fruit",
          None,
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

    val fileIds = Set("1_f", "2_f", "3_f", "regularFile").map(fcId => FileId(fcId))

    val originalMapping = FileIdsWithMapping(
      fileIds.map(_.toFieldId.modelComponentId),
      FormComponentIdToFileIdMapping(fileIds.map(fileId => fileId.toFieldId -> fileId).toMap)
    )

    val expectedMapping1 = Map("1_f" -> "2_f", "2_f" -> "3_f", "regularFile" -> "regularFile").toMapping
    val expectedMapping2 = Map("1_f" -> "1_f", "2_f" -> "3_f", "regularFile" -> "regularFile").toMapping
    val expectedMapping3 = Map("1_f" -> "1_f", "2_f" -> "2_f", "regularFile" -> "regularFile").toMapping

    val variations = Table(
      ("index", "addToListId", "expected"),
      (0, ownerAddToListId, (expectedData1, expectedMapping1, Set(FileId("1_f")))),
      (1, ownerAddToListId, (expectedData2, expectedMapping2, Set(FileId("2_f")))),
      (2, ownerAddToListId, (expectedData3, expectedMapping3, Set(FileId("3_f")))),
      (0, fruitAddToListId, (expectedData4, originalMapping.mapping, Set.empty[FileId])),
      (1, fruitAddToListId, (expectedData5, originalMapping.mapping, Set.empty[FileId]))
    )

    forAll(variations) { case (index, addToListId, (expectedVariadicData, expectedMapping, expectedFilesToDelete)) ⇒
      val bracket: Bracket.AddToList[DataExpanded] =
        formModelOptics.formModelRenderPageOptics.formModel.brackets.addToListBracket(addToListId)
      val (updatedVariadicData, updatedMapping, filesToDelete) =
        AddToListUtils.removeRecord(processData, bracket, index, originalMapping)

      updatedMapping shouldBe expectedMapping
      filesToDelete shouldBe expectedFilesToDelete
      updatedVariadicData shouldBe expectedVariadicData

    }
  }

  implicit class MapOps(map: Map[String, String]) {
    def toMapping: FormComponentIdToFileIdMapping =
      FormComponentIdToFileIdMapping(map.map { case (k, v) => FormComponentId(k) -> FileId(v) })
  }
}
