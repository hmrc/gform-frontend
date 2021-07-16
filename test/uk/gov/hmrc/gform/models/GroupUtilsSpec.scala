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

import org.scalatest.prop.TableDrivenPropertyChecks.{ Table, forAll }
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, FormModelOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileUpload, FormComponentId, Section, SectionNumber, ShortText, Text, Value }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.VariadicValue.One
import uk.gov.hmrc.gform.sharedmodel.form.FormComponentIdToFileIdMapping

class GroupUtilsSpec extends AnyFlatSpecLike with Matchers with FormModelSupport with VariadicFormDataSupport {

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  "GroupUtils.removeRecord" should "remove instance by its index" in {
    val data: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular" -> One("r"),
      "1_a"     -> One("a_1"),
      "1_f"     -> One("f_1"),
      "2_a"     -> One("a_2"),
      "2_f"     -> One("f_2"),
      "3_a"     -> One("a_3"),
      "3_f"     -> One("f_3"),
      "1_group" -> One(""),
      "2_group" -> One(""),
      "3_group" -> One("")
    )

    val expectedData1: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular" -> One("r"),
      "1_a"     -> One("a_2"),
      "1_f"     -> One("f_2"),
      "2_a"     -> One("a_3"),
      "2_f"     -> One("f_3"),
      "1_group" -> One(""),
      "2_group" -> One("")
    )

    val expectedData2: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular" -> One("r"),
      "1_a"     -> One("a_1"),
      "1_f"     -> One("f_1"),
      "2_a"     -> One("a_3"),
      "2_f"     -> One("f_3"),
      "1_group" -> One(""),
      "2_group" -> One("")
    )

    val expectedData3: VariadicFormData[SourceOrigin.OutOfDate] = mkVariadicFormData(
      "regular" -> One("r"),
      "1_a"     -> One("a_1"),
      "1_f"     -> One("f_1"),
      "2_a"     -> One("a_2"),
      "2_f"     -> One("f_2"),
      "1_group" -> One(""),
      "2_group" -> One("")
    )

    val sections: List[Section] =
      mkSection(
        mkFormComponent("regular", Text(ShortText.default, Value)) ::
          mkFormComponent("regularFile", FileUpload()) :: Nil
      ) ::
        mkSection(
          mkFormComponent(
            "group",
            mkGroup(
              5,
              List(
                mkFormComponent("a", Text(ShortText.default, Value)),
                mkFormComponent("f", FileUpload())
              )
            )
          ) :: Nil
        ) :: Nil

    val formModelOptics: FormModelOptics[DataOrigin.Browser] = mkFormModelOptics(mkFormTemplate(sections), data)

    val processData: ProcessData = mkProcessData(formModelOptics)

    val fileIds = Set("1_f", "2_f", "3_f", "regularFile").map(fcId => FileId(fcId))
    val originalMapping = FormComponentIdToFileIdMapping(fileIds.map(fileId => fileId.toFieldId -> fileId).toMap)

    val expectedMapping1 = Map("1_f" -> "2_f", "2_f" -> "3_f", "regularFile" -> "regularFile").toMapping
    val expectedMapping2 = Map("1_f" -> "1_f", "2_f" -> "3_f", "regularFile" -> "regularFile").toMapping
    val expectedMapping3 = Map("1_f" -> "1_f", "2_f" -> "2_f", "regularFile" -> "regularFile").toMapping

    val variations = Table(
      ("index", "expected"),
      (1, (expectedData1, expectedMapping1, Set(FileId("1_f")))),
      (2, (expectedData2, expectedMapping2, Set(FileId("2_f")))),
      (3, (expectedData3, expectedMapping3, Set(FileId("3_f"))))
    )

    forAll(variations) { case (index, (expectedVariadicData, expectedMapping, expectedFilesToDelete)) ⇒
      val modelComponentId: ModelComponentId = FormComponentId(index + "_group").modelComponentId
      val (updatedVariadicData, updatedMapping, filesToDelete) =
        GroupUtils.removeRecord(processData, modelComponentId, SectionNumber(1), originalMapping)

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
