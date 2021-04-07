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

package uk.gov.hmrc.gform.sharedmodel.form

import munit.FunSuite
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

class FormComponentIdToFileIdMappingSpec extends FunSuite {

  val table: List[(FormComponentIdToFileIdMapping, List[(FormComponentId, FileId)])] = List(
    (
      FormComponentIdToFileIdMapping(
        Map.empty[FormComponentId, FileId]
      ),
      List(
        FormComponentId("1_file") -> FileId("1_file"),
        FormComponentId("2_file") -> FileId("2_file"),
        FormComponentId("3_file") -> FileId("3_file"),
        FormComponentId("4_file") -> FileId("4_file"),
        FormComponentId("5_file") -> FileId("5_file"),
        FormComponentId("6_file") -> FileId("6_file")
      )
    ),
    (
      FormComponentIdToFileIdMapping(
        Map(
          FormComponentId("1_file") -> FileId("1_file"),
          FormComponentId("2_file") -> FileId("2_file")
        )
      ),
      List(
        FormComponentId("1_file") -> FileId("1_file"),
        FormComponentId("2_file") -> FileId("2_file"),
        FormComponentId("3_file") -> FileId("3_file"),
        FormComponentId("4_file") -> FileId("4_file"),
        FormComponentId("5_file") -> FileId("5_file"),
        FormComponentId("6_file") -> FileId("6_file")
      )
    ),
    (
      FormComponentIdToFileIdMapping(
        Map(
          FormComponentId("1_file") -> FileId("2_file"),
          FormComponentId("2_file") -> FileId("1_file")
        )
      ),
      List(
        FormComponentId("1_file") -> FileId("2_file"),
        FormComponentId("2_file") -> FileId("1_file"),
        FormComponentId("3_file") -> FileId("3_file"),
        FormComponentId("4_file") -> FileId("4_file"),
        FormComponentId("5_file") -> FileId("5_file"),
        FormComponentId("6_file") -> FileId("6_file")
      )
    ),
    (
      FormComponentIdToFileIdMapping(
        Map(
          FormComponentId("2_file") -> FileId("5_file"),
          FormComponentId("1_file") -> FileId("1_file")
        )
      ),
      List(
        FormComponentId("1_file") -> FileId("1_file"),
        FormComponentId("2_file") -> FileId("5_file"),
        FormComponentId("3_file") -> FileId("3_file"),
        FormComponentId("4_file") -> FileId("4_file"),
        FormComponentId("5_file") -> FileId("2_file"),
        FormComponentId("6_file") -> FileId("6_file")
      )
    ),
    (
      FormComponentIdToFileIdMapping(
        Map(
          FormComponentId("1_file") -> FileId("2_file"),
          FormComponentId("4_file") -> FileId("5_file")
        )
      ),
      List(
        FormComponentId("1_file") -> FileId("2_file"),
        FormComponentId("2_file") -> FileId("1_file"),
        FormComponentId("3_file") -> FileId("3_file"),
        FormComponentId("4_file") -> FileId("5_file"),
        FormComponentId("5_file") -> FileId("4_file"),
        FormComponentId("6_file") -> FileId("6_file")
      )
    ),
    (
      FormComponentIdToFileIdMapping(
        Map(
          FormComponentId("1_file") -> FileId("2_file"),
          FormComponentId("2_file") -> FileId("3_file"),
          FormComponentId("3_file") -> FileId("5_file")
        )
      ),
      List(
        FormComponentId("1_file") -> FileId("2_file"),
        FormComponentId("2_file") -> FileId("3_file"),
        FormComponentId("3_file") -> FileId("5_file"),
        FormComponentId("4_file") -> FileId("4_file"),
        FormComponentId("5_file") -> FileId("1_file"),
        FormComponentId("6_file") -> FileId("6_file")
      )
    )
  )

  table.zipWithIndex.foreach { case ((mapping, expected), rowIndex) =>
    expected.zipWithIndex.foreach { case ((fcId, fileId), mappingIndex) =>
      val obtained = mapping.fileIdFor(fcId)
      test(
        rowIndex + "." + mappingIndex + s". fileIdFor determines $fileId for input FormComponentId($fcId)"
      ) {
        assertEquals(fileId, obtained)
      }
    }
  }
}
