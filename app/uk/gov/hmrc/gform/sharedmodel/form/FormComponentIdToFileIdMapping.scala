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

import play.api.libs.json.Format
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }

/** Keeps maping between FileUpload component and it's associated FileId in file-upload microservice.
  *
  * When removing add-to-list iterations (or groups) we
  * are reindexing FormComponentIds of the components accross all iteration so there
  * are not gaps in indexes. This has unfortunate effect on uploaded files, which contains
  * FormComponentId as a prefix (to guaranee uniqueness of names in zip file later on).
  *
  * This applies equally to Group component.
  *
  * Since files lives in envelope in file-upload service we can't change their prefix.
  * So we need to keep track of what FormComponentId belongs to what FileId in this mapping.
  */
case class FormComponentIdToFileIdMapping(mapping: Map[FormComponentId, FileId]) {
  val inverseMapping: Map[FileId, FormComponentId] = mapping.map { case (k, v) => (v, k) }
  def modelComponentIds: Set[ModelComponentId] =
    inverseMapping.keySet.map(fileId => FormComponentId(fileId.value).modelComponentId)

  def fileIdFor(formComponentId: FormComponentId): FileId = {

    def loop(fileId: FileId): Option[FileId] = {
      val inverse: Option[FileId] = inverseMapping.get(fileId).map(_.value).map(FileId.apply)
      inverse.map { fileId =>
        loop(fileId).getOrElse(fileId)
      }
    }

    mapping.getOrElse(formComponentId, loop(FileId(formComponentId.value)).getOrElse(FileId(formComponentId.value)))

  }

  def +(formComponentId: FormComponentId, fileId: FileId): FormComponentIdToFileIdMapping =
    FormComponentIdToFileIdMapping(mapping + (formComponentId -> fileId))
  def -(formComponentId: FormComponentId): FormComponentIdToFileIdMapping =
    FormComponentIdToFileIdMapping(mapping - formComponentId)

  def find(formComponentId: FormComponentId): Option[FileId] = mapping.get(formComponentId)
  def find(modelComponentId: ModelComponentId): Option[FileId] = find(modelComponentId.toFormComponentId)

}

object FormComponentIdToFileIdMapping {
  val empty = FormComponentIdToFileIdMapping(Map.empty)
  val formatMap: Format[Map[FormComponentId, FileId]] = {
    implicit val fileIdFormat: Format[FileId] = JsonUtils.valueClassFormat[FileId, String](FileId.apply, _.value)
    JsonUtils.formatMap(FormComponentId.apply, _.value)
  }

  implicit val format: Format[FormComponentIdToFileIdMapping] = Format(
    formatMap.map(FormComponentIdToFileIdMapping.apply),
    formatMap.contramap(_.mapping)
  )
}
