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

import cats.syntax.eq._
import play.api.libs.json.Format
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, JsonUtils }

/**
  * Keeps maping between FileUpload component and it's associated FileId in file-upload microservice.
  *
  * When removing add-to-list iterations we
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
    val modelComponentId: ModelComponentId = formComponentId.modelComponentId

    def nextAvailable = modelComponentId.maybeIndex.fold(formComponentId.value) { index =>
      val existingIndices: Set[Int] =
        modelComponentIds.filter(_.baseComponentId === modelComponentId.baseComponentId).flatMap(_.maybeIndex)

      val maxIndex = if (existingIndices.isEmpty) 1 else 1 + existingIndices.max
      val allIndices: Set[Int] = (1 to maxIndex).toSet
      val diff = allIndices -- existingIndices
      if (diff.isEmpty) formComponentId.value else diff.min + "_" + modelComponentId.baseComponentId.value
    }
    FileId(mapping.get(formComponentId).fold(nextAvailable)(_.value))

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
