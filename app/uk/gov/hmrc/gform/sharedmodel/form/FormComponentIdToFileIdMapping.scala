/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileComponentId, FormComponentId, JsonUtils }

/** Keeps maping between FileUpload component and it's associated FileId in object-store microservice.
  *
  * When removing add-to-list iterations (or groups) we
  * are reindexing FormComponentIds of the components accross all iteration so there
  * are not gaps in indexes. This has unfortunate effect on uploaded files, which contains
  * FormComponentId as a prefix (to guaranee uniqueness of names in zip file later on).
  *
  * This applies equally to Group component.
  *
  * Since files lives in object-store service we can't change their prefix.
  * So we need to keep track of what FormComponentId belongs to what FileId in this mapping.
  */
case class FormComponentIdToFileIdMapping(mapping: Map[FileComponentId, FileId]) {
  val inverseMapping: Map[FileId, FileComponentId] = mapping.map { case (k, v) => (v, k) }
  def modelComponentIds: Set[ModelComponentId] =
    inverseMapping.keySet.map(fileId => FormComponentId(fileId.value).modelComponentId)

  def fileIdFor(fileComponentId: FileComponentId): FileId = {

    def loop(fileId: FileId): Option[FileId] = {
      val inverse: Option[FileId] = inverseMapping.get(fileId).map(_.value()).map(FileId.apply)
      inverse.map { fileId =>
        loop(fileId).getOrElse(fileId)
      }
    }

    mapping.getOrElse(fileComponentId, loop(fileComponentId.toFileId()).getOrElse(fileComponentId.toFileId()))

  }

  def +(fileComponentId: FileComponentId, fileId: FileId): FormComponentIdToFileIdMapping =
    FormComponentIdToFileIdMapping(mapping + (fileComponentId -> fileId))
  def -(fileComponentId: FileComponentId): FormComponentIdToFileIdMapping =
    fileComponentId match {
      case FileComponentId.Single(_) => FormComponentIdToFileIdMapping(mapping - fileComponentId)
      case FileComponentId.Multi(fcId, index) =>
        FormComponentIdToFileIdMapping(
          mapping
            .filter {
              case (FileComponentId.Multi(fcId2, index2), v) if fcId2 === fcId && index2 === index => false
              case _                                                                               => true
            }
            .map {
              case (fc @ FileComponentId.Multi(fcId2, index2), v) if fcId2 === fcId && index2 > index =>
                (fc.decrement(), v)
              case keep => keep
            }
        )

    }

  def find(fileComponentId: FileComponentId): Option[FileId] = mapping.get(fileComponentId)
  def findSingle(modelComponentId: ModelComponentId): Option[FileId] = find(
    FileComponentId.Single(modelComponentId.toFormComponentId)
  )

  def findMulti(modelComponentId: ModelComponentId): List[(FileComponentId.Multi, FileId)] =
    mapping
      .collect {
        case (fc @ FileComponentId.Multi(_, _), value) if fc.isMultiFor(modelComponentId.toFormComponentId) =>
          fc -> value
      }
      .toList
      .sortBy { case (fileComponentId, _) => fileComponentId.value() }

}

object FormComponentIdToFileIdMapping {
  val empty = FormComponentIdToFileIdMapping(Map.empty)
  val formatMap: Format[Map[FileComponentId, FileId]] = {
    implicit val fileIdFormat: Format[FileId] = JsonUtils.valueClassFormat[FileId, String](FileId.apply, _.value)
    JsonUtils.formatMap(FileComponentId.fromString(_), _.value())
  }

  implicit val format: Format[FormComponentIdToFileIdMapping] = Format(
    formatMap.map(FormComponentIdToFileIdMapping.apply),
    formatMap.contramap(_.mapping)
  )
}
