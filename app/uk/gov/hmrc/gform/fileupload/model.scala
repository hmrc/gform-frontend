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

package uk.gov.hmrc.gform.fileupload

import cats.instances.string._
import cats.syntax.eq._
import play.api.libs.json._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

case class Attachments(files: List[FormComponentId]) {
  val size = files.length
}

object Attachments {
  val empty = Attachments(Nil)
  implicit val format = Json.format[Attachments]
}

case class Envelope(
  files: List[File]
) {
  def find(fileId: FileId): Option[File] =
    files.find(_.fileId === fileId)

  def find(modelComponentId: ModelComponentId): Option[File] =
    files.find(_.fileId.value === modelComponentId.toMongoIdentifier)

  def contains(modelComponentId: ModelComponentId): Boolean =
    find(modelComponentId).isDefined

  def withUserFileNames: Envelope = Envelope(
    files.map(file => file.copy(fileName = file.fileName.replace(file.fileId.value + "_", "")))
  )
}

object Envelope {
  val empty = Envelope(Nil)
  private val envelopeRawReads = Json.reads[EnvelopeRaw]
  implicit val reads: Reads[Envelope] = envelopeRawReads.map(er => Envelope(er.files.getOrElse(Nil)))

}

case class EnvelopeRaw(files: Option[List[File]])
case class File(
  fileId: FileId,
  status: Status,
  fileName: String,
  contentType: ContentType,
  length: Long,
  metadata: Map[String, List[String]]
)

object File {

  private val fileRawReads: Reads[FileRaw] = Json.reads[FileRaw]

  //TIP: look for FileStatus trait in https://github.com/hmrc/file-upload/blob/master/app/uk/gov/hmrc/fileupload/read/envelope/model.scala
  implicit val format: Reads[File] = fileRawReads.map {
    case FileRaw(id, name, "QUARANTINED", _, cType, length, metatadata) =>
      File(FileId(id), Quarantined, name, cType, length, metatadata)
    case FileRaw(id, name, "CLEANED", _, cType, length, metatadata) =>
      File(FileId(id), Cleaned, name, cType, length, metatadata)
    case FileRaw(id, name, "AVAILABLE", _, cType, length, metatadata) =>
      File(FileId(id), Available, name, cType, length, metatadata)
    case FileRaw(id, name, "INFECTED", _, cType, length, metatadata) =>
      File(FileId(id), Infected, name, cType, length, metatadata)
    case FileRaw(id, name, ERROR, reason, cType, length, metatadata) =>
      File(FileId(id), Error(reason), name, cType, length, metatadata)
    case FileRaw(id, name, other, reason, cType, length, metatadata) =>
      File(FileId(id), Other(other, reason), name, cType, length, metatadata)
  }
  private val ERROR = "UnKnownFileStatusERROR"
}

case class FileRaw(
  id: String,
  name: String,
  status: String,
  reason: Option[String],
  contentType: ContentType,
  length: Long,
  metadata: Map[String, List[String]]
)

sealed trait Status
case object Quarantined extends Status
case object Infected extends Status
case object Cleaned extends Status
case object Available extends Status
case class Other(value: String, reason: Option[String]) extends Status
case class Error(reason: Option[String]) extends Status //based on experience FU not always sets reason field
