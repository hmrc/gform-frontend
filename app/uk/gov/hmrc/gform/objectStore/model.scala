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

package uk.gov.hmrc.gform.objectStore

import cats.instances.string._
import cats.syntax.eq._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.config.ContentType
import uk.gov.hmrc.gform.sharedmodel.form.FileId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FileComponentId

case class Attachments(files: List[FileComponentId]) {
  val size = files.length
}

object Attachments {
  val empty = Attachments(Nil)
  implicit val format: OFormat[Attachments] = Json.format[Attachments]
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
  fileName: String,
  status: FileStatus,
  contentType: ContentType,
  length: Long,
  metadata: Map[String, List[String]]
)

object File {
  implicit val fileReads: Reads[File] = (
    (JsPath \ "id").read[FileId] and
      (JsPath \ "name").read[String] and
      (JsPath \ "status").read[FileStatus] and
      (JsPath \ "contentType").read[ContentType] and
      (JsPath \ "length").read[Long] and
      (JsPath \ "metadata").read[Map[String, List[String]]]
  )(File.apply _)
}
