/*
 * Copyright 2019 HM Revenue & Customs
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

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.FileId

case class Envelope(
  files: List[File]
)

object Envelope {
  implicit val reads: Reads[Envelope] = envelopeRawReads.map(er => Envelope(er.files.getOrElse(Nil)))
  private lazy val envelopeRawReads = Json.reads[EnvelopeRaw]
}

case class EnvelopeRaw(files: Option[List[File]])
case class File(
  fileId: FileId,
  status: Status,
  fileName: String
)

object File {

  //TIP: look for FileStatus trait in https://github.com/hmrc/file-upload/blob/master/app/uk/gov/hmrc/fileupload/read/envelope/model.scala
  implicit val format: Reads[File] = fileRawReads.map {
    // format: OFF
    case FileRaw(id, name, "QUARANTINED", _)        => File(FileId(id), Quarantined, name)
    case FileRaw(id, name, "CLEANED", _)            => File(FileId(id), Cleaned, name)
    case FileRaw(id, name, "AVAILABLE", _)          => File(FileId(id), Available, name)
    case FileRaw(id, name, "INFECTED", _)           => File(FileId(id), Infected, name)
    case FileRaw(id, name, ERROR, reason)           => File(FileId(id), Error(reason), name)
    case FileRaw(id, name, other, reason)           => File(FileId(id), Other(other, reason), name)
    // format: ON
  }
  private lazy val fileRawReads: Reads[FileRaw] = Json.reads[FileRaw]
  private lazy val ERROR = "UnKnownFileStatusERROR"
}

case class FileRaw(id: String, name: String, status: String, reason: Option[String])

sealed trait Status
case object Quarantined extends Status
case object Infected extends Status
case object Cleaned extends Status
case object Available extends Status
case class Other(value: String, reason: Option[String]) extends Status
case class Error(reason: Option[String]) extends Status //based on experience FU not always sets reason field
