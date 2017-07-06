/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.gform.gformbackend.model.FileId

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

  implicit val format: Reads[File] = fileRawReads.map {
    case FileRaw(id, name, "ERROR", Some(reason)) => File(FileId(id), Error(reason), name)
    case FileRaw(id, name, "QUARANTINED", _) => File(FileId(id), Quarantined, name)
    case FileRaw(id, name, "CLEANED", _) => File(FileId(id), Cleaned, name)
  }
  private lazy val fileRawReads: Reads[FileRaw] = Json.reads[FileRaw]
}

case class FileRaw(id: String, name: String, status: String, reason: Option[String])

sealed trait Status
case object Quarantined extends Status
case object Cleaned extends Status
case class Error(reason: String) extends Status
