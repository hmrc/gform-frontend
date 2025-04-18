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

import cats.syntax.eq._
import uk.gov.hmrc.gform.gform.SummaryPagePurpose
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.{ FileId, Form, FormComponentIdToFileIdMapping }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FileComponentId, FormComponent }

class EnvelopeWithMapping(
  private val envelope: Envelope,
  val mapping: FormComponentIdToFileIdMapping
) {

  def files: List[File] = envelope.files

  def fileIdFor(fileComponentId: FileComponentId): FileId = mapping.fileIdFor(fileComponentId)

  def contains(modelComponentId: ModelComponentId): Boolean = findSingle(modelComponentId).isDefined

  def findMulti(modelComponentId: ModelComponentId): List[(FileComponentId.Multi, File)] =
    mapping
      .findMulti(modelComponentId)
      .flatMap { case (fileComponentId, fileId) =>
        envelope.files
          .map { envelopeFile =>
            fileComponentId -> envelopeFile
          }
          .filter { case (_, file) => file.fileId === fileId }
      }

  def findSingle(modelComponentId: ModelComponentId): Option[File] =
    mapping
      .findSingle(modelComponentId)
      .flatMap { fileId =>
        envelope.files.find(_.fileId === fileId)
      }

  def byPurpose(summaryPagePurpose: SummaryPagePurpose) = {
    val envelopeUpd = summaryPagePurpose match {
      case SummaryPagePurpose.ForUser => envelope.withUserFileNames
      case SummaryPagePurpose.ForDms  => envelope
      case other                      => throw new Exception(s"Invalid summaryPagePurpose, got $other")
    }
    new EnvelopeWithMapping(envelopeUpd, mapping)
  }

  def userFileName(formComponent: FormComponent): String =
    findSingle(formComponent.modelComponentId).fold("")(_.fileName)

  def userFileNames(formComponent: FormComponent): List[String] =
    findMulti(formComponent.modelComponentId).map { case (_, file) => file.fileName }

}

object EnvelopeWithMapping {
  val empty = new EnvelopeWithMapping(Envelope.empty, FormComponentIdToFileIdMapping.empty)

  def apply(envelope: Envelope, form: Form): EnvelopeWithMapping = new EnvelopeWithMapping(
    envelope,
    form.componentIdToFileId
  )

}
