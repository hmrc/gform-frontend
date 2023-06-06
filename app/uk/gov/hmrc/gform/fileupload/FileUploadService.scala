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

package uk.gov.hmrc.gform.fileupload

import uk.gov.hmrc.gform.gformbackend.GformConnector

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class FileUploadService(fileUploadConnector: FileUploadConnector, gformConnector: GformConnector)(implicit
  ec: ExecutionContext
) extends FileUploadAlgebra[Future] {

  override def getEnvelope(envelopeId: EnvelopeId)(implicit
    hc: HeaderCarrier
  ): Future[Envelope] = {
    val envelopeF = gformConnector.getMaybeEnvelope(envelopeId)
    envelopeF.flatMap {
      case Some(envelope) => Future.successful(envelope)
      case None           => fileUploadConnector.getEnvelope(envelopeId)
    }
  }

  override def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(
    objectStore: Option[Boolean]
  )(implicit hc: HeaderCarrier): Future[Unit] = objectStore match {
    case Some(true) => gformConnector.deleteFile(envelopeId, fileId)
    case _          => fileUploadConnector.deleteFile(envelopeId, fileId)
  }

  override def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(
    objectStore: Option[Boolean]
  )(implicit hc: HeaderCarrier): Future[Unit] = objectStore match {
    case Some(true) =>
      fileIds.foldLeft(Future.unit) { case (result, fileId) =>
        result.flatMap(_ => deleteFile(envelopeId, fileId)(objectStore))
      }
    case _ =>
      Future
        .traverse(fileIds) { fileId =>
          deleteFile(envelopeId, fileId)(objectStore)
        }
        .map(_ => ())
  }

}
