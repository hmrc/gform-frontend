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

  override def getEnvelope(envelopeId: EnvelopeId)(objectStore: Boolean)(implicit
    hc: HeaderCarrier
  ): Future[Envelope] =
    if (objectStore) {
      val envelopeF = gformConnector.getMaybeEnvelope(envelopeId)
      envelopeF.flatMap {
        case Some(envelope) => Future.successful(envelope)
        case None           => fileUploadConnector.getEnvelope(envelopeId)
      }
    } else fileUploadConnector.getEnvelope(envelopeId)

  override def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(
    objectStore: Boolean
  )(implicit hc: HeaderCarrier): Future[Unit] =
    if (objectStore) {
      val envelopeF = gformConnector.getMaybeEnvelope(envelopeId)
      envelopeF.flatMap {
        case Some(_) => gformConnector.deleteFile(envelopeId, fileId)
        case _       => fileUploadConnector.deleteFile(envelopeId, fileId)
      }
    } else fileUploadConnector.deleteFile(envelopeId, fileId)

  override def deleteFiles(envelopeId: EnvelopeId, fileIds: Set[FileId])(
    objectStore: Boolean
  )(implicit hc: HeaderCarrier): Future[Unit] =
    if (objectStore) {
      val envelopeF = gformConnector.getMaybeEnvelope(envelopeId)
      Future
        .traverse(fileIds) { fileId =>
          envelopeF.flatMap {
            case Some(_) => gformConnector.deleteFile(envelopeId, fileId)
            case _       => fileUploadConnector.deleteFile(envelopeId, fileId)
          }
        }
        .map(_ => ())
    } else Future.traverse(fileIds)(fileId => fileUploadConnector.deleteFile(envelopeId, fileId)).map(_ => ())
}
