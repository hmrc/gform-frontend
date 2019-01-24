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

import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class FileUploadService(fileUploadConnector: FileUploadConnector) {

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Envelope] =
    fileUploadConnector.getEnvelope(envelopeId)

  def getMaybeEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Option[Envelope]] =
    fileUploadConnector.getMaybeEnvelope(envelopeId)

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier) =
    fileUploadConnector.deleteFile(envelopeId, fileId)

}
