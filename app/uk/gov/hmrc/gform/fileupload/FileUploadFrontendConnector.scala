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

import akka.util.ByteString
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.upscan.UploadDetails
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HttpResponse

class FileUploadFrontendConnector(ws: WSHttp, baseUrl: String)(implicit
  ec: ExecutionContext
) {

  def uploadFile(
    envelopeId: EnvelopeId,
    fileId: FileId,
    uploadDetails: UploadDetails,
    bytes: ByteString
  ): Future[HttpResponse] = {
    val id = fileId.value
    val fileName = id + "_" + uploadDetails.fileName
    val url = s"$baseUrl/upload/envelopes/${envelopeId.value}/files/$id"
    ws.POSTFile(url, fileName, bytes, Seq.empty[(String, String)], uploadDetails.fileMimeType)
  }
}
