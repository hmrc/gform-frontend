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

import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.auditing.loggingHelpers
import uk.gov.hmrc.gform.commons.HttpFunctions
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.Future
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpReadsInstances, HttpResponse, NotFoundException }

class FileUploadConnector(wSHttp: WSHttp, baseUrl: String)(
  implicit ec: ExecutionContext
) extends HttpFunctions {
  private val logger = LoggerFactory.getLogger(getClass)

  def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Envelope] = {
    logger.info(s" get envelope, envelopeId: ${envelopeId.value}, ${loggingHelpers.cleanHeaderCarrierHeader(hc)}")
    implicit val httpReads: HttpReads[Envelope] = jsonHttpReads(HttpReadsInstances.readJsValue.map(_.as[Envelope]))
    wSHttp.GET[Envelope](s"$baseUrl/envelopes/${envelopeId.value}")
  }

  def getMaybeEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Option[Envelope]] =
    getEnvelope(envelopeId).map(Some(_)).recover {
      case e: NotFoundException => None
    }

  def deleteFile(envelopeId: EnvelopeId, fileId: FileId)(implicit hc: HeaderCarrier): Future[Unit] = {
    logger.info(s" delete file, envelopeId: '${envelopeId.value}', fileId: '${fileId.value}', ${loggingHelpers
      .cleanHeaderCarrierHeader(hc)}")
    implicit val httpReads: HttpReads[HttpResponse] = jsonHttpReads(HttpReadsInstances.readRaw)
    wSHttp
      .DELETE[HttpResponse](s"$baseUrl/envelopes/${envelopeId.value}/files/${fileId.value}")
      .map(_ => ())
  }
}
