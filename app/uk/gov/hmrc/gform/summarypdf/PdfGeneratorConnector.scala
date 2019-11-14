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

package uk.gov.hmrc.gform.summarypdf

import akka.stream.scaladsl.Source
import akka.util.ByteString
import uk.gov.hmrc.gform.sharedmodel.PdfHtml

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class PdfGeneratorConnector(servicesConfig: ServicesConfig, wSHttp: WSHttp)(
  implicit ec: ExecutionContext
) {

  def generatePDF(payload: Map[String, Seq[PdfHtml]], headers: Seq[(String, String)])(
    implicit hc: HeaderCarrier): Future[Source[ByteString, _]] = {
    val url = s"$baseURL/pdf-generator-service/generate"
    wSHttp
      .buildRequest(url)
      .withMethod("POST")
      .withHttpHeaders(headers: _*)
      .withBody(payload.mapValues(_.map(_.html)))
      .stream()
      .flatMap { response =>
        val status = response.status
        if (status >= 200 && status < 300)
          Future.successful(response.bodyAsSource)
        else
          Future.failed(new Exception(s"POST to $url failed with status $status"))
      }
  }

  private val serviceName = "pdf-generator"
  lazy val baseURL = servicesConfig.baseUrl(serviceName) + servicesConfig.getConfString(s"$serviceName.base-path", "")

}
