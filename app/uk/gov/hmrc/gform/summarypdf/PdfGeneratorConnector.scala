/*
 * Copyright 2022 HM Revenue & Customs
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
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.sharedmodel.PdfHtml

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.wshttp.WSHttp

import scala.concurrent.Future
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

class PdfGeneratorConnector(servicesConfig: ServicesConfig, wSHttp: WSHttp)(implicit
  ec: ExecutionContext
) {
  private val logger = LoggerFactory.getLogger(getClass)

  def generatePDF(payload: Map[String, Seq[PdfHtml]], headers: Seq[(String, String)]): Future[Source[ByteString, _]] = {
    val url = s"$baseURL/pdf-generator-service/generate"

    val payloadMap: Map[String, Seq[String]] = payload.mapValues(_.map(_.html.replaceAllLiterally("<br>", "<br/>")))
    val payloadSize = payloadMap.foldLeft(0) { case (acc, (key, value)) => acc + key.size + value.map(_.size).sum }
    logger.info(s"Generate pdf. Html payload size is: $payloadSize bytes.")
    wSHttp
      .buildRequest(url, headers)
      .withMethod("POST")
      .withBody(payloadMap)
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
