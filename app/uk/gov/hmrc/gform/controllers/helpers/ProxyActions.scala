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

package uk.gov.hmrc.gform.controllers.helpers

import akka.stream.scaladsl.Source
import akka.util.ByteString
import play.api.http.HttpEntity.Streamed
import play.api.libs.streams.Accumulator
import play.api.libs.ws.{ StreamedBody, WSClient, WSRequest }
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.logging.LoggingDetails
import uk.gov.hmrc.play.HeaderCarrierConverter

class ProxyActions(wsClient: WSClient)(
  implicit ec: ExecutionContext
) {

  /**
    * This creates actions which proxies incoming request to remote service.
    */
  def apply(remoteServiceBaseUrl: String)(path: String): Action[Source[ByteString, _]] =
    Action.async(streamedBodyParser(ec)) { implicit inboundRequest: Request[Source[ByteString, _]] =>
      for {
        outboundRequest  <- proxyRequest(s"$remoteServiceBaseUrl/$path", inboundRequest)
        streamedResponse <- outboundRequest.stream
      } yield {
        val headersMap = streamedResponse.headers.headers
        val contentLength = headersMap.get(contentLengthHeaderKey).flatMap(_.headOption.map(_.toLong))
        val contentType = headersMap.get(contentTypeHeaderKey).map(_.mkString(", "))
        Result(
          ResponseHeader(
            streamedResponse.headers.status,
            streamedResponse.headers.headers.mapValues(_.head).filter(filterOutContentHeaders)),
          Streamed(streamedResponse.body, contentLength, contentType)
        )
      }
    }

  private lazy val contentTypeHeaderKey = "Content-Type"
  private lazy val contentLengthHeaderKey = "Content-Length"
  private lazy val filterOutContentHeaders: ((String, String)) => Boolean = {
    case (key, _) => !key.equalsIgnoreCase(contentTypeHeaderKey) && !key.equalsIgnoreCase(contentLengthHeaderKey)
  }

  private def proxyRequest(path: String, inboundRequest: Request[Source[ByteString, _]])(
    implicit ec: ExecutionContext): Future[WSRequest] = Future(
    wsClient
      .url(s"$path")
      .withFollowRedirects(false)
      .withMethod(inboundRequest.method)
      .withHeaders(processHeaders(inboundRequest.headers, extraHeaders = Nil): _*)
      .withQueryString(inboundRequest.queryString.mapValues(_.head).toSeq: _*)
      .withBody(StreamedBody(inboundRequest.body))
  )

  private def processHeaders(inboundHeaders: Headers, extraHeaders: Seq[(String, String)]): Seq[(String, String)] =
    (inboundHeaders.toSimpleMap.filter(headerKeyValue => !headerKeyValue._1.equals("Host")) ++ extraHeaders.toMap).toSeq

  private def streamedBodyParser(implicit ec: ExecutionContext): BodyParser[Source[ByteString, _]] = BodyParser { _ =>
    Accumulator.source[ByteString].map(Right.apply)
  }

  private implicit def hc(implicit request: Request[_]): HeaderCarrier =
    HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))
}
