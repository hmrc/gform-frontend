/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.wshttp

import izumi.reflect.Tag
import play.api.libs.json.{ JsValue, Json }
import play.api.libs.ws.{ BodyWritable, WSClient, WSRequest }
import play.api.test.WsTestClient.InternalWSClient
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }
import uk.gov.hmrc.http.client.{ HttpClientV2, RequestBuilder, StreamHttpReads }

import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }

object HttpTestUtils {

  def getTestImpl(wiremockPort: Int): HttpClientV2 =
    new HttpClientV2 {
      private val wsClient = new InternalWSClient("http", wiremockPort)

      override def get(url: URL)(implicit hc: HeaderCarrier): RequestBuilder =
        new TestRequestBuilder(wsClient, url.toString, "GET")
      override def post(url: URL)(implicit hc: HeaderCarrier): RequestBuilder =
        new TestRequestBuilder(wsClient, url.toString, "POST")
      override def put(url: URL)(implicit hc: HeaderCarrier): RequestBuilder =
        new TestRequestBuilder(wsClient, url.toString, "PUT")
      override def delete(url: URL)(implicit hc: HeaderCarrier): RequestBuilder =
        new TestRequestBuilder(wsClient, url.toString, "DELETE")
      override def head(url: URL)(implicit hc: HeaderCarrier): RequestBuilder =
        new TestRequestBuilder(wsClient, url.toString, "HEAD")

      override protected def mkRequestBuilder(url: URL, method: String)(implicit hc: HeaderCarrier): RequestBuilder =
        new TestRequestBuilder(wsClient, url.toString, method)

      class TestRequestBuilder(wsClient: WSClient, url: String, method: String) extends RequestBuilder {
        private var wsRequest = wsClient.url(url)
        private var requestBody: Option[JsValue] = None

        override def execute[A](implicit rds: HttpReads[A], ec: ExecutionContext): Future[A] = {
          val responseFuture = method.toUpperCase match {
            case "GET" => wsRequest.get()
            case "POST" =>
              requestBody match {
                case Some(body) => wsRequest.post(body)
                case None       => wsRequest.post("")
              }
            case "PUT" =>
              requestBody match {
                case Some(body) => wsRequest.put(body)
                case None       => wsRequest.put("")
              }
            case "PATCH" =>
              requestBody match {
                case Some(body) => wsRequest.patch(body)
                case None       => wsRequest.patch("")
              }
            case "DELETE" => wsRequest.delete()
            case "HEAD"   => wsRequest.head()
            case _        => throw new UnsupportedOperationException(s"Method $method not supported")
          }

          responseFuture.map { wsResponse =>
            val httpResponse = HttpResponse(
              status = wsResponse.status,
              body = wsResponse.body,
              headers = wsResponse.headers.map { case (k, v) => k -> v.toSeq }.toMap
            )
            rds.read(method, url, httpResponse)
          }(ec)
        }

        override def withBody[B: BodyWritable: Tag](body: B)(implicit ec: ExecutionContext): RequestBuilder = {
          body match {
            case jsValue: JsValue => requestBody = Some(jsValue)
            case jsonString: String =>
              try requestBody = Some(Json.parse(jsonString))
              catch {
                case _: Exception =>
                  wsRequest = wsRequest.withBody(jsonString)
              }
            case other =>
              wsRequest = wsRequest.withBody(other)
          }
          this
        }

        override def setHeader(headers: (String, String)*): RequestBuilder = {
          wsRequest = wsRequest.withHttpHeaders(headers: _*)
          this
        }

        override def transform(transform: WSRequest => WSRequest): RequestBuilder = {
          wsRequest = transform(wsRequest)
          this
        }

        override def stream[A: StreamHttpReads](implicit ec: ExecutionContext): Future[A] = {
          val streamReads = implicitly[StreamHttpReads[A]]
          execute(streamReads.asInstanceOf[HttpReads[A]], ec)
        }
        override def withProxy: RequestBuilder = this
      }
    }

}
