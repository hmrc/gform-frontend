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

package uk.gov.hmrc.gform.wshttp

import izumi.reflect.Tag
import play.api.libs.json.Writes
import play.api.libs.ws.{ BodyWritable, WSRequest }
import uk.gov.hmrc.http.client.{ RequestBuilder, StreamHttpReads }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse, UpstreamErrorResponse }

import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }

/** Stubbed WSHttp which responses always with the same HttpResponse. Use it for test purposes
  */
class StubbedWSHttp(response: HttpResponse) extends WSHttp {

  override def get(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new StubbedRequestBuilder(response)
  override def post(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new StubbedRequestBuilder(response)
  override def put(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new StubbedRequestBuilder(response)
  override def delete(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new StubbedRequestBuilder(response)
  override def head(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = new StubbedRequestBuilder(response)

  override protected def mkRequestBuilder(url: URL, method: String)(implicit hc: HeaderCarrier): RequestBuilder =
    new StubbedRequestBuilder(response)

  override def GET[A](
    url: String,
    queryParams: Seq[(String, String)] = Seq.empty,
    headers: Seq[(String, String)] = Seq.empty
  )(implicit rds: HttpReads[A], hc: HeaderCarrier, ec: ExecutionContext): Future[A] =
    handleResponse("GET", url, response, rds)

  override def POST[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] =
    handleResponse("POST", url, response, rds)

  override def POSTEmpty[O](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] =
    handleResponse("POST", url, response, rds)

  override def PUT[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] =
    handleResponse("PUT", url, response, rds)

  override def PATCH[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] =
    handleResponse("PATCH", url, response, rds)

  override def DELETE[O](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] =
    handleResponse("DELETE", url, response, rds)

  private def handleResponse[A](method: String, url: String, response: HttpResponse, rds: HttpReads[A]): Future[A] =
    response.status match {
      case status if status >= 400 && status < 500 =>
        Future.failed(UpstreamErrorResponse(response.body, status, status, response.headers))
      case status if status >= 500 =>
        Future.failed(UpstreamErrorResponse(response.body, status, status, response.headers))
      case _ =>
        Future.successful(rds.read(method, url, response))
    }
}

private class StubbedRequestBuilder(response: HttpResponse) extends RequestBuilder {
  override def execute[A](implicit rds: HttpReads[A], ec: ExecutionContext): Future[A] =
    response.status match {
      case status if status >= 400 && status < 500 =>
        Future.failed(UpstreamErrorResponse(response.body, status, status, response.headers))
      case status if status >= 500 =>
        Future.failed(UpstreamErrorResponse(response.body, status, status, response.headers))
      case _ =>
        Future.successful(rds.read("STUBBED", "http://stubbed-url", response))
    }

  override def setHeader(headers: (String, String)*): RequestBuilder = this
  override def transform(transform: WSRequest => WSRequest): RequestBuilder = this
  override def stream[A: StreamHttpReads](implicit ec: ExecutionContext): Future[A] = execute[A]
  override def withProxy: RequestBuilder = this
  override def withBody[B: BodyWritable: Tag](body: B)(implicit ec: ExecutionContext): RequestBuilder = this
}
