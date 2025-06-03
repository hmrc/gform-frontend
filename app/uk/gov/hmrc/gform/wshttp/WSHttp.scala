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

import com.typesafe.config.Config
import play.api.libs.json.{ Json, Writes }
import uk.gov.hmrc.http.client.{ HttpClientV2, RequestBuilder }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, StringContextOps }
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }

trait WSHttp extends HttpClientV2 {
  //Old-style HTTP methods for backwards compatibility
  def GET[A](
    url: String,
    queryParams: Seq[(String, String)] = Seq.empty,
    headers: Seq[(String, String)] = Seq.empty
  )(implicit rds: HttpReads[A], hc: HeaderCarrier, ec: ExecutionContext): Future[A]
  def POST[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O]
  def POSTEmpty[O](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O]
  def PUT[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O]
  def PATCH[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O]
  def DELETE[O](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O]
}

class WSHttpImpl(
  val appName: String,
  val auditConnector: AuditConnector,
  val configuration: Config,
  val httpClientV2: HttpClientV2
) extends WSHttp with HttpAuditing {

  override protected def mkRequestBuilder(url: URL, method: String)(implicit hc: HeaderCarrier): RequestBuilder =
    method.toUpperCase match {
      case "GET"    => httpClientV2.get(url)
      case "POST"   => httpClientV2.post(url)
      case "PUT"    => httpClientV2.put(url)
      case "PATCH"  => httpClientV2.patch(url)
      case "DELETE" => httpClientV2.delete(url)
      case "HEAD"   => httpClientV2.head(url)
      case _        => throw new IllegalArgumentException(s"Unsupported HTTP method: $method")
    }

  override def get(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = httpClientV2.get(url)(hc)
  override def post(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = httpClientV2.post(url)(hc)
  override def put(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = httpClientV2.put(url)(hc)
  override def delete(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = httpClientV2.delete(url)(hc)
  override def head(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = httpClientV2.head(url)(hc)

  override def GET[A](
    url: String,
    queryParams: Seq[(String, String)] = Seq.empty,
    headers: Seq[(String, String)] = Seq.empty
  )(implicit rds: HttpReads[A], hc: HeaderCarrier, ec: ExecutionContext): Future[A] = {
    val requestBuilder = httpClientV2.get(url"$url")
    val withParams =
      if (queryParams.nonEmpty) requestBuilder.transform(_.addQueryStringParameters(queryParams: _*))
      else requestBuilder
    val withHeaders = if (headers.nonEmpty) withParams.setHeader(headers: _*) else withParams
    withHeaders.execute[A]
  }

  override def POST[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] = {
    val requestBuilder = httpClientV2.post(url"$url").withBody(Json.toJson(body))
    val withHeaders = if (headers.nonEmpty) requestBuilder.setHeader(headers: _*) else requestBuilder
    withHeaders.execute[O]
  }

  override def POSTEmpty[O](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] = {
    val requestBuilder = httpClientV2.post(url"$url")
    val withHeaders = if (headers.nonEmpty) requestBuilder.setHeader(headers: _*) else requestBuilder
    withHeaders.execute[O]
  }

  override def PUT[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] = {
    val requestBuilder = httpClientV2.put(url"$url").withBody(Json.toJson(body))
    val withHeaders = if (headers.nonEmpty) requestBuilder.setHeader(headers: _*) else requestBuilder
    withHeaders.execute[O]
  }

  override def PATCH[I, O](url: String, body: I, headers: Seq[(String, String)] = Seq.empty)(implicit
    wts: Writes[I],
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] = {
    val requestBuilder = httpClientV2.patch(url"$url").withBody(Json.toJson(body))
    val withHeaders = if (headers.nonEmpty) requestBuilder.setHeader(headers: _*) else requestBuilder
    withHeaders.execute[O]
  }

  override def DELETE[O](url: String, headers: Seq[(String, String)] = Seq.empty)(implicit
    rds: HttpReads[O],
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[O] = {
    val requestBuilder = httpClientV2.delete(url"$url")
    val withHeaders = if (headers.nonEmpty) requestBuilder.setHeader(headers: _*) else requestBuilder
    withHeaders.execute[O]
  }
}
