/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import play.api.libs.json.JsValue
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits._
import scala.concurrent.{ ExecutionContext, Future }
// Only used for local development, hence hardcoded host:port
class ObjectStoreAdminConnector(httpClientV2: HttpClientV2)(implicit ec: ExecutionContext) {
  private val tokenUrl: String = "http://localhost:8470/test-only/token"
  def isAuthorized(token: String): Future[Boolean] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    httpClientV2
      .get(url"$tokenUrl")
      .setHeader("Authorization" -> token)
      .execute[HttpResponse]
      .map { response =>
        response.status == 200
      }
  }
  def login(payload: JsValue): Future[HttpResponse] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    httpClientV2
      .post(url"$tokenUrl")
      .withBody(payload)
      .execute[HttpResponse]
  }
}
