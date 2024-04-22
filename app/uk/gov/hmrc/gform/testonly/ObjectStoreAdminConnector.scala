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
import play.api.libs.ws.WSClient
import play.api.libs.ws.WSResponse
import scala.concurrent.{ ExecutionContext, Future }

// Only used for local development, hence hardcoded host:port
class ObjectStoreAdminConnector(wsClient: WSClient)(implicit ec: ExecutionContext) {

  private val tokenUrl: String = "http://localhost:8470/test-only/token"

  def isAuthorized(token: String): Future[Boolean] =
    wsClient
      .url(tokenUrl)
      .withHttpHeaders("Authorization" -> token)
      .get()
      .map { response =>
        response.status == 200
      }

  def login(
    payload: JsValue
  ): Future[WSResponse] =
    wsClient
      .url(tokenUrl)
      .post(payload)
}
