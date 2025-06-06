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

package uk.gov.hmrc.gform.upscan

import play.api.libs.json.Json

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.{ HeaderCarrier, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson
import uk.gov.hmrc.http.client.HttpClientV2

class UpscanConnector(httpClient: HttpClientV2, baseUrl: String)(implicit
  ec: ExecutionContext
) {

  def upscanInitiate(request: UpscanInitiateRequest)(implicit hc: HeaderCarrier): Future[UpscanInitiateResponse] =
    httpClient
      .post(url"$baseUrl/v2/initiate")
      .withBody(Json.toJson(request))
      .execute[UpscanInitiateResponse]

}
