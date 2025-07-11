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

package uk.gov.hmrc.gform.auth

import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits._

import scala.concurrent.{ ExecutionContext, Future }

case class GGEnrolmentRequest(portalId: String, serviceName: String, friendlyName: String, knownFacts: Seq[String])

object GGEnrolmentRequest {
  implicit val format: OFormat[GGEnrolmentRequest] = Json.format[GGEnrolmentRequest]
}

class GovernmentGatewayConnector(baseUrl: String, httpClient: HttpClientV2) {

  def enrolGGUser(request: GGEnrolmentRequest)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    httpClient.post(url"$baseUrl/enrol").withBody(Json.toJson(request)).execute[HttpResponse]
}
