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

package uk.gov.hmrc.gform.api

import uk.gov.hmrc.gform.gform.DataRetrieveConnectorBlueprint
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, ServiceCallResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

trait NinoInsightsConnector[F[_]] {
  def insights(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  ): F[ServiceCallResponse[DataRetrieve.Response]]
}

class NinoInsightsAsyncConnector(ws: WSHttp, baseUrl: String, authorizationToken: String)(implicit ex: ExecutionContext)
    extends NinoInsightsConnector[Future] {

  val insightsB = new DataRetrieveConnectorBlueprint(ws, s"$baseUrl/check/insights", "nino insights")

  override def insights(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    implicit val hc = HeaderCarrier()

    val header: Seq[(String, String)] = Seq(
      "Authorization" -> authorizationToken,
      "Content-Type"  -> "application/json"
    )

    insightsB.post(dataRetrieve, request, header)
  }
}
