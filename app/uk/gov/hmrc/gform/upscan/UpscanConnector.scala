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

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson

class UpscanConnector(ws: WSHttp, baseUrl: String)(implicit
  ec: ExecutionContext
) {

  def upscanInitiate(request: UpscanInitiateRequest)(implicit hc: HeaderCarrier): Future[UpscanInitiateResponse] =
    ws.POST[UpscanInitiateRequest, UpscanInitiateResponse](
      s"$baseUrl/v2/initiate",
      request,
      List.empty[(String, String)]
    )

}
