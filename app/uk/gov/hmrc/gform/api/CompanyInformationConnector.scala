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
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

trait CompanyInformationConnector[F[_]] {
  def companyProfile(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]
}

class CompanyInformationAsyncConnector(ws: WSHttp, baseUrl: String)(implicit ex: ExecutionContext)
    extends CompanyInformationConnector[Future] {

  private val urlWithPlaceholders = s"$baseUrl/companies-house-api-proxy/company/{{companyNumber}}"
  private val companyProfileB = new DataRetrieveConnectorBlueprint(ws, urlWithPlaceholders, "company profile")

  override def companyProfile(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] =
    companyProfileB.get(dataRetrieve, request).map {
      case CannotRetrieveResponse => ServiceResponse[DataRetrieve.Response](DataRetrieve.Response.Object(Map.empty))
      case otherwise              => otherwise
    }

}
