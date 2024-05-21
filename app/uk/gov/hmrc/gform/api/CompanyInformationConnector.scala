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

import org.slf4j.{ Logger, LoggerFactory }
import play.api.http.Status
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import scala.concurrent.{ ExecutionContext, Future }

trait CompanyInformationConnector[F[_]] {
  def companyProfile(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]
}

class CompanyInformationAsyncConnector(ws: WSHttp, baseUrl: String)(implicit ex: ExecutionContext)
    extends CompanyInformationConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val urlWithPlaceholders = s"$baseUrl/companies-house-api-proxy/company/{{companyNumber}}"
  private val identifier = "company profile"

  override def companyProfile(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val url = request.fillPlaceholders(urlWithPlaceholders)

    ws.GET[HttpResponse](url)
      .map { httpResponse =>
        httpResponse.status match {
          case Status.OK =>
            dataRetrieve
              .processResponse(httpResponse.json)
              .fold(
                invalid => {
                  logger.error(
                    s"Calling $identifier returned successfully, but marshalling of data failed with: $invalid"
                  )
                  CannotRetrieveResponse
                },
                valid => {
                  logger.info(s"Calling $identifier returned Success.")
                  ServiceResponse(valid)
                }
              )
          case Status.NOT_FOUND =>
            logger.info(s"Calling $identifier returned successfully, but no company was found: $httpResponse")
            ServiceResponse[DataRetrieve.Response](DataRetrieve.Response.Object(Map.empty))
          case other =>
            logger.error(s"Problem when calling $identifier. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover { case ex =>
        logger.error(s"Unknown problem when calling $identifier", ex)
        CannotRetrieveResponse
      }
  }
}
