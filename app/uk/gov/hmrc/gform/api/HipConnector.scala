/*
 * Copyright 2025 HM Revenue & Customs
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
import play.api.http.HeaderNames.AUTHORIZATION
import play.api.http.Status
import play.api.libs.json.{ JsArray, JsResult, JsValue }
import uk.gov.hmrc.gform.config.HipConnectorConfig
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import scala.concurrent.{ ExecutionContext, Future }

trait HipConnector[F[_]] {
  def employmentSummary(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): F[ServiceCallResponse[DataRetrieve.Response]]
}

class HipAsyncConnector(ws: WSHttp, baseUrl: String, hipConfig: HipConnectorConfig)(implicit ex: ExecutionContext)
    extends HipConnector[Future] {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val employmentSummaryUrlWithPlaceholders =
    s"$baseUrl${hipConfig.basePath}/nps/nps-json-service/nps/v1/api/employment/employment-summary/{{nino}}/taxYear/{{taxYear}}"
  private val employmentSummaryIdentifier = "employment summary"

  private val headers: Seq[(String, String)] = Seq(
    AUTHORIZATION          -> s"Basic ${hipConfig.authorizationToken}",
    "correlationId"        -> hipConfig.correlationId,
    "gov-uk-originator-id" -> hipConfig.originatorId
  )

  def employmentSummary(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  )(implicit hc: HeaderCarrier): Future[ServiceCallResponse[DataRetrieve.Response]] = {
    val url = request.fillPlaceholders(employmentSummaryUrlWithPlaceholders)

    ws.GET[HttpResponse](url, headers = headers)
      .map { httpResponse =>
        logger.info(
          s"Calling $employmentSummaryIdentifier returned response status: ${httpResponse.status} body: ${httpResponse.body}"
        )
        httpResponse.status match {
          case Status.OK =>
            processEmploymentSummary(httpResponse.json)
              .fold(
                invalid => {
                  logger.error(
                    s"Calling $employmentSummaryIdentifier returned successfully, but marshalling of data failed with: $invalid"
                  )
                  CannotRetrieveResponse
                },
                validResponse => {
                  println(s"validResponse: $validResponse")
                  dataRetrieve
                    .processResponse(validResponse)
                    .fold(
                      invalid => {
                        logger.error(
                          s"Calling internal $employmentSummaryIdentifier returned successfully, but marshalling of data failed with: $invalid"
                        )
                        CannotRetrieveResponse
                      },
                      valid => {
                        logger.info(s"Calling $employmentSummaryIdentifier returned Success.")
                        ServiceResponse(valid)
                      }
                    )
                }
              )
          case Status.NOT_FOUND =>
            logger.info(
              s"Calling $employmentSummaryIdentifier returned successfully, but no company was found: $httpResponse"
            )
            ServiceResponse[DataRetrieve.Response](DataRetrieve.Response.Object(Map.empty))
          case other =>
            logger.error(
              s"Problem when calling $employmentSummaryIdentifier. Http status: $other, body: ${httpResponse.body}"
            )
            CannotRetrieveResponse
        }
      }
      .recover { case ex =>
        logger.error(s"Unknown problem when calling $employmentSummaryIdentifier", ex)
        CannotRetrieveResponse
      }
  }

  private def processEmploymentSummary(json: JsValue): JsResult[JsArray] =
    (json \ "individualsEmploymentDetails").validate[JsArray]
}
