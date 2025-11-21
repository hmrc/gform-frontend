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

package uk.gov.hmrc.gform.gform

import org.slf4j.{ Logger, LoggerFactory }
import play.api.http.Status.{ NOT_FOUND, OK }
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.exceptions.DataRetrieveResponseValidationException
import uk.gov.hmrc.gform.gform.DataRetrieveResponseValidator._
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HttpReads.Implicits.{ readFromJson, readRaw }
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }

class DataRetrieveConnectorBlueprint(
  httpClient: HttpClientV2,
  rawUrl: String,
  identifier: String,
  exceptionalResponses: Option[List[ExceptionalResponse]] = None,
  enableResponseValidation: Boolean = false
) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private def validateResponse(json: JsValue, dataRetrieve: DataRetrieve): Unit =
    if (enableResponseValidation) {
      val result = validateDataRetrieveResponse(json, dataRetrieve)
      result match {
        case DataRetrieveValidationResult.Success => // Success, do nothing
        case DataRetrieveValidationResult.Failure(errors) =>
          throw new DataRetrieveResponseValidationException(
            s"Data Retrieve response validation failed: ${errors.mkString(", ")}"
          )
      }
    }

  def get(dataRetrieve: DataRetrieve, request: DataRetrieve.Request, header: Seq[(String, String)] = Seq.empty)(implicit
    ex: ExecutionContext,
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val url = request.fillPlaceholders(rawUrl)

    httpClient
      .get(url"$url")
      .setHeader(header: _*)
      .execute[JsValue]
      .map { response =>
        validateResponse(response, dataRetrieve)

        dataRetrieve
          .processResponse(response)
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
      }
      .recover {
        case drResponseEx: DataRetrieveResponseValidationException =>
          logger.error(s"Response validation errors when calling $identifier: ${drResponseEx.getMessage}")
          CannotRetrieveResponse
        case otherEx =>
          logger.error(s"Unknown problem when calling $identifier", otherEx)
          CannotRetrieveResponse
      }
  }

  def getEmptyIfNotFound(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request,
    header: Seq[(String, String)] = Seq.empty
  )(implicit
    ex: ExecutionContext,
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val url = request.fillPlaceholders(rawUrl)

    httpClient
      .get(url"$url")
      .setHeader(header: _*)
      .execute[HttpResponse]
      .map { response =>
        response.status match {
          case OK =>
            validateResponse(response.json, dataRetrieve)

            dataRetrieve
              .processResponse(response.json)
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
          case NOT_FOUND => dataRetrieve.emptyValidResponse()
          case other =>
            logger.error(s"Unexpected status $other received when calling $identifier")
            CannotRetrieveResponse
        }
      }
      .recover {
        case drResponseEx: DataRetrieveResponseValidationException =>
          logger.error(s"Response validation errors when calling $identifier: ${drResponseEx.getMessage}")
          CannotRetrieveResponse
        case otherEx =>
          logger.error(s"Unknown problem when calling $identifier", otherEx)
          CannotRetrieveResponse
      }
  }

  def post(dataRetrieve: DataRetrieve, request: DataRetrieve.Request, header: Seq[(String, String)] = Seq.empty)(
    implicit
    ex: ExecutionContext,
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val url = request.fillPlaceholders(rawUrl)

    httpClient
      .post(url"$url")
      .withBody(Json.toJson(request.json))
      .setHeader(header: _*)
      .execute[HttpResponse]
      .map { httpResponse =>
        val status: Int = httpResponse.status
        val maybeExceptionalResponse: Option[ExceptionalResponse] = exceptionalResponses.flatMap(_.find { ex =>
          status == ex.statusMatch && httpResponse.body.contains(ex.responseMatch)
        })

        (maybeExceptionalResponse, status) match {
          case (Some(_), _) | (_, 200) =>
            val responseJson = maybeExceptionalResponse.fold(httpResponse.json)(ex => Json.parse(ex.response))

            validateResponse(responseJson, dataRetrieve)

            dataRetrieve
              .processResponse(responseJson)
              .fold(
                invalid => {
                  logger.error(
                    s"Calling $identifier returned $status, but marshalling of data failed with: $invalid"
                  )
                  CannotRetrieveResponse
                },
                valid => {
                  logger.info(s"Calling $identifier returned $status: Success.")
                  ServiceResponse(valid)
                }
              )
          case (_, other) =>
            logger.error(s"Problem when calling $identifier. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover {
        case drResponseEx: DataRetrieveResponseValidationException =>
          logger.error(s"Response validation errors when calling $identifier: ${drResponseEx.getMessage}")
          CannotRetrieveResponse
        case otherEx =>
          logger.error(s"Unknown problem when calling $identifier", otherEx)
          CannotRetrieveResponse
      }
  }
}

case class ExceptionalResponse(statusMatch: Int, responseMatch: String, response: String)
