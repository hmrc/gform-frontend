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
import play.api.libs.json.JsValue
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, DataRetrieve, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.http.HttpReads.Implicits.{ readFromJson, readRaw }

class DataRetrieveConnectorBlueprint(ws: WSHttp, rawUrl: String, identifier: String) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  def get(dataRetrieve: DataRetrieve, request: DataRetrieve.Request)(implicit
    ex: ExecutionContext,
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val url = request.fillPlaceholders(rawUrl)

    ws.GET[JsValue](url)
      .map { response =>
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
      .recover { ex =>
        logger.error(s"Unknown problem when calling $identifier", ex)
        CannotRetrieveResponse
      }
  }

  def post(dataRetrieve: DataRetrieve, request: DataRetrieve.Request, header: Seq[(String, String)] = Seq.empty)(
    implicit
    ex: ExecutionContext,
    hc: HeaderCarrier
  ): Future[ServiceCallResponse[DataRetrieve.Response]] = {

    val url = request.fillPlaceholders(rawUrl)

    ws.POST[JsValue, HttpResponse](
      url,
      request.json,
      header
    ).map { httpResponse =>
      val status = httpResponse.status
      status match {
        case 200 =>
          dataRetrieve
            .processResponse(httpResponse.json)
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
        case other =>
          logger.error(s"Problem when calling $identifier. Http status: $other, body: ${httpResponse.body}")
          CannotRetrieveResponse
      }
    }.recover { ex =>
      logger.error(s"Unknown problem when calling $identifier", ex)
      CannotRetrieveResponse
    }
  }
}
