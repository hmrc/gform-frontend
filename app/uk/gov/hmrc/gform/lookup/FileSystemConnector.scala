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

package uk.gov.hmrc.gform.lookup

import org.slf4j.{ Logger, LoggerFactory }
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.sharedmodel._

import scala.concurrent.Future

class FileSystemConnector {
  private val logger: Logger = LoggerFactory.getLogger(getClass)

  private val csvTaxRateAdapter = new CsvTaxRateAdapter()

  def getHmrcTaxRate(
    dataRetrieve: DataRetrieve,
    request: DataRetrieve.Request
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    processResponse(dataRetrieve, csvTaxRateAdapter.search(request))

  private def processResponse(
    dataRetrieve: DataRetrieve,
    response: Option[JsValue]
  ): Future[ServiceCallResponse[DataRetrieve.Response]] =
    if (response.isDefined) {
      Future.successful {
        dataRetrieve
          .processResponse(response.get)
          .fold(
            invalid => {
              logger.error(
                s"Fetching ${dataRetrieve.tpe.name} found result, but marshalling of data failed with: $invalid"
              )
              CannotRetrieveResponse
            },
            valid => {
              logger.info(s"Fetching ${dataRetrieve.tpe.name} returned: Success.")
              ServiceResponse(valid)
            }
          )
      }
    } else {
      logger.error(s"Fetching ${dataRetrieve.tpe.name} returned no result.")
      Future.successful(NotFound)
    }
}
