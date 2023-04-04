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

import play.api.i18n.Messages
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel._

import scala.concurrent.{ ExecutionContext, Future }

object DataRetrieveService {

  def retrieveData(
    dataRetrieve: DataRetrieve,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
    maybeRequestParams: Option[JsValue],
    executor: (DataRetrieve, DataRetrieve.Request) => Future[ServiceCallResponse[DataRetrieve.Response]]
  )(implicit messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {

    val request: DataRetrieve.Request = dataRetrieve.prepareRequest(formModelVisibilityOptics)

    val requestParams = request.paramsAsJson()

    if (request.notReady() || maybeRequestParams.contains(requestParams)) {
      Future.successful(None)
    } else {

      executor(dataRetrieve, request).map {
        case ServiceResponse(result) =>
          Some(
            DataRetrieveResult(
              dataRetrieve.id,
              result.toRetrieveDataType(),
              requestParams
            )
          )
        // We need to fail the journey here, otherwise expressions like
        // ${dataRetrieve.bankDetails.isValid='no'} etc.
        // will always evaluate to false
        case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve data")
      }
    }
  }

}
