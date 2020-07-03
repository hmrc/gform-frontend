/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.Logger
import play.api.libs.json.Json
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

case class UtrEligibilityRequest(utr: String)

object UtrEligibilityRequest {
  implicit val format = Json.format[UtrEligibilityRequest]
}

class SelfEmployedIncomeSupportEligibilityConnector(baseUrl: String, http: WSHttp) {

  private def getUtrEligibility(
    request: UtrEligibilityRequest)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    http.doPost(s"$baseUrl/self-employed-income-support-eligibility/utr-eligibility", request)

  def eligibilityStatus(request: UtrEligibilityRequest, hc: HeaderCarrier)(
    implicit ec: ExecutionContext): Future[Boolean] = {
    implicit val hc_ = hc
    getUtrEligibility(request)
      .map { response =>
        response.status match {
          case 200 =>
            Logger.info(s"The person with the given UTR, ${request.utr} is eligible to use the SEISS service")
            true
          case 404 =>
            Logger.info(s"The person with the given UTR, ${request.utr} is not eligible to use the SEISS service")
            false
          case 400 =>
            Logger.warn(s"The UTR, ${request.utr} could not be read from the request body, or was not a valid UTR")
            true
          case 500 =>
            Logger.warn(s"Internal server error")
            true
          case _ =>
            true
        }
      }
      .recover {
        case ex =>
          Logger.error("Unknown problem when calling SelfEmployedIncomeSupportEligibilityConnector", ex)
          true
      }
  }
}
