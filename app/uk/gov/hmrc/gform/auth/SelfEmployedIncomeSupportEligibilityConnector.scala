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

package uk.gov.hmrc.gform.auth

import org.slf4j.LoggerFactory
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw

import scala.concurrent.{ ExecutionContext, Future }

case class UtrEligibilityRequest(utr: String) extends AnyVal

object UtrEligibilityRequest {
  implicit val format: OFormat[UtrEligibilityRequest] = Json.format[UtrEligibilityRequest]
}

class SelfEmployedIncomeSupportEligibilityConnector(baseUrl: String, http: WSHttp) {

  private val logger = LoggerFactory.getLogger(getClass)

  private def getUtrEligibility(
    request: UtrEligibilityRequest
  )(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    http.POST[UtrEligibilityRequest, HttpResponse](
      s"$baseUrl/self-employed-income-support-eligibility/utr-eligibility",
      request
    )

  def eligibilityStatus(request: UtrEligibilityRequest, hc: HeaderCarrier)(implicit
    ec: ExecutionContext
  ): Future[Boolean] =
    request match {
      case r if r.utr.trim.nonEmpty =>
        getUtrEligibility(r)(hc, ec)
          .flatMap { response =>
            response.status match {
              case 200 =>
                logger.info(s"The person with the given UTR is eligible to use the SEISS service")
                Future.successful(true)
              case 404 =>
                logger.info(s"The person with the given UTR is not eligible to use the SEISS service")
                Future.successful(false)
              case 400 =>
                Future.failed(
                  new BadRequestException(
                    s"SEISS response description : The UTR could not be read from the request body, or was not a valid UTR"
                  )
                )
              case 500 =>
                Future.failed(
                  new InternalServerException(
                    s"SEISS response description : Something went wrong processing your request - it was our fault"
                  )
                )
              case _ =>
                Future.failed(new Exception(s"Unexpected SEISS response"))
            }
          }

      case _ =>
        logger.warn(s"An empty UTR is invalid.")
        Future.successful(false)
    }
}
