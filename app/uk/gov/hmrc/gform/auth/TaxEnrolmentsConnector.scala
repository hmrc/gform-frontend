/*
 * Copyright 2021 HM Revenue & Customs
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
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ServiceId
import uk.gov.hmrc.gform.sharedmodel.taxenrolments.TaxEnrolmentsResponse
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

case class TaxEnrolment(identifiers: List[Identifier], verifiers: List[Verifier])
case class TaxEnrolmentPayload(verifiers: List[Verifier], `type`: String, userId: String, friendlyName: String)

object TaxEnrolmentPayload {
  implicit val format = Json.format[TaxEnrolmentPayload]
}

object TaxEnrolment {
  implicit val format = Json.format[TaxEnrolment]
}

class TaxEnrolmentsConnector(baseUrl: String, http: WSHttp) {

  private val logger = LoggerFactory.getLogger(getClass)

  def enrolGGUser(request: TaxEnrolment, service: ServiceId, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[ServiceCallResponse[TaxEnrolmentsResponse]] = {
    val groupId = retrievals.groupId
    val identifiers = request.identifiers.sortBy(_.key)

    val enrolmentKey = service.value + "~" + identifiers
      .map(identifier => identifier.key + "~" + identifier.value)
      .mkString("~")

    http
      .doPost(
        s"$baseUrl/tax-enrolments/groups/$groupId/enrolments/$enrolmentKey",
        TaxEnrolmentPayload(request.verifiers, "principal", retrievals.ggCredId, "gform-enrolment")
      )
      .map { httpResponse =>
        val status = httpResponse.status
        status match {
          case 201 =>
            logger.info(s"Calling tax enrolment returned $status: Success.")
            ServiceResponse(TaxEnrolmentsResponse.Success)
          case 400 =>
            logger.info(s"Calling tax enrolment returned $status: InvalidIdentifiers.")
            ServiceResponse(TaxEnrolmentsResponse.InvalidIdentifiers)
          case 403 =>
            logger.info(s"Calling tax enrolment returned $status: InvalidCredentials.")
            ServiceResponse(TaxEnrolmentsResponse.InvalidCredentials)
          case 409 =>
            logger.info(s"Calling tax enrolment returned $status: Conflict.")
            ServiceResponse(TaxEnrolmentsResponse.Conflict)
          case other =>
            logger.error(s"Problem when calling tax enrolment. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover {
        case ex =>
          logger.error("Unknown problem when calling tax enrolment", ex)
          CannotRetrieveResponse
      }

  }
}
