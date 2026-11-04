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

import cats.implicits.catsSyntaxEq
import org.slf4j.LoggerFactory
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ServiceId
import uk.gov.hmrc.gform.sharedmodel.taxenrolments.TaxEnrolmentsResponse
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import uk.gov.hmrc.http.HttpReads.Implicits.readRaw
import uk.gov.hmrc.http.client.HttpClientV2

import scala.concurrent.{ ExecutionContext, Future }

case class TaxEnrolment(identifiers: List[Identifier], verifiers: List[Verifier])
case class TaxEnrolmentPayload(verifiers: List[Verifier], `type`: String, userId: String, friendlyName: String)
case class TaxEnrolmentErrorResponse(code: String, message: String, errors: Option[List[TaxEnrolmentErrorResponse]])

object TaxEnrolmentPayload {
  implicit val format: OFormat[TaxEnrolmentPayload] = Json.format[TaxEnrolmentPayload]
}

object TaxEnrolment {
  implicit val format: OFormat[TaxEnrolment] = Json.format[TaxEnrolment]
}

object TaxEnrolmentErrorResponse {
  implicit val format: OFormat[TaxEnrolmentErrorResponse] = Json.format[TaxEnrolmentErrorResponse]
}

class TaxEnrolmentsConnector(baseUrl: String, httpClient: HttpClientV2) {

  private val logger = LoggerFactory.getLogger(getClass)

  def enrolGGUser(request: TaxEnrolment, service: ServiceId, retrievals: MaterialisedRetrievals)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ): Future[ServiceCallResponse[TaxEnrolmentsResponse]] = {
    val groupId = retrievals.groupId
    val identifiers = request.identifiers.sortBy(_.key)

    val enrolmentKey = service.value + "~" + identifiers
      .map(identifier => identifier.key + "~" + identifier.value)
      .mkString("~")

    httpClient
      .post(url"$baseUrl/tax-enrolments/groups/$groupId/enrolments/$enrolmentKey")
      .withBody(
        Json.toJson(TaxEnrolmentPayload(request.verifiers, "principal", retrievals.ggCredId, "gform-enrolment"))
      )
      .execute[HttpResponse]
      .map { httpResponse =>
        val status = httpResponse.status
        val body = httpResponse.body
        val errorCodes = Json
          .parse(body)
          .validate[TaxEnrolmentErrorResponse]
          .fold(
            _ => {
              if (!body.isBlank && status =!= 201) {
                logger.error(
                  s"Calling tax enrolment; problem parsing non-blank error response. Http status: $status, body: $body"
                )
              }
              Option.empty[List[String]]
            },
            valid => Some(List(valid.code) ++ valid.errors.map(_.map(_.code)).getOrElse(Nil))
          )
          .map(_.mkString(", "))
          .fold("")(codes => s"; error code(s): $codes")

        status match {
          case 201 =>
            logger.info(s"Calling tax enrolment returned $status: Success.")
            ServiceResponse(TaxEnrolmentsResponse.Success)
          case 400 =>
            logger.info(s"Calling tax enrolment returned $status: InvalidIdentifiers$errorCodes")
            ServiceResponse(TaxEnrolmentsResponse.InvalidIdentifiers)
          case 403 =>
            logger.info(s"Calling tax enrolment returned $status: InvalidCredentials$errorCodes")
            ServiceResponse(TaxEnrolmentsResponse.InvalidCredentials)
          case 409 =>
            logger.info(s"Calling tax enrolment returned $status: Conflict$errorCodes")
            ServiceResponse(TaxEnrolmentsResponse.Conflict)
          case other =>
            logger.error(s"Problem when calling tax enrolment. Http status: $other, body: $body")
            CannotRetrieveResponse
        }
      }
      .recover { case ex =>
        logger.error("Unknown problem when calling tax enrolment", ex)
        CannotRetrieveResponse
      }

  }
}
