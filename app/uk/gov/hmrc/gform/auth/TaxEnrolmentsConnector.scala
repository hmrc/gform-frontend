/*
 * Copyright 2017 HM Revenue & Customs
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

import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ServiceId
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.{ ExecutionContext, Future }

case class TaxEnrolment(key: ServiceId, identifiers: List[Identifier], verifiers: List[Verifier])

object TaxEnrolment {
  implicit val format = Json.format[TaxEnrolment]
}

case class TaxEnrolmentRequest(enrolments: List[TaxEnrolment])

object TaxEnrolmentRequest {
  implicit val format = Json.format[TaxEnrolmentRequest]
}

class TaxEnrolmentsConnector(baseUrl: String, http: WSHttp) {

  // TODO: This tax-enrolments endpoint is not ready yet, once it is available, this connector will need to be
  //       updated to reflect the final design. For now, the GovernmentGatewayConnector is used for this operation.

  def enrolGGUser(request: TaxEnrolmentRequest, service: ServiceId)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[HttpResponse] =
    http.PUT(s"${baseUrl}/tax-enrolments/service/${service.value}/enrolment", request)
}

