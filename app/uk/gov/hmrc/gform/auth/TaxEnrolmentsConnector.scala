/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.formtemplate.ServiceId
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

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
  def enrolGGUser(request: TaxEnrolment, service: ServiceId, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[HttpResponse] = {
    val groupId = retrievals.groupId
    val identifiers = request.identifiers.sortBy(_.key)

    val enrolmentKey = service.value + "~" + identifiers
      .map(identifier => identifier.key + "~" + identifier.value)
      .mkString("~")

    http
      .POST(
        s"$baseUrl/tax-enrolments/groups/$groupId/enrolments/$enrolmentKey",
        TaxEnrolmentPayload(request.verifiers, "principal", retrievals.ggCredId, "gform-enrolment")
      )
  }
}
