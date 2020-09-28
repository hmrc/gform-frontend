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

import julienrf.json.derived
import play.api.Logger
import play.api.libs.json.Format
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.IdentifierValue
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.DelegatedEnrolment
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

case class DelegatedUserIds(delegatedUserIds: List[String])

object DelegatedUserIds {

  val empty = DelegatedUserIds(Nil)
  implicit val format: Format[DelegatedUserIds] = derived.oformat()
}

class EnrolmentStoreProxyConnector(baseUrl: String, http: WSHttp) {
  def hasDelegatedEnrolment(delegatedEnrolment: DelegatedEnrolment, identifierValue: IdentifierValue)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext): Future[ServiceCallResponse[DelegatedUserIds]] = {

    val delegatedEnrolmentKey = delegatedEnrolment.serviceName.value :: delegatedEnrolment.identifierName.value :: identifierValue.value :: Nil mkString ("~")

    val url = s"$baseUrl/enrolment-store-proxy/enrolment-store/enrolments/$delegatedEnrolmentKey/users?type=delegated"

    http
      .doGet(url)
      .map { httpResponse =>
        val status = httpResponse.status
        status match {
          case 200 =>
            Logger.info(s"Calling enrolment store proxy returned $status: Success.")
            ServiceResponse(httpResponse.json.asOpt[DelegatedUserIds].getOrElse(DelegatedUserIds.empty))
          case 204 =>
            ServiceResponse(DelegatedUserIds.empty)
          case 400 =>
            Logger.info(s"Calling enrolment store proxy returned $status. Response: ${httpResponse.body}")
            CannotRetrieveResponse
          case other =>
            Logger.error(s"Problem when calling enrolment store proxy. Http status: $other, body: ${httpResponse.body}")
            CannotRetrieveResponse
        }
      }
      .recover {
        case ex =>
          Logger.error("Unknown problem when calling enrolment store proxy", ex)
          CannotRetrieveResponse
      }
  }
}
