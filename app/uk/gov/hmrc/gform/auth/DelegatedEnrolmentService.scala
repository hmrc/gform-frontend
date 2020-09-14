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

import cats.instances.future._
import cats.syntax.applicative._

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.{ GovernmentGatewayId, IdentifierValue }
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, NotFound, ServiceCallResponse, ServiceResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataSource.DelegatedEnrolment
import uk.gov.hmrc.http.HeaderCarrier

class DelegatedEnrolmentService(
  enrolmentStoreProxyConnector: EnrolmentStoreProxyConnector
) {
  private def processDelegatedEnrolmentResponse(
    governmentGatewayId: GovernmentGatewayId,
    delegatedUserIds: ServiceCallResponse[DelegatedUserIds]
  )(
    implicit
    ec: ExecutionContext
  ): Future[Boolean] = delegatedUserIds match {
    case ServiceResponse(DelegatedUserIds(ids)) => ids.contains(governmentGatewayId.ggId).pure[Future]
    case NotFound | CannotRetrieveResponse      => Future.failed(new Exception("Call to enrolment-store-proxy has failed"))
  }

  def checkDelegatedEnrolment(
    governmentGatewayId: GovernmentGatewayId,
    delegatedEnrolment: DelegatedEnrolment,
    identifierValue: IdentifierValue,
    hc: HeaderCarrier
  )(
    implicit
    ec: ExecutionContext
  ): Future[Boolean] = {
    implicit val _hc = hc
    for {
      delegatedUserIds <- enrolmentStoreProxyConnector.hasDelegatedEnrolment(delegatedEnrolment, identifierValue)
      isDelegated      <- processDelegatedEnrolmentResponse(governmentGatewayId, delegatedUserIds)
    } yield isDelegated
  }
}
