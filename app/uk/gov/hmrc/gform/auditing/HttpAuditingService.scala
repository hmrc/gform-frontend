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

package uk.gov.hmrc.gform.auditing

import play.api.mvc.RequestHeader

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.play.audit.http.connector.{ AuditConnector, AuditResult }

import scala.concurrent.Future
import scala.language.reflectiveCalls
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.HeaderCarrierConverter
import uk.gov.hmrc.play.bootstrap.config.HttpAuditEvent

/** This is ErrorAuditingSettings logic ported out of deprecated play GlobalSettings
  */
class HttpAuditingService(appName: String, auditConnector: AuditConnector)(implicit
  ec: ExecutionContext
) { self =>

  def auditServerError(requestHeader: RequestHeader): Future[AuditResult] = auditConnector.sendEvent(
    httpAuditEvent.dataEvent0(ServerInternalError, unexpectedError, requestHeader)
  )

  def auditNotFound(requestHeader: RequestHeader): Future[AuditResult] = auditConnector.sendEvent(
    httpAuditEvent.dataEvent0(ResourceNotFound, notFoundError, requestHeader)
  )

  def auditForbidden(requestHeader: RequestHeader): Future[AuditResult] = auditConnector.sendEvent(
    httpAuditEvent.dataEvent0(ResourceForbidden, resourceForbiddenError, requestHeader)
  )

  def auditBadRequest(requestHeaders: RequestHeader, error: String) = auditConnector.sendEvent(
    httpAuditEvent.dataEvent0(ServerValidationError, badRequestError, requestHeaders)
  )

  private val httpAuditEvent = new HttpAuditEvent {
    //function dataEvent is protected, we need to access it this is why it's exposed in such way
    def dataEvent0(eventType: String, transactionName: String, request: RequestHeader)(implicit
      hc: HeaderCarrier = HeaderCarrierConverter.fromHeadersAndSession(request.headers)
    ) =
      dataEvent(eventType, transactionName, request)
    override def appName = self.appName
  }
  private val unexpectedError = "Unexpected error"
  private val notFoundError = "Resource Endpoint Not Found"
  private val badRequestError = "Request bad format exception"
  private val resourceForbiddenError = "Resource Endpoint Forbidden"

  private val ResourceForbidden = "ResourceNotFound"
  private val ResourceNotFound = "ResourceNotFound"
  private val ServerInternalError = "ServerInternalError"
  private val ServerValidationError = "ServerValidationError"
}
