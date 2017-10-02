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

package uk.gov.hmrc.gform.auditing

import play.api.mvc.RequestHeader
import uk.gov.hmrc.play.audit.EventTypes.{ ResourceNotFound, ServerInternalError, ServerValidationError, TransactionFailureReason }
import uk.gov.hmrc.play.audit.http.HttpAuditEvent
import uk.gov.hmrc.play.audit.http.connector.{ AuditConnector, AuditResult }
import uk.gov.hmrc.play.http.{ HeaderCarrier, JsValidationException, NotFoundException }

import scala.concurrent.Future
import scala.language.reflectiveCalls

/**
 * This is ErrorAuditingSettings logic ported out of deprecated play GlobalSettings
 */
class HttpAuditingService(appName: String, auditConnector: AuditConnector) { self =>
  import scala.concurrent.ExecutionContext.Implicits.global

  def auditError(requestHeader: RequestHeader, ex: Throwable): Future[AuditResult] = {
    val code = ex match {
      case e: NotFoundException => ResourceNotFound
      case jsError: JsValidationException => ServerValidationError
      case _ => ServerInternalError
    }

    val dataEvent = httpAuditEvent.dataEvent0(code, unexpectedError, requestHeader).withDetail((TransactionFailureReason, ex.getMessage))
    auditConnector.sendEvent(dataEvent)
  }

  def auditNotFound(requestHeader: RequestHeader): Future[AuditResult] = auditConnector.sendEvent(
    httpAuditEvent.dataEvent0(ResourceNotFound, notFoundError, requestHeader)
  )

  def auditBadRequest(requestHeaders: RequestHeader, error: String) = auditConnector.sendEvent(
    httpAuditEvent.dataEvent0(ServerValidationError, badRequestError, requestHeaders)
  )

  private val httpAuditEvent = new HttpAuditEvent {
    //function dataEvent is protected, we need to access it this is why it's exposed in such way
    def dataEvent0(eventType: String, transactionName: String, request: RequestHeader)(implicit hc: HeaderCarrier = HeaderCarrier.fromHeadersAndSession(request.headers)) =
      dataEvent(eventType, transactionName, request)
    override def appName = self.appName
  }
  private val unexpectedError = "Unexpected error"
  private val notFoundError = "Resource Endpoint Not Found"
  private val badRequestError = "Request bad format exception"

}
