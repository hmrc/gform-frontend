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

package uk.gov.hmrc.gform.playcomponents

import play.api._
import play.api.http.DefaultHttpErrorHandler
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc.Results._
import play.api.mvc.{ RequestHeader, Result }
import play.core.SourceMapper
import uk.gov.hmrc.gform.auditing.HttpAuditingService
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.play.audit.http.connector.AuditConnector

import scala.concurrent.Future

class ErrorHandler(
    frontendAppConfig: FrontendAppConfig,
    auditConnector: AuditConnector,
    httpAuditingService: HttpAuditingService,
    environment: Environment,
    configuration: Configuration,
    sourceMapper: Option[SourceMapper],
    i18nSupport: I18nSupport
) extends DefaultHttpErrorHandler(environment, configuration, sourceMapper, None) //    with JsonErrorHandling //    with ErrorAuditingSettings // TODO: auditConnector.sendEvent(dataEvent(code, unexpectedError, request)
{

  import i18nSupport._

  override protected def onBadRequest(requestHeader: RequestHeader, message: String): Future[Result] = {
    Logger.logger.info(s"Bad request: $message")
    httpAuditingService.auditBadRequest(requestHeader, message)
    Future.successful(
      BadRequest(renderBadRequest(requestHeader))
    )
  }

  override protected def onForbidden(requestHeader: RequestHeader, message: String): Future[Result] = {
    Logger.logger.info(s"Forbidden: $message")
    //no auditing here (originally we didn't audited forbidden requests, leaving as it was)
    Future.successful(
      Forbidden(renderErrorPage(
        "Access forbidden",
        "We're sorry, but this page is restricted.",
        message,
        requestHeader
      ))
    )
  }

  override protected def onNotFound(requestHeader: RequestHeader, message: String): Future[Result] = {
    Logger.logger.info(s"NotFound: $message")
    httpAuditingService.auditNotFound(requestHeader)
    Future.successful(
      NotFound(renderNotFound(requestHeader))
    )
  }

  override protected def onOtherClientError(requestHeader: RequestHeader, statusCode: Int, message: String): Future[Result] = {
    Logger.logger.error(s"onOtherClientError: $message, $statusCode")
    //no auditing here (don't know what to audit here. Originally there was no such situation)
    Future.successful(
      Status(statusCode).apply(renderInternalServerError(requestHeader))
    )
  }

  override def onServerError(requestHeader: RequestHeader, exception: Throwable): Future[Result] = {
    Logger.logger.error(s"onServerError", exception)
    httpAuditingService.auditError(requestHeader, exception)
    Future.successful(
      InternalServerError.apply(renderInternalServerError(requestHeader))
    )
  }

  private def renderErrorPage(
    pageTitle: String,
    heading: String,
    message: String,
    request: RequestHeader
  ) = {
    uk.gov.hmrc.gform.views.html.error_template(pageTitle, heading, message, frontendAppConfig)
  }

  private def renderBadRequest(request: RequestHeader) = uk.gov.hmrc.gform.views.html.error_template(
    pageTitle = Messages("global.error.badRequest400.title"),
    heading = Messages("global.error.badRequest400.heading"),
    message = Messages("global.error.badRequest400.message"),
    frontendAppConfig = frontendAppConfig
  )

  private def renderNotFound(request: RequestHeader) = uk.gov.hmrc.gform.views.html.error_template(
    pageTitle = Messages("global.error.pageNotFound404.title"),
    heading = Messages("global.error.pageNotFound404.heading"),
    message = Messages("global.error.pageNotFound404.message"),
    frontendAppConfig = frontendAppConfig
  )

  private def renderInternalServerError(request: RequestHeader) = uk.gov.hmrc.gform.views.html.error_template(
    pageTitle = Messages("global.error.InternalServerError500.title"),
    heading = Messages("global.error.InternalServerError500.heading"),
    message = Messages("global.error.InternalServerError500.message"),
    frontendAppConfig = frontendAppConfig
  )

}

