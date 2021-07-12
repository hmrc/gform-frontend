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

package uk.gov.hmrc.gform.controllers

import akka.http.scaladsl.model.StatusCodes.{ BadRequest, Forbidden, NotFound }
import play.api._
import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.{ RequestHeader, Result }
import play.core.SourceMapper

import scala.concurrent.Future
import uk.gov.hmrc.http.{ BadRequestException, ForbiddenException, NotFoundException, UpstreamErrorResponse }

class ErrorHandler(
  environment: Environment,
  configuration: Configuration,
  sourceMapper: Option[SourceMapper],
  errResponder: ErrResponder
) extends DefaultHttpErrorHandler(environment, configuration, sourceMapper, None) {

  private val smartUpstreamLogger = SmartLogger.upstreamLogger

  override protected def onOtherClientError(
    requestHeader: RequestHeader,
    statusCode: Int,
    message: String
  ): Future[Result] = errResponder.onOtherClientError(requestHeader, statusCode, message)

  override def onServerError(requestHeader: RequestHeader, exception: Throwable): Future[Result] = exception match {
    case UpstreamErrorResponse.WithStatusCode(statusCode, e) if statusCode == BadRequest.intValue =>
      errResponder.badRequest(requestHeader, e.message, smartUpstreamLogger)
    case e: BadRequestException =>
      errResponder.badRequest(requestHeader, e.message)
    //    case e: UnauthorizedException => TODO redirect to login page
    case UpstreamErrorResponse.WithStatusCode(statusCode, e) if statusCode == Forbidden.intValue =>
      errResponder.forbidden(e.message, None, None, smartUpstreamLogger)(requestHeader)
    case e: ForbiddenException =>
      errResponder.forbidden(e.message)(requestHeader)
    case UpstreamErrorResponse.WithStatusCode(statusCode, e) if statusCode == NotFound.intValue =>
      errResponder.notFound(requestHeader, e.message, smartUpstreamLogger)
    case e: NotFoundException => errResponder.notFound(requestHeader, e.message)
    case e                    => errResponder.internalServerError(requestHeader, e)
  }
}
