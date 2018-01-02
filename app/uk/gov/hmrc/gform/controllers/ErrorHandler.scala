/*
 * Copyright 2018 HM Revenue & Customs
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

import play.api._
import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.{ RequestHeader, Result }
import play.core.SourceMapper
import uk.gov.hmrc.play.http._

import scala.concurrent.Future
import uk.gov.hmrc.http.{ BadRequestException, ForbiddenException, NotFoundException }

class ErrorHandler(
    environment: Environment,
    configuration: Configuration,
    sourceMapper: Option[SourceMapper],
    errResponder: ErrResponder
) extends DefaultHttpErrorHandler(environment, configuration, sourceMapper, None) {

  override protected def onBadRequest(requestHeader: RequestHeader, message: String): Future[Result] = errResponder.badRequest(requestHeader, message)

  override protected def onForbidden(requestHeader: RequestHeader, message: String): Future[Result] = errResponder.forbidden(requestHeader, message)

  override protected def onNotFound(requestHeader: RequestHeader, message: String): Future[Result] = errResponder.notFound(requestHeader, message)

  override protected def onOtherClientError(requestHeader: RequestHeader, statusCode: Int, message: String): Future[Result] = errResponder.onOtherClientError(requestHeader, statusCode, message)

  override def onServerError(requestHeader: RequestHeader, exception: Throwable): Future[Result] = exception match {
    case e: BadRequestException => onBadRequest(requestHeader, e.message)
    //    case e: UnauthorizedException => TODO redirect to login page
    case e: ForbiddenException => errResponder.forbidden(requestHeader, e.message)
    case e: NotFoundException => errResponder.notFound(requestHeader, e.message)
    case e => errResponder.internalServerError(requestHeader, e)
  }

}

