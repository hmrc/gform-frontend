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

package uk.gov.hmrc.gform.controllers

import akka.http.scaladsl.model.StatusCodes.{ BadRequest, Forbidden, NotFound }
import play.api._
import play.api.http.DefaultHttpErrorHandler
import play.api.mvc.{ RequestHeader, Result }
import play.core.SourceMapper
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.http.{ BadRequestException, ForbiddenException, NotFoundException, UpstreamErrorResponse }
import uk.gov.hmrc.gform.playcomponents.RequestHeaderService
import org.slf4j.{ Logger, LoggerFactory }
import uk.gov.hmrc.gform.binders.IllegalBindException

class ErrorHandler(
  environment: Environment,
  configuration: Configuration,
  sourceMapper: Option[SourceMapper],
  errResponder: ErrResponder,
  requestHeaderService: RequestHeaderService
)(implicit ec: ExecutionContext)
    extends DefaultHttpErrorHandler(environment, configuration, sourceMapper, None) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)
  private val smartUpstreamLogger = SmartLogger.upstreamLogger
  private val smartLocalLogger = SmartLogger.localLogger

  override protected def onBadRequest(
    requestHeader: RequestHeader,
    message: String
  ): Future[Result] = errResponder.badRequest(requestHeader, message, None, smartLocalLogger)

  override protected def onNotFound(
    requestHeader: RequestHeader,
    message: String
  ): Future[Result] =
    errResponder.notFound(requestHeader, message, None, smartLocalLogger)

  override protected def onOtherClientError(
    requestHeader: RequestHeader,
    statusCode: Int,
    message: String
  ): Future[Result] = {
    val maybeFormTemplate: Future[Option[FormTemplate]] =
      requestHeaderService.formTemplateContext(requestHeader).map(_.map(_.formTemplate))

    maybeFormTemplate.flatMap(maybeFormTemplate =>
      errResponder.onOtherClientError(requestHeader, statusCode, message, maybeFormTemplate)
    )
  }

  override def onServerError(requestHeader: RequestHeader, exception: Throwable): Future[Result] = {

    val maybeFormTemplateF: Future[Option[FormTemplate]] =
      requestHeaderService
        .formTemplateContext(requestHeader)
        .map(_.map(_.formTemplate))
        .recoverWith { case _ => Future.successful(None) }

    maybeFormTemplateF.flatMap { maybeFormTemplate =>
      exception match {
        case e @ UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode == BadRequest.intValue =>
          errResponder.badRequest(requestHeader, e.message, maybeFormTemplate, smartUpstreamLogger)
        case e: BadRequestException =>
          errResponder.badRequest(requestHeader, e.message, maybeFormTemplate, smartLocalLogger)
        case e @ UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode == Forbidden.intValue =>
          errResponder.forbidden(e.message, maybeFormTemplate, None, smartUpstreamLogger)(requestHeader)
        case e: ForbiddenException =>
          errResponder.forbidden(e.message, maybeFormTemplate)(requestHeader)
        case e @ UpstreamErrorResponse.WithStatusCode(statusCode) if statusCode == NotFound.intValue =>
          errResponder.notFound(requestHeader, e.message, maybeFormTemplate, smartUpstreamLogger)
        case e: NotFoundException => errResponder.notFound(requestHeader, e.message, maybeFormTemplate)
        case e: IllegalBindException =>
          logger.info(s"URL: ${requestHeader.uri}, Query Params: ${requestHeader.queryString}")
          errResponder.internalServerError(requestHeader, maybeFormTemplate, e)
        case e => errResponder.internalServerError(requestHeader, maybeFormTemplate, e)
      }
    }
  }
}
