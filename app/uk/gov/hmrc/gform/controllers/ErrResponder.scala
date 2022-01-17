/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform
package controllers

import org.slf4j.LoggerFactory
import play.api.i18n.{ I18nSupport, Langs, Messages, MessagesApi }
import play.api.mvc.Results.{ BadRequest, Forbidden, InternalServerError, NotFound }
import play.api.mvc.{ RequestHeader, Result }
import play.twirl.api.Html
import uk.gov.hmrc.gform.auditing.HttpAuditingService
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.sharedmodel.LangADT

import scala.concurrent.Future

/** This object suppose to render responses if something unexpected happened.
  * It as well does a bunch of side effects:
  *  - sends audit events
  *  - does logging so please use it just before returning
  *  - (TODO occurrenceId)
  */
class ErrResponder(
  frontendAppConfig: FrontendAppConfig,
  httpAuditingService: HttpAuditingService,
  i18nSupport: I18nSupport,
  langs: Langs
)(implicit
  messagesApi: MessagesApi
) {

  private val logger = LoggerFactory.getLogger(getClass)
  private val smartLocalLogger = SmartLogger.localLogger

  import i18nSupport._

  def internalServerError(requestHeader: RequestHeader, maybeFormTemplate: Option[FormTemplate], e: Throwable) = {
    val l: LangADT = LangADT.fromRequest(requestHeader, langs)
    logger.error(s"Experienced internal server error", e)
    httpAuditingService.auditServerError(requestHeader)
    Future.successful(InternalServerError.apply(renderInternalServerError(maybeFormTemplate)(requestHeader, l)))
  }

  def onOtherClientError(
    requestHeader: RequestHeader,
    statusCode: Int,
    message: String,
    maybeFormTemplate: Option[FormTemplate]
  ) = {
    val l: LangADT = LangADT.fromRequest(requestHeader, langs)
    logger.warn(s"Experienced internal server error, statusCode=$statusCode, message=$message")
    //no auditing
    Future.successful(InternalServerError.apply(renderInternalServerError(maybeFormTemplate)(requestHeader, l)))
  }

  def forbidden(
    message: String,
    maybeFormTemplate: Option[FormTemplate],
    messageHtml: Option[Html] = None,
    smartLogger: SmartLogger = smartLocalLogger
  )(implicit request: RequestHeader): Future[Result] = {
    implicit val l: LangADT = LangADT.fromRequest(request, langs)
    forbiddenReason(
      message,
      "generic.error.pageRestricted",
      smartLogger,
      maybeFormTemplate,
      messageHtml
    )
  }

  def forbiddenWithReason(
    reason: String,
    maybeFormTemplate: Option[FormTemplate],
    smartLogger: SmartLogger = smartLocalLogger
  )(implicit
    request: RequestHeader
  ): Future[Result] = {
    implicit val l: LangADT = LangADT.fromRequest(request, langs)
    forbiddenReason(reason, reason, smartLogger, maybeFormTemplate)
  }

  private def forbiddenReason(
    message: String,
    reason: String,
    smartLogger: SmartLogger,
    maybeFormTemplate: Option[FormTemplate],
    reasonHtml: Option[Html] = None
  )(implicit
    request: RequestHeader,
    messages: Messages,
    l: LangADT
  ): Future[Result] = {
    smartLogger.log(s"Trying to access forbidden resource: $message")
    httpAuditingService.auditForbidden(request)
    Future.successful(
      Forbidden(
        renderErrorPage(
          messages("generic.error.accessForbidden"),
          messages("generic.error.pageRestricted"),
          messages(reason),
          maybeFormTemplate,
          reasonHtml
        )
      )
    )
  }

  def badRequest(
    requestHeader: RequestHeader,
    message: String,
    maybeFormTemplate: Option[FormTemplate],
    smartLogger: SmartLogger = smartLocalLogger
  ): Future[Result] = {
    val l: LangADT = LangADT.fromRequest(requestHeader, langs)
    smartLogger.log(s"Bad request: $message")
    httpAuditingService.auditBadRequest(requestHeader, message)
    Future.successful(BadRequest(renderBadRequest(maybeFormTemplate)(requestHeader, l)))
  }

  def notFound(
    requestHeader: RequestHeader,
    message: String,
    maybeFormTemplate: Option[FormTemplate],
    smartLogger: SmartLogger = smartLocalLogger
  ): Future[Result] = {
    val l: LangADT = LangADT.fromRequest(requestHeader, langs)
    smartLogger.log(s"Page NotFound: $message")
    httpAuditingService.auditNotFound(requestHeader)
    Future.successful(NotFound(renderNotFound(maybeFormTemplate)(requestHeader, l)))
  }

  private def renderInternalServerError(
    maybeFormTemplate: Option[FormTemplate]
  )(implicit request: RequestHeader, l: LangADT) =
    renderErrorPage(
      pageTitle =
        maybeFormTemplate.map(_.formName.value).getOrElse(Messages("global.error.InternalServerError500.title")),
      heading = Messages("global.error.InternalServerError500.heading"),
      message = Messages("global.error.InternalServerError500.message"),
      maybeFormTemplate = maybeFormTemplate
    )

  private def renderNotFound(maybeFormTemplate: Option[FormTemplate])(implicit request: RequestHeader, lang: LangADT) =
    renderErrorPage(
      pageTitle = Messages("global.error.pageNotFound404.title"),
      heading = Messages("global.error.pageNotFound404.heading"),
      message = Messages("global.error.pageNotFound404.message"),
      maybeFormTemplate = maybeFormTemplate
    )

  private def renderBadRequest(
    maybeFormTemplate: Option[FormTemplate]
  )(implicit request: RequestHeader, lang: LangADT) =
    renderErrorPage(
      pageTitle = Messages("global.error.badRequest400.title"),
      heading = Messages("global.error.badRequest400.heading"),
      message = Messages("global.error.badRequest400.message"),
      maybeFormTemplate = maybeFormTemplate
    )

  private def renderErrorPage(
    pageTitle: String,
    heading: String,
    message: String,
    maybeFormTemplate: Option[FormTemplate],
    maybeMessageHtml: Option[Html] = None
  )(implicit requestHeader: RequestHeader, lang: LangADT) =
    views.html.error_template(
      pageTitle,
      heading,
      message,
      maybeFormTemplate,
      maybeMessageHtml,
      frontendAppConfig
    )
}
