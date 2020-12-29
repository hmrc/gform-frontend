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

import org.slf4j.LoggerFactory

import java.net.URLEncoder
import play.api._
import play.api.mvc.Results.Redirect
import play.api.mvc.{ RequestHeader, Result }
import play.core.SourceMapper

import scala.concurrent.Future
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.gform.routes
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

class CSRFErrorHandler(
  environment: Environment,
  configuration: Configuration,
  sourceMapper: Option[SourceMapper],
  errResponder: ErrResponder,
  appConfig: AppConfig
) extends ErrorHandler(environment, configuration, sourceMapper, errResponder) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
  private val gformBaseUrl = appConfig.`gform-frontend-base-url`

  /* When user is signed-out/logged (ie. session is invalidated by some other service) it will
   * cause CSRF token error for POST request.
   * This handler will handle that situation gracefully, redirecting user to login page.
   */
  override protected def onForbidden(requestHeader: RequestHeader, message: String): Future[Result] = {
    val maybeFormTemplateId: Option[FormTemplateId] =
      requestHeader.cookies.get(CookieNames.formTemplateIdCookieName).map(cookie => FormTemplateId(cookie.value))

    val loginUrl = maybeFormTemplateId.fold(ggLoginUrl) { formTemplateId =>
      val dashboardUrl = routes.NewFormController.dashboard(formTemplateId).url
      val continueUrl = URLEncoder.encode(gformBaseUrl + dashboardUrl, "UTF-8")
      s"$ggLoginUrl?continue=$continueUrl"
    }

    logger.info(s"CSRF token problem for request.uri: ${requestHeader.uri}. Redirecting user to: $loginUrl")

    Future.successful(Redirect(loginUrl))
  }
}
