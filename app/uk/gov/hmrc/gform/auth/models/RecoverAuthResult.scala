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

package uk.gov.hmrc.gform.auth.models

import play.api.Logger
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate

object RecoverAuthResult {

  val noop: PartialFunction[Throwable, AuthResult] = PartialFunction.empty

  def redirectToEnrolmentSection(authRedirect: AuthRedirect): PartialFunction[Throwable, AuthResult] = {
    case _: InsufficientEnrolments =>
      Logger.debug("Enrolment required")
      authRedirect
  }

  val rejectInsufficientEnrolments: PartialFunction[Throwable, AuthResult] = {
    case _: InsufficientEnrolments =>
      Logger.debug("Auth Failed")
      AuthRedirectFlashingFormName(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments().url)
  }

  def basicRecover(request: Request[AnyContent], appConfig: AppConfig): PartialFunction[Throwable, AuthResult] =
    recoverNoActiveSession(request, appConfig) orElse logAndRethrow

  def recoverNoActiveSession(
    request: Request[AnyContent],
    appConfig: AppConfig
  ): PartialFunction[Throwable, AuthResult] = {
    case _: NoActiveSession =>
      Logger.debug("No Active Session")
      val continueUrl = java.net.URLEncoder.encode(appConfig.`gform-frontend-base-url` + request.uri, "UTF-8")
      val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
      val url = s"$ggLoginUrl?continue=$continueUrl"
      AuthRedirectFlashingFormName(url)
  }

  val logAndRethrow: PartialFunction[Throwable, AuthResult] = {
    case otherException =>
      Logger.debug(s"Exception thrown on authorization with message : ${otherException.getMessage}")
      throw otherException
  }
}
