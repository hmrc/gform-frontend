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
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

sealed trait AuthResult
final case class AuthSuccessful(retrievals: MaterialisedRetrievals) extends AuthResult
final case class AuthRedirect(loginUrl: String, flashing: Seq[(String, String)] = Seq.empty) extends AuthResult
final case class AuthRedirectFlashingFormName(loginUrl: String) extends AuthResult
final case class AuthBlocked(message: String) extends AuthResult
final case class AuthForbidden(message: String) extends AuthResult

object AuthResult {
  implicit object hasAuthResult extends HasAuthResult[AuthResult] {

    def getAuthResult(x: AuthResult): AuthResult = x

    def errorHandler(
      request: Request[AnyContent],
      authConfig: AuthConfig,
      appConfig: AppConfig,
      formTemplate: FormTemplate): PartialFunction[Throwable, AuthResult] = {
      case _: InsufficientEnrolments =>
        authConfig match {
          case _: AuthConfigWithEnrolment =>
            Logger.debug("Enrolment required")
            AuthRedirect(uk.gov.hmrc.gform.gform.routes.EnrolmentController.showEnrolment(formTemplate._id, None).url)
          case _ =>
            Logger.debug("Auth Failed")
            AuthRedirectFlashingFormName(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments().url)
        }
      case _: NoActiveSession =>
        Logger.debug("No Active Session")
        val continueUrl = java.net.URLEncoder.encode(appConfig.`gform-frontend-base-url` + request.uri, "UTF-8")
        val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
        val url = s"$ggLoginUrl?continue=$continueUrl"
        AuthRedirectFlashingFormName(url)
      case otherException =>
        Logger.debug(s"Exception thrown on authorization with message : ${otherException.getMessage}")
        throw otherException
    }
  }
}
