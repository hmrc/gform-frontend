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

package uk.gov.hmrc.gform.auth.models

import org.apache.commons.codec.net.URLCodec
import org.slf4j.LoggerFactory
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.gform.routes

object RecoverAuthResult {

  private val logger = LoggerFactory.getLogger(getClass)

  val noop: PartialFunction[Throwable, AuthResult] = PartialFunction.empty

  def redirectToEnrolmentSection(authRedirect: AuthRedirect): PartialFunction[Throwable, AuthResult] = {
    case _: InsufficientEnrolments =>
      logger.debug("Enrolment required")
      authRedirect
  }

  def rejectInsufficientEnrolments(formTemplateId: FormTemplateId): PartialFunction[Throwable, AuthResult] = {
    case _: InsufficientEnrolments =>
      logger.debug("Auth Failed")
      AuthRedirectFlashingFormName(
        uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments(formTemplateId).url
      )
  }

  def basicRecover(request: Request[AnyContent], appConfig: AppConfig): PartialFunction[Throwable, AuthResult] =
    recoverNoActiveSession(request, appConfig) orElse logAndRethrow

  private def recoverNoActiveSession(
    request: Request[AnyContent],
    appConfig: AppConfig
  ): PartialFunction[Throwable, AuthResult] = { case _: NoActiveSession =>
    val maybeFormTemplateWithRedirect = request.attrs.get(FormTemplateKey)
    val uri = maybeFormTemplateWithRedirect.fold(request.uri) { formTemplateWithRedirect =>
      val formTemplate = formTemplateWithRedirect.formTemplate
      routes.NewFormController.dashboard(formTemplate._id).url
    }
    logger.info("No Active Session. Redirecting user to: " + uri)
    val codec = new URLCodec("UTF-8")
    val continueUrl = codec.encode(appConfig.`gform-frontend-base-url` + uri)
    val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
    val url = s"$ggLoginUrl?continue=$continueUrl"
    AuthRedirectFlashingFormName(url)
  }

  val logAndRethrow: PartialFunction[Throwable, AuthResult] = { case otherException =>
    logger.debug(s"Exception thrown on authorization with message : ${otherException.getMessage}")
    throw otherException
  }
}
