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

package uk.gov.hmrc.gform.gform

import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }

import scala.concurrent.Future
import uk.gov.hmrc.gform.config.FrontendAppConfig
import play.api.i18n.I18nSupport
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, EmailAuthDetails }
import uk.gov.hmrc.gform.controllers.GformSessionKeys.{ COMPOSITE_AUTH_DETAILS_SESSION_KEY, EMAIL_AUTH_DETAILS_SESSION_KEY }
import uk.gov.hmrc.gform.controllers.NonAuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.gform.MaskUtil.maskEmail
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthConfig.hmrcSimpleModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, FormTemplateId }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ signed_out, signed_out_email_auth }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class SignOutController(
  frontendConfig: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents,
  nonAuth: NonAuthenticatedRequestActionsAlgebra[Future]
) extends FrontendController(messagesControllerComponents) with I18nSupport {

  def signOut(formTemplateId: FormTemplateId): Action[AnyContent] = nonAuth { request => l =>
    val formTemplateWithRedirects = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateWithRedirects.formTemplate
    val redirect = Redirect(routes.SignOutController.showSignedOutPage(formTemplateId)).withNewSession

    val config: Option[AuthConfig] = formTemplate.authConfig match {
      case Composite(configs) =>
        val compositeAuthDetails =
          jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
            .get(formTemplateWithRedirects)
        AuthConfig
          .getAuthConfig(compositeAuthDetails.getOrElse(hmrcSimpleModule), configs)
      case config => Some(config)
    }

    config match {
      case Some(c) if c.isEmailAuthConfig =>
        val emailAuthDetails: EmailAuthDetails =
          jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)

        emailAuthDetails.get(formTemplateWithRedirects).fold(redirect) { emailAuthData =>
          redirect
            .flashing("maskedEmailId" -> maskEmail(emailAuthData.email.toString))
        }
      case _ =>
        redirect
    }
  }

  def showSignedOutPage(formTemplateId: FormTemplateId): Action[AnyContent] = nonAuth {
    implicit request => implicit l =>
      val signBackInUrl = routes.NewFormController.dashboard(formTemplateId).url
      val maskedEmailId = request.flash.get("maskedEmailId")
      maskedEmailId match {
        case Some(id) =>
          Ok(signed_out_email_auth(signBackInUrl, id, frontendConfig))
        case _ =>
          Ok(signed_out(signBackInUrl, frontendConfig))
      }
  }
}
