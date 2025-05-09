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

package uk.gov.hmrc.gform.gform

import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }

import scala.concurrent.{ ExecutionContext, Future }
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
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.OperationWithForm
import uk.gov.hmrc.http.HeaderCarrier

class SignOutController(
  frontendConfig: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents,
  nonAuth: NonAuthenticatedRequestActionsAlgebra[Future],
  auth: AuthenticatedRequestActionsAlgebra[Future],
  auditService: AuditService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) with I18nSupport {

  def signOut(formTemplateId: FormTemplateId): Action[AnyContent] = nonAuth { request => _ =>
    val formTemplateContext = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateContext.formTemplate
    val redirect = Redirect(
      frontendConfig.getBasGatewayFrontendSignOutUrl(
        Option(routes.SignOutController.showSignedOutPage(formTemplate._id).url)
      )
    )

    val config: Option[AuthConfig] = formTemplate.authConfig match {
      case Composite(configs) =>
        val compositeAuthDetails =
          jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
            .get(formTemplateContext)
        AuthConfig
          .getAuthConfig(compositeAuthDetails.getOrElse(hmrcSimpleModule), configs)
      case config => Some(config)
    }
    sendSignOutEvent(formTemplateId)(request)
    config match {
      case Some(c) if c.isEmailAuthConfig =>
        val emailAuthDetails: EmailAuthDetails =
          jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)

        emailAuthDetails.get(formTemplateContext).fold(redirect) { emailAuthData =>
          redirect
            .flashing("maskedEmailId" -> maskEmail(emailAuthData.email.toString))
        }
      case _ =>
        redirect
    }
  }

  private def sendSignOutEvent(formTemplateId: FormTemplateId): Action[AnyContent] = {
    implicit val hc: HeaderCarrier = HeaderCarrier()
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, OperationWithForm.AuditSessionEnd) {
      _ => _ => cache => _ => formModelOptics =>
        auditService.sendFormSignOut(cache.form, cache.retrievals)
        Future.successful(Ok("success"))
    }
  }

  def showSignedOutPage(formTemplateId: FormTemplateId): Action[AnyContent] = nonAuth {
    implicit request => implicit l =>
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      val signBackInUrl = routes.NewFormController.dashboard(formTemplateId).url
      val maskedEmailId = request.flash.get("maskedEmailId")
      maskedEmailId match {
        case Some(id) =>
          Ok(signed_out_email_auth(formTemplate, signBackInUrl, id, frontendConfig))
        case _ =>
          Ok(signed_out(formTemplate, signBackInUrl, frontendConfig))
      }
  }
}
