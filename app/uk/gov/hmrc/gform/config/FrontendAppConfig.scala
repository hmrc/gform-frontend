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

package uk.gov.hmrc.gform.config

import cats.data.NonEmptyList
import org.typelevel.ci.CIString
import play.api.i18n.{ Lang, Messages }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, AuthConfig, EmailAuthConfig, FormTemplateId }
import uk.gov.hmrc.hmrcfrontend.views.html.helpers.hmrcTrackingConsentSnippet
import uk.gov.hmrc.hmrcfrontend.views.viewmodels.timeoutdialog.TimeoutDialog

case class FrontendAppConfig(
  albAdminIssuerUrl: String,
  reportAProblemPartialUrl: String,
  reportAProblemNonJSUrl: String,
  governmentGatewaySignInUrl: String,
  gformFrontendBaseUrl: String,
  betaFeedbackUrlNoAuth: String,
  signOutUrl: String,
  footerCookiesUrl: String,
  footerPrivacyPolicyUrl: String,
  footerTermsConditionsUrl: String,
  footerHelpUrl: String,
  footerAccessibilityStatementUrl: String,
  authModule: AuthModule,
  availableLanguages: Map[String, Lang],
  routeToSwitchLanguage: String => play.api.mvc.Call,
  contactFormServiceIdentifier: String,
  optimizelyUrl: Option[String],
  trackingConsentSnippet: hmrcTrackingConsentSnippet,
  emailAuthStaticCodeEmails: Option[NonEmptyList[CIString]]
) {

  def jsConfig(authConfig: Option[AuthConfig]): JSConfig = authConfig match {
    case Some(Anonymous)                   => authModule.anonymous
    case Some(EmailAuthConfig(_, _, _, _)) => authModule.email
    case Some(_)                           => authModule.hmrc
    case None                              => JSConfig(timeoutEnabled = false, 0, 0, "", "")
  }

  def timeoutDialog(templateId: FormTemplateId, authConfig: Option[AuthConfig])(implicit
    messages: Messages
  ): Option[TimeoutDialog] = {
    val authTimeout = jsConfig(authConfig)
    if (authTimeout.timeoutEnabled) {
      Some(
        TimeoutDialog(
          timeout = Some(authTimeout.timeout),
          countdown = Some(authTimeout.countdown),
          keepAliveUrl = Some(authTimeout.keepAliveUrl),
          keepAliveButtonText = Some(messages("timeout.dialog.keepAliveButton")),
          signOutUrl = Some(authTimeout.signOutUrl + "/" + templateId.value),
          signOutButtonText = Some(messages("timeout.dialog.signOutButton")),
          title = Some(messages("timeout.dialog.title")),
          message = Some(messages("timeout.dialog.message"))
        )
      )
    } else {
      None
    }
  }

}
