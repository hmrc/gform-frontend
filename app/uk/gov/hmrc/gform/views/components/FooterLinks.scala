/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.views.components

import play.api.i18n.Messages
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.govukfrontend.views.viewmodels.footer.FooterItem

object FooterLinks {

  def cookieLink(appConfig: FrontendAppConfig)(implicit messages: Messages) = FooterItem(
    Some(messages("footer.links.Cookies")),
    Some(appConfig.footerCookiesUrl)
  )

  def privacyLink(appConfig: FrontendAppConfig)(implicit messages: Messages) = FooterItem(
    Some(messages("footer.links.PrivacyPolicy")),
    Some(appConfig.footerPrivacyPolicyUrl)
  )

  def termsConditionsLink(appConfig: FrontendAppConfig)(implicit messages: Messages) = FooterItem(
    Some(messages("footer.links.TermsConditions")),
    Some(appConfig.footerTermsConditionsUrl)
  )

  def helpLink(appConfig: FrontendAppConfig)(implicit messages: Messages) = FooterItem(
    Some(messages("footer.links.Help")),
    Some(appConfig.footerHelpUrl)
  )

  def items(appConfig: FrontendAppConfig)(implicit messages: Messages) = Seq(
    cookieLink(appConfig),
    privacyLink(appConfig),
    termsConditionsLink(appConfig),
    helpLink(appConfig)
  )

}
