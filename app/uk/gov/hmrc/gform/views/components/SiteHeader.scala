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
import play.api.mvc.Request
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Anonymous, AuthConfig, EeittModule, FormTemplateId }
import uk.gov.hmrc.gform.config.JSConfig

object SiteHeader {
  def apply(
    appConfig: FrontendAppConfig,
    authConfig: Option[AuthConfig],
    templateId: FormTemplateId,
    serviceTitle: String
  )(
    implicit
    request: Request[_],
    messages: Messages
  ): Header = {

    val authTimeout = appConfig.jsConfig(authConfig)

    Header(
      homepageUrl = Some(s"https://www.gov.uk/"),
      serviceName = Some(serviceTitle),
      serviceUrl = None,
      containerClasses = Some("govuk-width-container"),
      navigation =
        if (authTimeout.signOutUrl.isEmpty) None
        else {
          Some(
            Seq(
              HeaderNavigation(
                href = Some(s"${authTimeout.signOutUrl}/${templateId.value}"),
                text = Some(messages("linkText.signOut"))
              )
            ))
        }
    )
  }

}
