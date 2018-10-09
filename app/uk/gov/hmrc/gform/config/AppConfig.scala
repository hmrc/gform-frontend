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

package uk.gov.hmrc.gform.config

import pureconfig._
import uk.gov.hmrc.gform.sharedmodel.config.ContentType

case class AppConfig(
  appName: String,
  `google-analytics`: GoogleAnalytics,
  `government-gateway-sign-in-url`: String,
  `gform-frontend-base-url`: String,
  `agent-subscription-frontend-base-url`: String,
  feature: FeatureToggle,
  formMaxAttachmentSizeMB: Int,
  /*we can't override list in app-config-base:*/
  contentTypesSeparatedByPipe: String
) {
  def contentTypes: List[ContentType] = contentTypesSeparatedByPipe.split('|').toList.map(ContentType.apply)
}

object AppConfig {

  def loadOrThrow(): AppConfig = {
    implicit def hint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))
    val appConfig = loadConfigOrThrow[AppConfig]

    appConfig
  }

  private implicit class VerifyThat[T](t: T) {
    def verifyThat(assertion: T => Boolean, message: String = "") =
      if (!assertion(t)) throw new AppConfigException(message)
  }

  class AppConfigException(message: String) extends IllegalArgumentException(message)
}

case class GoogleAnalytics(
  token: String,
  host: String
)

case class FeatureToggle(emailEnabled: Boolean, concurrentAgentAccess: Boolean)
