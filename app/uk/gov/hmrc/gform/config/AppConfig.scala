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

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.generic.ProductHint
import uk.gov.hmrc.gform.sharedmodel.config.{ ContentType, FileExtension }

case class AppConfig(
  appName: String,
  `government-gateway-sign-in-url`: String,
  `gform-frontend-base-url`: String,
  `agent-subscription-frontend-base-url`: String,
  feature: FeatureToggle,
  `auth-module`: AuthModule,
  formMaxAttachmentSizeMB: Int,
  /*we can't override list in app-config-base:*/
  contentTypesSeparatedByPipe: String,
  restrictedFileExtensionsSeparatedByPipe: String,
  albAdminIssuerUrl: String,
  `case-worker-assumed-identity-cookie`: String
) {
  def contentTypes: List[ContentType] = contentTypesSeparatedByPipe.split('|').toList.map(ContentType.apply)

  def restrictedFileExtensions: List[FileExtension] =
    restrictedFileExtensionsSeparatedByPipe.split('|').toList.map(FileExtension)
}

object AppConfig {

  def loadOrThrow(): AppConfig = {
    implicit def hint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))
    val appConfig = ConfigSource.default.loadOrThrow[AppConfig]

    appConfig
  }

  class AppConfigException(message: String) extends IllegalArgumentException(message)
}

case class AuthModule(
  hmrc: JSConfig,
  anonymous: JSConfig,
  awsAlbAuth: JSConfig,
  email: JSConfig
)
case class JSConfig(
  timeoutEnabled: Boolean,
  timeout: Int,
  countdown: Int,
  keepAliveUrl: String,
  signOutUrl: String
)

case class FeatureToggle(emailEnabled: Boolean, concurrentAgentAccess: Boolean)
