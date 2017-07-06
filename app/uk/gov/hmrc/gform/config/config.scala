/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.play.config.ServicesConfig

class ConfigModule {
  lazy val appConfig: AppConfig = AppConfig.loadOrThrow()
  lazy val serviceConfig: ServicesConfig = {

    val c = new ServicesConfig {}
    //ensure eagerly that all configs are in place (below will eagerly throw exception if some of the config are missing)
    c.baseUrl("gform")
    c.baseUrl("auth")
    c.baseUrl("eeitt")
    c
  }
}

case class AppConfig(
  appName: String,
  fileUploadRedirect: String, //TODO: we don't need it
  `google-analytics`: GoogleAnalytics,
  `government-gateway-sign-in-url`: String,
  `gform-frontend-base-url`: String
)

case class GoogleAnalytics(
  token: String,
  host: String
)

case class Assets(
  version: String,
  url: String
)

case class ContactFrontend(host: String)

object AppConfig {

  def loadOrThrow(): AppConfig = {
    implicit def hint[T] = ProductHint[T](ConfigFieldMapping(CamelCase, CamelCase))
    val appConfig = loadConfigOrThrow[AppConfig]

    appConfig
  }

  private implicit class VerifyThat[T](t: T) {
    def verifyThat(assertion: T => Boolean, message: String = "") = if (!assertion(t)) throw new AppConfigException(message)
  }

  class AppConfigException(message: String) extends IllegalArgumentException(message)
}
