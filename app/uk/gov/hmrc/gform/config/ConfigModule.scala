/*
 * Copyright 2019 HM Revenue & Customs
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

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import net.ceedubs.ficus.Ficus._
import play.api.{ Configuration, Environment }
import play.api.Mode.Mode
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.play.config.{ ControllerConfig, ServicesConfig }

class ConfigModule(playBuiltInsModule: PlayBuiltInsModule) {

  val playConfiguration: Configuration = playBuiltInsModule.context.initialConfiguration
  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()
  val environment: Environment = playBuiltInsModule.context.environment

  val timeOut: Int = typesafeConfig.getInt("future.timeout")

  val appConfig: AppConfig = AppConfig.loadOrThrow()

  val serviceConfig: ServicesConfig = {
    val c = new ServicesConfig {
      //watch out!
      // ServicesConfig requires running play application so if we don't override these
      // we will experience 'Caused by: java.lang.RuntimeException: There is no started application'
      override protected def runModeConfiguration: Configuration = playConfiguration

      override protected def mode: Mode = playBuiltInsModule.context.environment.mode
    }

    //ensure eagerly that all configs are in place (below will eagerly throw exception if some of the config are missing)
    c.baseUrl("gform")
    c.baseUrl("auth")
    c.baseUrl("eeitt")
    c.baseUrl("email")
    c
  }
  val controllerConfig: ControllerConfig = new ControllerConfig {
    val controllerConfigs: TypeSafeConfig = typesafeConfig.as[TypeSafeConfig]("controllers")
  }

  val frontendAppConfig: FrontendAppConfig = {
    def getJSConfig(path: String) =
      JSConfig(
        typesafeConfig.getBoolean(s"$path.timeoutEnabled"),
        typesafeConfig.getInt(s"$path.timeout"),
        typesafeConfig.getInt(s"$path.countdown"),
        typesafeConfig.getString(s"$path.keepAliveUrl"),
        typesafeConfig.getString(s"$path.signOutUrl")
      )
    val contactFormServiceIdentifier = "GForm"
    FrontendAppConfig(
      assetsPrefix = typesafeConfig.getString(s"assets.url") + typesafeConfig.getString(s"assets.version"),
      analyticsToken = typesafeConfig.getString(s"google-analytics.token"),
      analyticsHost = typesafeConfig.getString(s"google-analytics.host"),
      reportAProblemPartialUrl = s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier",
      reportAProblemNonJSUrl = s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier",
      governmentGatewaySignInUrl = typesafeConfig.getString("government-gateway-sign-in-url"),
      gformFrontendBaseUrl = typesafeConfig.getString("gform-frontend-base-url"),
      signOutUrl = typesafeConfig.getString("signout-url"),
      betaFeedbackUrlNoAuth = s"/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier",
      whitelistEnabled = typesafeConfig.getString("whitelisting-enabled").toBoolean,
      googleTagManagerIdAvailable = typesafeConfig.getString("google-tag-manager.id-available").toBoolean,
      googleTagManagerId = typesafeConfig.getString(s"google-tag-manager.id"),
      authModule = AuthModule(
        getJSConfig("auth-module.legacyEEITTAuth"),
        getJSConfig("auth-module.hmrc"),
        getJSConfig("auth-module.anonymous"))
    )
  }
}
