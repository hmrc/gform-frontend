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

package uk.gov.hmrc.gform.config

import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import net.ceedubs.ficus.Ficus._
import play.api.libs.ws.WSClient
import play.api.{ ApplicationLoader, Configuration, Environment }
import play.api.Mode
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.csp.config.ApplicationConfig
import uk.gov.hmrc.csp.{ CachedStaticHtmlPartialProvider, WebchatClient }
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.bootstrap.config.{ AuditingConfigProvider, ControllerConfig, ControllerConfigs, RunMode, ServicesConfig }

class ConfigModule(val context: ApplicationLoader.Context, playBuiltInsModule: PlayBuiltInsModule, wsClient: WSClient) {

  val playConfiguration: Configuration = context.initialConfiguration
  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()
  val environment: Environment = context.environment

  val mode: Mode = environment.mode
  val runMode: RunMode = new RunMode(playConfiguration, mode)

  val timeOut: Int = typesafeConfig.getInt("future.timeout")

  val appConfig: AppConfig = AppConfig.loadOrThrow()

  val serviceConfig = new ServicesConfig(playConfiguration, runMode)

  val controllerConfigs = ControllerConfigs.fromConfig(playConfiguration)

  val controllerConfig: ControllerConfig = new ControllerConfig {
    val controllerConfigs: TypeSafeConfig = typesafeConfig.as[TypeSafeConfig]("controllers")
  }

  val auditingConfig: AuditingConfig = new AuditingConfigProvider(playConfiguration, runMode, appConfig.appName).get()

  val availableLanguages: Map[String, Lang] = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy"))
  def routeToSwitchLanguage: String => Call =
    (lang: String) => uk.gov.hmrc.gform.gform.routes.LanguageSwitchController.switchToLanguage(lang)

  private val webchatClient =
    new WebchatClient(
      new CachedStaticHtmlPartialProvider(
        wsClient,
        context.initialConfiguration,
        playBuiltInsModule.builtInComponents.actorSystem),
      new ApplicationConfig(context.initialConfiguration)
    )

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
      webchatClient = webchatClient,
      albAdminIssuerUrl =
        playConfiguration.getOptional[String]("albAdminIssuerUrl").getOrElse("idp-url-variable-not-set"),
      analyticsToken = typesafeConfig.getString(s"google-analytics.token"),
      analyticsHost = typesafeConfig.getString(s"google-analytics.host"),
      reportAProblemPartialUrl = s"/contact/problem_reports_ajax?service=$contactFormServiceIdentifier",
      reportAProblemNonJSUrl = s"/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier",
      governmentGatewaySignInUrl = typesafeConfig.getString("government-gateway-sign-in-url"),
      gformFrontendBaseUrl = typesafeConfig.getString("gform-frontend-base-url"),
      signOutUrl = typesafeConfig.getString("signout-url"),
      footerCookiesUrl = typesafeConfig.getString("footer-cookies-url"),
      footerPrivacyPolicyUrl = typesafeConfig.getString("footer-privacy-policy-url"),
      footerTermsConditionsUrl = typesafeConfig.getString("footer-terms-conditions-url"),
      footerHelpUrl = typesafeConfig.getString("footer-help-url"),
      betaFeedbackUrlNoAuth = s"/contact/beta-feedback-unauthenticated?service=$contactFormServiceIdentifier",
      whitelistEnabled = typesafeConfig.getString("whitelisting-enabled").toBoolean,
      googleTagManagerIdAvailable = typesafeConfig.getString("google-tag-manager.id-available").toBoolean,
      googleTagManagerId = typesafeConfig.getString(s"google-tag-manager.id"),
      authModule = AuthModule(
        getJSConfig("auth-module.legacyEEITTAuth"),
        getJSConfig("auth-module.hmrc"),
        getJSConfig("auth-module.anonymous"),
        getJSConfig("auth-module.awsAlbAuth")
      ),
      availableLanguages = availableLanguages,
      routeToSwitchLanguage = routeToSwitchLanguage,
      contactFormServiceIdentifier = contactFormServiceIdentifier,
      optimizelyUrl = for {
        url       <- playConfiguration.getOptional[String]("optimizely.url")
        projectId <- playConfiguration.getOptional[String]("optimizely.projectId")
      } yield s"$url$projectId.js"
    )
  }
}
