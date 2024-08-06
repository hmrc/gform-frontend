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

package uk.gov.hmrc.gform.config

import cats.data.NonEmptyList
import cats.implicits._
import com.typesafe.config.{ ConfigFactory, Config => TypeSafeConfig }
import org.typelevel.ci.CIString
import play.api.{ ApplicationLoader, Configuration, Environment }
import play.api.Mode
import play.api.i18n.Lang
import play.api.mvc.Call
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.hmrcfrontend.config.{ AccessibilityStatementConfig, TrackingConsentConfig }
import uk.gov.hmrc.hmrcfrontend.views.html.helpers.HmrcTrackingConsentSnippet
import uk.gov.hmrc.play.audit.http.config.AuditingConfig
import uk.gov.hmrc.play.bootstrap.config.{ ControllerConfig, ControllerConfigs, ServicesConfig }
import org.typelevel.ci._
import play.api.http.HttpConfiguration

class ConfigModule(val context: ApplicationLoader.Context, playBuiltInsModule: PlayBuiltInsModule) {

  val playConfiguration: Configuration = context.initialConfiguration
  val httpConfiguration: HttpConfiguration = HttpConfiguration.fromConfiguration(playConfiguration, context.environment)
  val typesafeConfig: TypeSafeConfig = ConfigFactory.load()
  val environment: Environment = context.environment
  val isProd: Boolean = typesafeConfig.getString("play.http.router") =!= "testOnlyDoNotUseInAppConf.Routes"

  val mode: Mode = environment.mode

  val timeOut: Int = typesafeConfig.getInt("future.timeout")

  val appConfig: AppConfig = AppConfig.loadOrThrow()

  val serviceConfig = new ServicesConfig(playConfiguration)

  val controllerConfigs = ControllerConfigs.fromConfig(playConfiguration)

  val controllerConfig: ControllerConfig = new ControllerConfig {
    //val controllerConfigs: TypeSafeConfig = typesafeConfig.as[TypeSafeConfig]("controllers")
  }

  val accessibilityStatementConfig = new AccessibilityStatementConfig(playConfiguration)

  val auditingConfig: AuditingConfig = AuditingConfig.fromConfig(playConfiguration)

  val availableLanguages: Map[String, Lang] = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy"))
  def routeToSwitchLanguageNoDataChange: String => Call =
    (lang: String) => uk.gov.hmrc.gform.gform.routes.LanguageSwitchController.switchToLanguageNoDataChange(lang)

  def routeToSwitchLanguageDataChange: (FormTemplateId, Option[AccessCode], String) => Call =
    (formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], lang: String) =>
      uk.gov.hmrc.gform.gform.routes.LanguageSwitchController
        .switchToLanguageDataChange(formTemplateId, maybeAccessCode, lang)

  def getOptionalNonEmptyCIStringList(emails: Option[String]): Option[NonEmptyList[CIString]] =
    emails.flatMap(v => NonEmptyList.fromList(v.split(":").toList.filter(_.trim.nonEmpty).map(email => ci"$email")))

  val frontendAppConfig: FrontendAppConfig = {
    def getJSConfig(path: String) =
      JSConfig(
        typesafeConfig.getBoolean(s"$path.timeoutEnabled"),
        typesafeConfig.getInt(s"$path.timeout"),
        typesafeConfig.getInt(s"$path.countdown"),
        typesafeConfig.getString(s"$path.keepAliveUrl"),
        typesafeConfig.getString(s"$path.signOutUrl")
      )
    FrontendAppConfig(
      albAdminIssuerUrl =
        playConfiguration.getOptional[String]("albAdminIssuerUrl").getOrElse("idp-url-variable-not-set"),
      governmentGatewaySignInUrl = typesafeConfig.getString("government-gateway-sign-in-url"),
      gformFrontendBaseUrl = typesafeConfig.getString("gform-frontend-base-url"),
      signOutUrl = typesafeConfig.getString("signout-url"),
      footerAccessibilityStatementUrl = typesafeConfig.getString("footer-accessibility-statement-url"),
      ptaHomeUrl = typesafeConfig.getString("pta-home-url"),
      ptaMessagesUrl = typesafeConfig.getString("pta-messages-url"),
      ptaTrackUrl = typesafeConfig.getString("pta-track-url"),
      ptaProfileUrl = typesafeConfig.getString("pta-profile-url"),
      betaFeedbackUrlNoAuth = s"/contact/beta-feedback-unauthenticated?service=",
      authModule = AuthModule(
        getJSConfig("auth-module.hmrc"),
        getJSConfig("auth-module.anonymous"),
        getJSConfig("auth-module.awsAlbAuth"),
        getJSConfig("auth-module.email")
      ),
      availableLanguages = availableLanguages,
      routeToSwitchLanguageDataChange = routeToSwitchLanguageDataChange,
      routeToSwitchLanguageNoDataChange = routeToSwitchLanguageNoDataChange,
      optimizelyUrl = for {
        url       <- playConfiguration.getOptional[String]("optimizely.url")
        projectId <- playConfiguration.getOptional[String]("optimizely.projectId")
      } yield s"$url$projectId.js",
      trackingConsentSnippet = new HmrcTrackingConsentSnippet(new TrackingConsentConfig(playConfiguration)),
      emailAuthStaticCodeEmails =
        getOptionalNonEmptyCIStringList(playConfiguration.getOptional[String]("emailAuth.staticCodeEmails")),
      accessibilityStatementConfig = accessibilityStatementConfig,
      refreshSessionUrl = typesafeConfig.getString("refresh-session-url"),
      isProd = isProd,
      configuration = playConfiguration
    )
  }
}
