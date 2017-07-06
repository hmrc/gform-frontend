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

package uk.gov.hmrc.gform

import play.api.Play.{ configuration, current }
import uk.gov.hmrc.play.config.ServicesConfig

trait AppConfig {
  def assetsPrefix: String
  def analyticsToken: String
  def analyticsHost: String
  def reportAProblemPartialUrl: String
  def reportAProblemNonJSUrl: String
  def governmentGatewaySignInUrl: String
  def gformFrontendBaseUrl: String
  def sessionCacheDomain: String
  def sessionCacheBaseUri: String
}

object FrontendAppConfig extends AppConfig with ServicesConfig {

  private val config = configuration

  private def loadConfig(key: String) = config.getString(key).getOrElse(throw new Exception(s"Missing configuration key: $key"))

  private val contactHost = config.getString(s"contact-frontend.host").getOrElse("")
  private val contactFormServiceIdentifier = "MyService"

  override lazy val assetsPrefix = loadConfig(s"assets.url") + loadConfig(s"assets.version")
  override lazy val analyticsToken = loadConfig(s"google-analytics.token")
  override lazy val analyticsHost = loadConfig(s"google-analytics.host")
  override lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  override lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  override lazy val governmentGatewaySignInUrl = config.getString("government-gateway-sign-in-url").getOrElse("")
  //  override lazy val governmentGatewaySignInUrl = "http://localhost:9025/gg/sign-in"

  // this will be empty in non-local environments
  override lazy val gformFrontendBaseUrl = config.getString("gform-frontend-base-url").getOrElse("")

  override lazy val sessionCacheDomain: String = config.getString("cachable.session-cache.domain").getOrElse("")
  override lazy val sessionCacheBaseUri: String = baseUrl("cachable.session-cache")
}
