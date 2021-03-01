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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import play.api.libs.crypto.CookieSigner
import play.api.mvc.{ EssentialFilter, SessionCookieBaker }
import play.filters.csrf.CSRFComponents
import play.filters.headers.SecurityHeadersFilter
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.crypto.ApplicationCrypto
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.play.bootstrap.config.DefaultHttpAuditEvent
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCryptoFilter
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCryptoProvider
import uk.gov.hmrc.play.bootstrap.frontend.filters.deviceid.DefaultDeviceIdFilter
import uk.gov.hmrc.play.bootstrap.frontend.filters._
import uk.gov.hmrc.play.bootstrap.filters.{ CacheControlConfig, CacheControlFilter, DefaultLoggingFilter, MDCFilter }

import scala.concurrent.ExecutionContext

class FrontendFiltersModule(
  applicationCrypto: ApplicationCrypto,
  playBuiltInsModule: PlayBuiltInsModule,
  akkaModule: AkkaModule,
  configModule: ConfigModule,
  auditingModule: AuditingModule,
  gformBackendModule: GformBackendModule,
  metricsModule: MetricsModule,
  controllersModule: ControllersModule,
  csrfComponents: CSRFComponents,
  sessionCookieBaker: SessionCookieBaker,
  anonymousSessionCookieBaker: SessionCookieBaker,
  cookieSigner: CookieSigner
)(implicit ec: ExecutionContext) { self =>
  private implicit val materializer: Materializer = akkaModule.materializer

  private val frontendAuditFilter = new DefaultFrontendAuditFilter(
    configModule.controllerConfigs,
    auditingModule.auditConnector,
    new DefaultHttpAuditEvent(configModule.appConfig.appName),
    materializer) {
    override val maskedFormFields = Seq("password")
  }

  private val cookieCryptoFilter: SessionCookieCryptoFilter = {
    val applicationCrypto: ApplicationCrypto = new ApplicationCrypto(configModule.typesafeConfig)
    val sessionCookieCrypto: SessionCookieCrypto = new SessionCookieCryptoProvider(applicationCrypto).get()

    new GformSessionCookieCryptoFilter(
      sessionCookieCrypto,
      gformBackendModule.gformConnector,
      cookieSigner,
      sessionCookieBaker,
      anonymousSessionCookieBaker
    )
  }

  private val cacheControlFilter: CacheControlFilter = {
    val cacheControlConfig: CacheControlConfig = CacheControlConfig.fromConfig(configModule.playConfiguration)
    new CacheControlFilter(cacheControlConfig, materializer)
  }

  private val mdcFilter: MDCFilter =
    new MDCFilter(materializer, configModule.playConfiguration, configModule.appConfig.appName)

  private val sessionTimeoutFilter = new SessionTimeoutFilter(
    SessionTimeoutFilterConfig.fromConfig(configModule.playConfiguration))

  private val deviceIdFilter = new DefaultDeviceIdFilter(
    configModule.appConfig.appName,
    configModule.playConfiguration,
    auditingModule.auditConnector)

  private val securityHeadersFilter = SecurityHeadersFilter(configModule.playConfiguration)

  private val headersFilter = new HeadersFilter(materializer)

  private val loggingFilter = new DefaultLoggingFilter(configModule.controllerConfigs)

  private val allowListFilter = new AllowlistFilter(configModule.playConfiguration, materializer)

  private val sessionIdFilter = new SessionIdFilter(materializer, ec, sessionCookieBaker)

  lazy val httpFilters: Seq[EssentialFilter] = new FrontendFilters(
    configModule.playConfiguration,
    loggingFilter,
    headersFilter,
    securityHeadersFilter,
    frontendAuditFilter,
    metricsModule.metricsFilter,
    deviceIdFilter,
    csrfComponents.csrfFilter,
    cookieCryptoFilter,
    sessionTimeoutFilter,
    cacheControlFilter,
    mdcFilter,
    allowListFilter,
    sessionIdFilter
  ).filters
}
