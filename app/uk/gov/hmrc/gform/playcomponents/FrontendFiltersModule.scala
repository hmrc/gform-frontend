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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import play.api.Configuration
import play.api.http.HttpErrorHandler
import play.api.libs.crypto.CSRFTokenSigner
import play.api.mvc.EssentialFilter
import play.filters.csrf.CSRFComponents
import play.filters.headers.{ SecurityHeadersConfig, SecurityHeadersFilter }
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.crypto.ApplicationCrypto
import uk.gov.hmrc.play.bootstrap.filters.frontend.crypto.{ DefaultCookieCryptoFilter, SessionCookieCrypto }
import uk.gov.hmrc.play.bootstrap.filters.frontend.deviceid.DefaultDeviceIdFilter
import uk.gov.hmrc.play.bootstrap.filters.{ CacheControlConfig, CacheControlFilter, DefaultLoggingFilter, FrontendFilters, MDCFilter }
import uk.gov.hmrc.play.bootstrap.filters.frontend.{ DefaultFrontendAuditFilter, HeadersFilter, SessionTimeoutFilter, SessionTimeoutFilterConfig }

import scala.concurrent.ExecutionContext

class FrontendFiltersModule(
  applicationCrypto: ApplicationCrypto,
  playBuiltInsModule: PlayBuiltInsModule,
  akkaModule: AkkaModule,
  configModule: ConfigModule,
  auditingModule: AuditingModule,
  metricsModule: MetricsModule,
  controllersModule: ControllersModule
)(implicit ec: ExecutionContext) { self =>
  private implicit val materializer: Materializer = akkaModule.materializer

  private val frontendAuditFilter = new DefaultFrontendAuditFilter(
    configModule.playConfiguration,
    configModule.controllerConfigs,
    auditingModule.auditConnector,
    materializer) {
    override val maskedFormFields = Seq("password")
  }

  private val cSRFComponents = new CSRFComponents {
    override def configuration: Configuration = configModule.playConfiguration
    override def csrfTokenSigner: CSRFTokenSigner = playBuiltInsModule.builtInComponents.csrfTokenSigner
    override def httpErrorHandler: HttpErrorHandler = controllersModule.errorHandler
    override implicit def materializer: Materializer = playBuiltInsModule.builtInComponents.materializer
  }

  private val cookieCryptoFilter =
    new DefaultCookieCryptoFilter(new SessionCookieCrypto(applicationCrypto.SessionCookieCrypto))

  lazy val httpFilters: Seq[EssentialFilter] = new FrontendFilters(
    configModule.playConfiguration,
    new DefaultLoggingFilter(configModule.controllerConfigs),
    new HeadersFilter(materializer),
    SecurityHeadersFilter(SecurityHeadersConfig.fromConfiguration(configModule.playConfiguration)),
    frontendAuditFilter,
    metricsModule.metricsFilter,
    new DefaultDeviceIdFilter(configModule.playConfiguration, auditingModule.auditConnector),
    cSRFComponents.csrfFilter,
    cookieCryptoFilter,
    new SessionTimeoutFilter(SessionTimeoutFilterConfig.fromConfig(configModule.playConfiguration)),
    new CacheControlFilter(CacheControlConfig.fromConfig(configModule.playConfiguration), materializer),
    new MDCFilter(akkaModule.materializer, configModule.playConfiguration)
  ).filters
}
