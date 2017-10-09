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

package uk.gov.hmrc.gform.playcomponents

import akka.stream.Materializer
import org.joda.time.Duration
import play.api.Configuration
import play.api.http.HttpErrorHandler
import play.api.libs.crypto.CSRFTokenSigner
import play.api.mvc.EssentialFilter
import play.filters.csrf.CSRFComponents
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.play.audit.filters.FrontendAuditFilter
import uk.gov.hmrc.play.filters.RecoveryFilter
import uk.gov.hmrc.play.filters.frontend.{CSRFExceptionsFilter, SessionTimeoutFilter}
import uk.gov.hmrc.play.frontend.bootstrap.FrontendFilters
import uk.gov.hmrc.play.frontend.filters.{DeviceIdCookieFilter, SecurityHeadersFilterFactory}
import uk.gov.hmrc.play.http.logging.filters.FrontendLoggingFilter

class FrontendFiltersModule(
    playBuiltInsModule: PlayBuiltInsModule,
    akkaModule: AkkaModule,
    configModule: ConfigModule,
    auditingModule: AuditingModule,
    metricsModule: MetricsModule,
    controllersModule: ControllersModule
) { self =>

  private val frontendAuditFilter = new FrontendAuditFilter {
    override val maskedFormFields = Seq("password")
    override val applicationPort = None
    override val auditConnector = auditingModule.auditConnector
    override def controllerNeedsAuditing(controllerName: String) = configModule.controllerConfig.paramsForController(controllerName).needsAuditing
    override implicit val mat: Materializer = akkaModule.materializer
    override val appName: String = configModule.appConfig.appName
  }

  private val loggingFilter = new FrontendLoggingFilter {
    override def controllerNeedsLogging(controllerName: String) = configModule.controllerConfig.paramsForController(controllerName).needsLogging
    override implicit def mat: Materializer = akkaModule.materializer
  }

  private val csrfExceptionsFilter: CSRFExceptionsFilter = {
    val uriWhiteList = configModule.playConfiguration.getStringSeq("csrfexceptions.whitelist").getOrElse(Seq.empty).toSet
    new CSRFExceptionsFilter(uriWhiteList)
  }

  private val deviceIdCookieFilter = DeviceIdCookieFilter(
    configModule.appConfig.appName,
    auditingModule.auditConnector
  )

  private val sessionTimeoutFilter: SessionTimeoutFilter = {
    //Copy paste from deprecated uk.gov.hmrc.play.frontend.bootstrap.GlobalSettings
    val defaultTimeout = Duration.standardMinutes(15)

    val timeoutDuration = configModule.playConfiguration
      .getLong("session.timeoutSeconds")
      .map(Duration.standardSeconds)
      .getOrElse(defaultTimeout)

    val wipeIdleSession = configModule.playConfiguration
      .getBoolean("session.wipeIdleSession")
      .getOrElse(true)

    val additionalSessionKeysToKeep = configModule.playConfiguration
      .getStringSeq("session.additionalSessionKeysToKeep")
      .getOrElse(Seq.empty).toSet

    new SessionTimeoutFilter(
      timeoutDuration = timeoutDuration,
      additionalSessionKeysToKeep = additionalSessionKeysToKeep,
      onlyWipeAuthToken = !wipeIdleSession
    )
  }

  private val cSRFComponents = new CSRFComponents {
    override def configuration: Configuration = configModule.playConfiguration
    override def csrfTokenSigner: CSRFTokenSigner = playBuiltInsModule.builtInComponents.csrfTokenSigner
    override def httpErrorHandler: HttpErrorHandler = controllersModule.errorHandler
    override implicit def materializer: Materializer = playBuiltInsModule.builtInComponents.materializer
  }

  lazy val httpFilters: Seq[EssentialFilter] = {

    val securityFiltersFactory = new SecurityHeadersFilterFactory {
      override def configuration = configModule.playConfiguration
    }

    lazy val ff = new FrontendFilters {
      override def loggingFilter = self.loggingFilter
      override def securityFilter = securityFiltersFactory.newInstance
      override def frontendAuditFilter = self.frontendAuditFilter
      override def metricsFilter = metricsModule.metricsFilter
      override def deviceIdFilter = self.deviceIdCookieFilter
      override def csrfFilter = cSRFComponents.csrfFilter // was: CSRFFilter()(akkaModule.materializer) //It's deprecated however this is how platform ops instantiated it.
      override def sessionTimeoutFilter = self.sessionTimeoutFilter
      override def csrfExceptionsFilter = self.csrfExceptionsFilter

      override def frontendFilters: Seq[EssentialFilter] = {
        val enableSecurityHeaderFilter = configModule.playConfiguration.getBoolean("security.headers.filter.enabled").getOrElse(true)
        if (enableSecurityHeaderFilter) Seq(securityFilter) ++ defaultFrontendFilters else defaultFrontendFilters
      }
    }

    ff.frontendFilters.filter(_ != RecoveryFilter) //RecoveryFilter unveils internal error messages from connectors
  }

}
