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

package uk.gov.hmrc.gform

import org.slf4j.{ LoggerFactory, MDC }
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.I18nComponents
import play.api.inject.{ Injector, SimpleInjector }
import play.api.libs.ws.ahc.AhcWSComponents
import play.api.mvc.{ EssentialFilter, LegacySessionCookieBaker, SessionCookieBaker }
import play.api.routing.Router
import uk.gov.hmrc.crypto.ApplicationCrypto
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.{ CSRFErrorHandler, ControllersModule, ErrResponder, ErrorHandler }
import _root_.controllers.AssetsComponents
import play.filters.csrf.{ CSRF, CSRFComponents }
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.GformModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.metrics.{ GraphiteModule, MetricsModule }
import uk.gov.hmrc.gform.playcomponents.{ FrontendFiltersModule, PlayBuiltInsModule, RoutingModule }
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorConnector
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.controllers.CookieNames._

class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment)
    }
    val applicationModule = new ApplicationModule(context)
    applicationModule.initialize()
    applicationModule.application
  }
}

class ApplicationModule(context: Context)
    extends BuiltInComponentsFromContext(context) with AssetsComponents with AhcWSComponents with I18nComponents
    with CSRFComponents {
  self =>

  private val logger = LoggerFactory.getLogger(getClass)

  logger.info(s"Starting GFORM-FRONTEND (ApplicationModule)...")

  protected val akkaModule = new AkkaModule(materializer, actorSystem)
  private val playBuiltInsModule = new PlayBuiltInsModule(self)

  protected val configModule = new ConfigModule(context, playBuiltInsModule, wsClient)
  protected val auditingModule = new AuditingModule(configModule, akkaModule, applicationLifecycle)

  val errResponder: ErrResponder = new ErrResponder(
    configModule.frontendAppConfig,
    auditingModule.httpAuditingService,
    playBuiltInsModule.i18nSupport,
    playBuiltInsModule.langs
  )(messagesApi)

  override lazy val httpErrorHandler: ErrorHandler = new ErrorHandler(
    configModule.environment,
    configModule.playConfiguration,
    configModule.context.devContext.map(_.sourceMapper),
    errResponder
  )

  val csrfHttpErrorHandler: CSRFErrorHandler = new CSRFErrorHandler(
    configModule.environment,
    configModule.playConfiguration,
    configModule.context.devContext.map(_.sourceMapper),
    errResponder,
    configModule.appConfig
  )

  override lazy val csrfErrorHandler: CSRF.ErrorHandler = new CSRF.CSRFHttpErrorHandler(csrfHttpErrorHandler)

  private val metricsModule = new MetricsModule(configModule, akkaModule, controllerComponents, executionContext)

  new GraphiteModule(environment, configuration, applicationLifecycle, metricsModule)

  protected lazy val wSHttpModule = new WSHttpModule(auditingModule, configModule, akkaModule, this)

  private val gformBackendModule = new GformBackendModule(wSHttpModule, configModule)

  private val authModule = new AuthModule(configModule, wSHttpModule, gformBackendModule)

  protected val pdfGeneratorConnector =
    new PdfGeneratorConnector(configModule.serviceConfig, wSHttpModule.auditableWSHttp)

  private val lookupRegistry = new LookupRegistry(new uk.gov.hmrc.gform.LookupLoader().registerLookup)

  private val sessionCookieBaker: SessionCookieBaker = {
    val httpConfiguration: HttpConfiguration =
      new HttpConfiguration.HttpConfigurationProvider(configModule.playConfiguration, configModule.environment).get

    val config: SessionConfiguration = httpConfiguration.session

    new LegacySessionCookieBaker(config, cookieSigner)
  }

  private val anonymousSessionCookieBaker: SessionCookieBaker = {
    val httpConfiguration: HttpConfiguration =
      new HttpConfiguration.HttpConfigurationProvider(configModule.playConfiguration, configModule.environment).get

    val config: SessionConfiguration =
      httpConfiguration.session.copy(cookieName = anonymousFormSessionCookieName)

    new LegacySessionCookieBaker(config, cookieSigner)
  }

  private val graphModule = new GraphModule(
    authModule,
    gformBackendModule
  )

  private val controllersModule = new ControllersModule(
    configModule,
    authModule,
    gformBackendModule,
    playBuiltInsModule,
    auditingModule,
    this,
    sessionCookieBaker,
    errResponder,
    graphModule,
    wSHttpModule
  )

  private val fileUploadModule = new FileUploadModule(
    wSHttpModule,
    configModule,
    controllersModule,
    gformBackendModule
  )

  private val validationModule = new ValidationModule(
    fileUploadModule,
    gformBackendModule,
    graphModule,
    lookupRegistry,
    playBuiltInsModule
  )

  private val gformModule = new GformModule(
    configModule,
    wSHttpModule,
    controllersModule,
    pdfGeneratorConnector,
    authModule,
    gformBackendModule,
    fileUploadModule,
    validationModule,
    auditingModule,
    playBuiltInsModule,
    graphModule,
    lookupRegistry,
    errResponder
  )

  private val testOnlyModule = new TestOnlyModule(
    configModule,
    gformBackendModule,
    controllersModule,
    graphModule,
    lookupRegistry,
    this,
    fileUploadModule
  )

  val applicationCrypto: ApplicationCrypto = new ApplicationCrypto(configModule.typesafeConfig)
  applicationCrypto.verifyConfiguration()

  private val frontendFiltersModule = new FrontendFiltersModule(
    applicationCrypto,
    playBuiltInsModule,
    akkaModule,
    configModule,
    auditingModule,
    gformBackendModule,
    metricsModule,
    controllersModule,
    this,
    sessionCookieBaker,
    anonymousSessionCookieBaker,
    cookieSigner
  )

  private val routingModule = new RoutingModule(
    playBuiltInsModule,
    akkaModule,
    configModule,
    auditingModule,
    metricsModule,
    gformModule,
    fileUploadModule,
    testOnlyModule,
    frontendFiltersModule,
    controllersModule,
    this,
    httpErrorHandler,
    assetsMetadata
  )

  override lazy val httpRequestHandler: HttpRequestHandler = routingModule.httpRequestHandler
  override val httpFilters: Seq[EssentialFilter] = frontendFiltersModule.httpFilters
  override def router: Router = routingModule.router

  val customInjector: Injector =
    new SimpleInjector(injector) + wsClient

  private val app = new DefaultApplication(
    environment,
    applicationLifecycle,
    customInjector,
    configuration,
    requestFactory,
    httpRequestHandler,
    httpErrorHandler,
    actorSystem,
    materializer
  )

  override lazy val application = app

  def initialize() = {

    val appName = configModule.appConfig.appName
    logger.info(s"Starting frontend $appName in mode ${environment.mode}")
    MDC.put("appName", appName)
    val loggerDateFormat: Option[String] = configuration.getOptional[String]("logger.json.dateformat")
    loggerDateFormat.foreach(str => MDC.put("logger.json.dateformat", str))
    logger.info(
      s"Started Fronted $appName in mode ${environment.mode} at port ${application.configuration.getOptional[String]("http.port")}")
  }
}
