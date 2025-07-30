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

package uk.gov.hmrc.gform

import org.slf4j.LoggerFactory
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.{ I18nComponents, Lang, Messages }
import play.api.inject.{ Injector, SimpleInjector }
import play.api.libs.ws.ahc.{ AhcWSClient, AhcWSClientConfigFactory, AhcWSComponents }
import play.api.mvc.{ EssentialFilter, LegacySessionCookieBaker, SessionCookieBaker }
import play.api.routing.Router
import uk.gov.hmrc.crypto.ApplicationCrypto
import uk.gov.hmrc.gform.LookupLoader
import uk.gov.hmrc.gform.addresslookup.AddressLookupModule
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.{ CSRFErrorHandler, ControllersModule, ErrResponder, ErrorHandler }
import _root_.controllers.AssetsComponents
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import play.filters.csrf.{ CSRF, CSRFComponents }
import uk.gov.hmrc.gform.cache.CacheModule
import uk.gov.hmrc.gform.objectStore.ObjectStoreModule
import uk.gov.hmrc.gform.gform.GformModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.metrics.{ GraphiteModule, MetricsModule }
import uk.gov.hmrc.gform.playcomponents.{ FrontendFiltersModule, PlayBuiltInsModule, RequestHeaderService, RoutingModule }
import uk.gov.hmrc.gform.tasklist.TaskListModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.upscan.UpscanModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.gform.controllers.CookieNames._
import uk.gov.hmrc.gform.mongo.MongoModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateContextCacheManager
import uk.gov.hmrc.play.bootstrap.LoggerModule
import uk.gov.hmrc.play.bootstrap.config.Base64ConfigDecoder
import uk.gov.hmrc.http.client.{ HttpClientV2, HttpClientV2Impl }

class ApplicationLoader extends play.api.ApplicationLoader with Base64ConfigDecoder {

  def load(_context: Context): Application = {
    val context = _context.copy(initialConfiguration = decodeConfig(_context.initialConfiguration))

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

  private val loggerModule = new LoggerModule
  loggerModule.bindings(environment, configuration)

  private val logger = LoggerFactory.getLogger(getClass)

  logger.info(s"Starting GFORM-FRONTEND (ApplicationModule)...")

  override lazy val actorSystem: ActorSystem = ActorSystem()

  protected val akkaModule =
    new AkkaModule(Materializer.matFromSystem(actorSystem), actorSystem)

  private val playBuiltInsModule = new PlayBuiltInsModule(self)

  protected lazy val configModule = new ConfigModule(context, playBuiltInsModule)

  private val metricsModule = new MetricsModule(configModule, akkaModule, executionContext)

  private val graphiteModule = new GraphiteModule(environment, configuration, applicationLifecycle, metricsModule)

  protected val auditingModule = new AuditingModule(configModule, akkaModule, graphiteModule, applicationLifecycle)

  private val mongoModule = new MongoModule(configModule)

  val errResponder: ErrResponder = new ErrResponder(
    configModule.frontendAppConfig,
    auditingModule.httpAuditingService,
    playBuiltInsModule.i18nSupport,
    playBuiltInsModule.langs
  )(messagesApi)

  val englishMessages: Messages = messagesApi.preferred(Seq(Lang("en")))

  private val httpClientV2: HttpClientV2 =
    new HttpClientV2Impl(
      wsClient = AhcWSClient(AhcWSClientConfigFactory.forConfig(configuration.underlying)),
      actorSystem,
      configuration,
      hooks = Seq.empty
    )

  protected lazy val wSHttpModule = new WSHttpModule(httpClientV2)

  private val gformBackendModule = new GformBackendModule(wSHttpModule, configModule)

  private val authModule = new AuthModule(configModule, wSHttpModule)

  private val lookupRegistry = new LookupRegistry(new LookupLoader(configModule.appConfig.indexPath).registerLookup)

  private val hmrcSessionCookieBaker: SessionCookieBaker = {
    val httpConfiguration: HttpConfiguration =
      new HttpConfiguration.HttpConfigurationProvider(configModule.playConfiguration, configModule.environment).get

    val config: SessionConfiguration = httpConfiguration.session
    new LegacySessionCookieBaker(config, cookieSigner)
  }

  private val anonymousSessionCookieBaker: SessionCookieBaker = {
    val httpConfiguration: HttpConfiguration =
      new HttpConfiguration.HttpConfigurationProvider(configModule.playConfiguration, configModule.environment).get

    val config: SessionConfiguration = httpConfiguration.session.copy(cookieName = anonymousFormSessionCookieName)
    new LegacySessionCookieBaker(config, cookieSigner)
  }

  private val emailSessionCookieBaker: SessionCookieBaker = {
    val httpConfiguration: HttpConfiguration =
      new HttpConfiguration.HttpConfigurationProvider(configModule.playConfiguration, configModule.environment).get

    val config: SessionConfiguration = httpConfiguration.session.copy(cookieName = emailFormSessionCookieName)
    new LegacySessionCookieBaker(config, cookieSigner)
  }

  private val graphModule = new GraphModule(
    authModule,
    gformBackendModule,
    englishMessages
  )

  private val objectStoreModule = new ObjectStoreModule(
    gformBackendModule.gformConnector
  )

  private val controllersModule = new ControllersModule(
    configModule,
    authModule,
    gformBackendModule,
    playBuiltInsModule,
    this,
    errResponder,
    graphModule,
    lookupRegistry
  )

  val applicationCrypto: ApplicationCrypto = new ApplicationCrypto(configModule.typesafeConfig)

  applicationCrypto.verifyConfiguration()

  private val upscanModule = new UpscanModule(
    wSHttpModule,
    configModule,
    gformBackendModule,
    configModule.appConfig
  )

  private val addressLookupModule = new AddressLookupModule(
    wSHttpModule,
    configModule,
    gformBackendModule
  )

  private val validationModule = new ValidationModule(
    gformBackendModule,
    graphModule,
    lookupRegistry
  )

  private val taskListModule = new TaskListModule(
    configModule,
    validationModule,
    gformBackendModule,
    graphModule
  )

  private val gformModule = new GformModule(
    akkaModule,
    configModule,
    wSHttpModule,
    controllersModule,
    authModule,
    gformBackendModule,
    objectStoreModule,
    taskListModule,
    upscanModule,
    addressLookupModule,
    validationModule,
    auditingModule,
    playBuiltInsModule,
    graphModule,
    lookupRegistry,
    englishMessages,
    this
  )

  private val testOnlyModule = new TestOnlyModule(
    playBuiltInsModule,
    configModule,
    gformBackendModule,
    controllersModule,
    graphModule,
    lookupRegistry,
    this,
    objectStoreModule,
    gformModule,
    wSHttpModule,
    applicationCrypto,
    hmrcSessionCookieBaker,
    englishMessages
  )

  private val formTemplateContextCacheManager = new FormTemplateContextCacheManager()
  private val cacheModule = new CacheModule(controllersModule, mongoModule, configModule)

  private val requestHeaderService =
    new RequestHeaderService(
      gformBackendModule.gformConnector,
      formTemplateContextCacheManager,
      cacheModule.formTemplateCacheService
    )

  val errorHandler: ErrorHandler = new ErrorHandler(
    configModule.environment,
    configModule.playConfiguration,
    configModule.context.devContext.map(_.sourceMapper),
    errResponder,
    requestHeaderService
  )

  private val frontendFiltersModule = new FrontendFiltersModule(
    gformBackendModule,
    authModule,
    playBuiltInsModule,
    akkaModule,
    configModule,
    auditingModule,
    metricsModule,
    controllersModule,
    this,
    emailSessionCookieBaker,
    anonymousSessionCookieBaker,
    hmrcSessionCookieBaker,
    requestHeaderService,
    errorHandler
  )

  private val routingModule = new RoutingModule(
    playBuiltInsModule,
    configModule,
    gformModule,
    testOnlyModule,
    frontendFiltersModule,
    controllersModule,
    this,
    errorHandler,
    assetsMetadata,
    cacheModule
  )

  lazy val csrfHttpErrorHandler: CSRFErrorHandler = new CSRFErrorHandler(
    configModule.environment,
    configModule.playConfiguration,
    configModule.context.devContext.map(_.sourceMapper),
    errResponder,
    configModule.appConfig,
    requestHeaderService
  )

  override lazy val csrfErrorHandler: CSRF.ErrorHandler = new CSRF.CSRFHttpErrorHandler(csrfHttpErrorHandler)
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
    errorHandler,
    actorSystem,
    materializer
  )

  override lazy val application = app

  def initialize() = {
    val appName = configModule.appConfig.appName
    logger.info(
      s"Started Frontend $appName in mode ${environment.mode} at port ${application.configuration.getOptional[String]("http.port")}"
    )
  }
}
