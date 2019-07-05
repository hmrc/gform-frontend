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

package uk.gov.hmrc.gform

import org.slf4j.MDC
import play.api.ApplicationLoader.Context
import play.api._
import play.api.http._
import play.api.i18n.I18nComponents
import play.api.inject.{ Injector, SimpleInjector }
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import uk.gov.hmrc.crypto.ApplicationCrypto
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.GformModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.playcomponents.{ FrontendFiltersModule, PlayBuiltInsModule, RoutingModule }
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import uk.gov.hmrc.play.config.{ AssetsConfig, OptimizelyConfig }

class ApplicationLoader extends play.api.ApplicationLoader {
  def load(context: Context): Application = {
    LoggerConfigurator(context.environment.classLoader).foreach {
      _.configure(context.environment)
    }
    val applicationModule = new ApplicationModule(context)
    applicationModule.application
    applicationModule.initialize()
    applicationModule.application
  }
}

class ApplicationModule(context: Context) extends BuiltInComponentsFromContext(context) with I18nComponents { self =>

  Logger.info(s"Starting GFORM-FRONTEND (ApplicationModule)...")

  private implicit val executionContext = play.api.libs.concurrent.Execution.defaultContext

  private val akkaModule = new AkkaModule(materializer, actorSystem)
  private val playBuiltInsModule = new PlayBuiltInsModule(context, self)

  protected val configModule = new ConfigModule(playBuiltInsModule)
  private val metricsModule = new MetricsModule(playBuiltInsModule, akkaModule, configModule)
  protected val auditingModule = new AuditingModule(configModule, akkaModule, playBuiltInsModule)
  private val wSHttpModule = new WSHttpModule(auditingModule, configModule)

  private val gformBackendModule = new GformBackendModule(wSHttpModule, configModule)

  private val authModule = new AuthModule(configModule, wSHttpModule, gformBackendModule)

  private val pdfGeneratorModule = new PdfGeneratorModule(configModule, wSHttpModule)

  private val lookupRegistry = new LookupRegistry(new uk.gov.hmrc.gform.LookupLoader().registerLookup)

  private val controllersModule = new ControllersModule(
    configModule,
    authModule,
    gformBackendModule,
    playBuiltInsModule,
    auditingModule
  )

  private val fileUploadModule = new FileUploadModule(
    wSHttpModule,
    configModule,
    controllersModule,
    gformBackendModule
  )

  private val graphModule = new GraphModule(
    authModule
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
    pdfGeneratorModule,
    authModule,
    gformBackendModule,
    fileUploadModule,
    validationModule,
    auditingModule,
    playBuiltInsModule,
    graphModule,
    lookupRegistry
  )

  private val testOnlyModule = new TestOnlyModule(
    playBuiltInsModule,
    gformBackendModule
  )

  private val frontendFiltersModule = new FrontendFiltersModule(
    playBuiltInsModule,
    akkaModule,
    configModule,
    auditingModule,
    metricsModule,
    controllersModule
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
    controllersModule
  )

  override lazy val httpErrorHandler: HttpErrorHandler = controllersModule.errorHandler
  override lazy val httpRequestHandler: HttpRequestHandler = routingModule.httpRequestHandler
  override lazy val httpFilters: Seq[EssentialFilter] = frontendFiltersModule.httpFilters
  override def router: Router = routingModule.router

  lazy val optimizelyConfig: OptimizelyConfig = new OptimizelyConfig(configuration)
  lazy val assetsConfig: AssetsConfig = new AssetsConfig(configuration)

  lazy val customInjector
    : Injector = new SimpleInjector(injector) + playBuiltInsModule.ahcWSComponents.wsApi + optimizelyConfig + assetsConfig
  override lazy val application = new DefaultApplication(
    environment,
    applicationLifecycle,
    customInjector,
    configuration,
    httpRequestHandler,
    httpErrorHandler,
    actorSystem,
    materializer
  )

  def initialize() = {

    val appName = configModule.appConfig.appName
    Logger.info(s"Starting frontend $appName in mode ${environment.mode}")
    val applicationCrypto = new ApplicationCrypto(configModule.playConfiguration.underlying)
    applicationCrypto.verifyConfiguration()
    MDC.put("appName", appName)
    val loggerDateFormat: Option[String] = configuration.getString("logger.json.dateformat")
    loggerDateFormat.foreach(str => MDC.put("logger.json.dateformat", str))
    metricsModule.graphiteService.startReporter()
    Logger.info(
      s"Started Fronted $appName in mode ${environment.mode} at port ${application.configuration.getString("http.port")}")
  }
}
