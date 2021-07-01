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

import controllers.AssetsComponents
import org.slf4j.LoggerFactory
import play.api.http.HttpRequestHandler
import play.api.routing.Router
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.{ ControllersModule, ErrorHandler }
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.GformModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.play.health.HealthController
import uk.gov.hmrc.govukfrontend.controllers.{ Assets => GovukAssets }
import uk.gov.hmrc.hmrcfrontend.config.LanguageConfig
import uk.gov.hmrc.hmrcfrontend.controllers.{ KeepAliveController, LanguageController, Assets => HmrcAssets }
import uk.gov.hmrc.play.language.LanguageUtils

class RoutingModule(
  playBuiltInsModule: PlayBuiltInsModule,
  akkaModule: AkkaModule,
  configModule: ConfigModule,
  auditingModule: AuditingModule,
  metricsModule: MetricsModule,
  gformModule: GformModule,
  fileUploadModule: FileUploadModule,
  testOnlyModule: TestOnlyModule,
  frontendFiltersModule: FrontendFiltersModule,
  controllersModule: ControllersModule,
  assetsComponents: AssetsComponents,
  errorHandler: ErrorHandler,
  assetsMetadata: _root_.controllers.AssetsMetadata
) { self =>

  private val logger = LoggerFactory.getLogger(getClass)

  //This must be called before `controllers.template.routes` gets read be classloader ...

  val govukfrontendAssets = new GovukAssets(errorHandler, assetsMetadata)
  val hmrcfrontendAssets = new HmrcAssets(errorHandler, assetsMetadata)
  val govukRoutes: govuk.Routes = new govuk.Routes(errorHandler, govukfrontendAssets)
  val hmrcfrontendRoutes: hmrcfrontend.Routes = new hmrcfrontend.Routes(
    errorHandler,
    hmrcfrontendAssets,
    new KeepAliveController(controllersModule.messagesControllerComponents),
    LanguageController(
      configModule.playConfiguration,
      new LanguageUtils(playBuiltInsModule.langs, configModule.playConfiguration)(playBuiltInsModule.messagesApi),
      controllersModule.messagesControllerComponents,
      new LanguageConfig(configModule.playConfiguration)
    )
  )

  private val appRoutes: app.Routes = new app.Routes(
    errorHandler,
    govukRoutes,
    hmrcfrontendRoutes,
    gformModule.emailAuthController,
    gformModule.newFormController,
    gformModule.formController,
    gformModule.addToListController,
    gformModule.summaryController,
    gformModule.declarationController,
    gformModule.acknowledgementController,
    gformModule.errorController,
    gformModule.enrolmentController,
    gformModule.agentEnrolmentController,
    gformModule.saveAcknowledgementController,
    gformModule.reviewController,
    assetsComponents.assets,
    gformModule.fileUploadController,
    gformModule.languageSwitchController,
    gformModule.lookupController,
    gformModule.signOutController,
    gformModule.staticPagesController,
    gformModule.printSectionController,
    gformModule.identityVerificationController
  )

  private val prodRoutes: prod.Routes = new prod.Routes(
    errorHandler,
    appRoutes,
    new HealthController(
      configModule.playConfiguration,
      configModule.environment,
      controllersModule.messagesControllerComponents
    ),
    metricsModule.metricsController
  )

  //we don't need it always, lets leave it lazy
  private lazy val testOnlyDoNotUseInAppConfRoutes: testOnlyDoNotUseInAppConf.Routes =
    new testOnlyDoNotUseInAppConf.Routes(
      errorHandler,
      prodRoutes,
      testOnlyModule.testOnlyController,
      testOnlyModule.debugController
    )

  val router: Router = {
    val key = "application.router"
    val property = configModule.typesafeConfig.getString(key)
    property match {
      case null | "prod.Routes" =>
        logger.info("Using router with prod.Routes")
        prodRoutes
      case "testOnlyDoNotUseInAppConf.Routes" =>
        logger.info(s"Using router with $property")
        testOnlyDoNotUseInAppConfRoutes
      case _ =>
        logger.error(
          s"The option $key has unsupported value: $property. We support only prod.Routes and testOnlyDoNotUseInAppConf.Routes . Using prod.Routes ."
        )
        prodRoutes
    }
  }

  val httpRequestHandler: HttpRequestHandler = new CustomHttpRequestHandler(
    router,
    errorHandler,
    playBuiltInsModule.builtInComponents.httpConfiguration,
    frontendFiltersModule.httpFilters
  )
}
