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

import controllers.Assets
import play.api.Logger
import play.api.http.HttpRequestHandler
import play.api.routing.Router
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.GformModule
import uk.gov.hmrc.gform.metrics.MetricsModule
import uk.gov.hmrc.gform.testonly.TestOnlyModule
import uk.gov.hmrc.play.health.AdminController

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
    errorHandlerModule: ErrorHandlerModule

) { self =>

  //This must be called before `controllers.template.routes` gets read be classloader ...
  template.RoutesPrefix.setPrefix("/template")

  private val appRoutes: app.Routes = new app.Routes(
    errorHandlerModule.errorHandler,
    gformModule.formController,
    gformModule.summaryController,
    gformModule.declarationController,
    gformModule.acknowledgementController,
    gformModule.errorController,
    gformModule.enrolmentController,
    new Assets(errorHandlerModule.errorHandler),
    fileUploadModule.fileUploadController
  )

  private val prodRoutes: prod.Routes = new prod.Routes(
    errorHandlerModule.errorHandler,
    appRoutes,
    new controllers.template.Template(errorHandlerModule.errorHandler),
    new AdminController(configModule.playConfiguration),
    metricsModule.metricsController
  )

  //we don't need it always, lets leave it lazy
  private lazy val testOnlyDoNotUseInAppConfRoutes: testOnlyDoNotUseInAppConf.Routes = new testOnlyDoNotUseInAppConf.Routes(
    errorHandlerModule.errorHandler,
    prodRoutes,
    testOnlyModule.testOnlyController
  )

  val router: Router = {
    val applicationRouterKey = "application.router"
    val applicationRouterProp = System.getProperty(applicationRouterKey)

    if (applicationRouterProp == null) {
      Logger.info("Using router with prod.routes")
      prodRoutes
    } else if (applicationRouterProp == "testOnlyDoNotUseInAppConf.Routes") {
      Logger.info("Using router with testOnlyDoNotUseInAppConf.routes")
      testOnlyDoNotUseInAppConfRoutes
    } else {
      Logger.error(s"The option $applicationRouterKey has unsupported value: $applicationRouterProp. We support only 'testOnlyDoNotUseInAppConf.Routes'. Using 'prodRoutes'.")
      prodRoutes
    }
  }

  val httpRequestHandler: HttpRequestHandler = new CustomHttpRequestHandler(
    router,
    errorHandlerModule.errorHandler,
    playBuiltInsModule.builtInComponents.httpConfiguration,
    frontendFiltersModule.httpFilters
  )

}
