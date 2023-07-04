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

package uk.gov.hmrc.gform.testonly

import play.api.libs.ws.ahc.AhcWSComponents
import uk.gov.hmrc.gform.config.ConfigModule

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.builder.BuilderController
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.SectionRenderingService
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.validation.ComponentChecker

class TestOnlyModule(
  playBuiltInsModule: PlayBuiltInsModule,
  configModule: ConfigModule,
  gformBackendModule: GformBackendModule,
  controllersModule: ControllersModule,
  graphModule: GraphModule,
  lookupRegistry: LookupRegistry,
  ahcWSComponents: AhcWSComponents,
  fileUploadModule: FileUploadModule
)(implicit
  ec: ExecutionContext
) {

  private val sectionRenderingService: SectionRenderingService = new SectionRenderingService(
    configModule.frontendAppConfig,
    lookupRegistry
  )

  private val proxyActions = new ProxyActions(ahcWSComponents.wsClient)(controllersModule.messagesControllerComponents)

  val validationReportService = new ValidationService(
    fileUploadModule.fileUploadService,
    graphModule.booleanExprEval,
    gformBackendModule.gformConnector,
    lookupRegistry,
    graphModule.recalculation,
    playBuiltInsModule.i18nSupport,
    ComponentChecker.ErrorReportInterpreter
  )

  val testOnlyController = new TestOnlyController(
    playBuiltInsModule.i18nSupport,
    proxyActions,
    gformBackendModule.gformConnector,
    lookupRegistry,
    controllersModule.authenticatedRequestActions,
    configModule.serviceConfig,
    controllersModule.messagesControllerComponents
  )
  val testOnlyErrorMessageController = new TestOnlyErrorMessageController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    controllersModule.messagesControllerComponents,
    validationReportService
  )

  val debugController = new DebugController(
    controllersModule.authenticatedRequestActions,
    fileUploadModule.fileUploadService,
    controllersModule.messagesControllerComponents
  )

  val builderController: BuilderController =
    new BuilderController(
      controllersModule.authenticatedRequestActions,
      sectionRenderingService,
      playBuiltInsModule.i18nSupport,
      gformBackendModule.gformConnector,
      controllersModule.messagesControllerComponents
    )
}
