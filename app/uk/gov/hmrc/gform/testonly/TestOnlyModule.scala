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
import play.api.mvc.SessionCookieBaker

import uk.gov.hmrc.gform.config.ConfigModule

import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCryptoProvider
import uk.gov.hmrc.crypto.ApplicationCrypto

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.auth.AuthLoginStubConnector
import uk.gov.hmrc.gform.controllers.ControllersModule
import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.builder.BuilderController
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gform.SectionRenderingService
import uk.gov.hmrc.gform.gform.GformModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.validation.ComponentChecker
import uk.gov.hmrc.gform.wshttp.WSHttpModule
import snapshot.AuthLoginStubService

class TestOnlyModule(
  playBuiltInsModule: PlayBuiltInsModule,
  configModule: ConfigModule,
  gformBackendModule: GformBackendModule,
  controllersModule: ControllersModule,
  graphModule: GraphModule,
  lookupRegistry: LookupRegistry,
  ahcWSComponents: AhcWSComponents,
  fileUploadModule: FileUploadModule,
  gformModule: GformModule,
  wSHttpModule: WSHttpModule,
  applicationCrypto: ApplicationCrypto,
  sessionCookieBaker: SessionCookieBaker
)(implicit
  ec: ExecutionContext
) {

  private val sectionRenderingService: SectionRenderingService = new SectionRenderingService(
    configModule.frontendAppConfig,
    lookupRegistry
  )

  private val proxyActions = new ProxyActions(ahcWSComponents.wsClient)(controllersModule.messagesControllerComponents)

  val validationReportService = new ValidationService(
    graphModule.booleanExprEval,
    gformBackendModule.gformConnector,
    lookupRegistry,
    ComponentChecker.ErrorReportInterpreter
  )

  lazy val authLoginStubConnector: AuthLoginStubConnector = new AuthLoginStubConnector(
    configModule.serviceConfig.baseUrl("auth-login-stub"),
    wSHttpModule.wsClient
  )

  lazy val sessionCookieCrypto: SessionCookieCrypto = new SessionCookieCryptoProvider(applicationCrypto).get()
  lazy val authLoginStubService = new AuthLoginStubService(
    authLoginStubConnector,
    sessionCookieCrypto,
    sessionCookieBaker
  )

  val testOnlyController = new TestOnlyController(
    playBuiltInsModule.i18nSupport,
    proxyActions,
    gformBackendModule.gformConnector,
    lookupRegistry,
    controllersModule.authenticatedRequestActions,
    configModule.serviceConfig,
    configModule.frontendAppConfig,
    controllersModule.messagesControllerComponents,
    gformModule.newFormController,
    authLoginStubService
  )
  val testOnlyErrorMessageController = new TestOnlyErrorMessageController(
    playBuiltInsModule.i18nSupport,
    controllersModule.authenticatedRequestActions,
    controllersModule.messagesControllerComponents,
    validationReportService,
    lookupRegistry
  )

  val objectStoreAdminConnector = new ObjectStoreAdminConnector(
    wSHttpModule.wsClient
  )

  val objectStoreAdminController = new ObjectStoreAdminController(
    controllersModule.authenticatedRequestActions,
    configModule.frontendAppConfig,
    controllersModule.messagesControllerComponents,
    objectStoreAdminConnector,
    configModule.mode
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

  val formTemplateExtractController: FormTemplateExtractController =
    new FormTemplateExtractController(
      controllersModule.authenticatedRequestActions,
      controllersModule.messagesControllerComponents
    )
}
