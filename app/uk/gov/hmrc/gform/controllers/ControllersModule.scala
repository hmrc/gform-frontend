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

package uk.gov.hmrc.gform.controllers

import play.api.BuiltInComponents
import play.api.mvc.{ DefaultMessagesActionBuilderImpl, DefaultMessagesControllerComponents, MessagesControllerComponents, SessionCookieBaker }

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.fileupload.FileUploadConnector
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class ControllersModule(
  configModule: ConfigModule,
  authModule: AuthModule,
  gformBackendModule: GformBackendModule,
  playBuiltInsModule: PlayBuiltInsModule,
  auditingModule: AuditingModule,
  builtInComponents: BuiltInComponents,
  sessionCookieBaker: SessionCookieBaker,
  errResponder: ErrResponder,
  graphModule: GraphModule,
  wSHttpModule: WSHttpModule
)(implicit
  ec: ExecutionContext
) {

  private implicit val messagesApi = builtInComponents.messagesApi

  val nonAuthenticatedRequestActions: NonAuthenticatedRequestActions = new NonAuthenticatedRequestActions(
    playBuiltInsModule.langs,
    builtInComponents.defaultActionBuilder
  )

  // Delete after 28 days of deployment of this change
  private lazy val fileUploadConnector = {
    lazy val fileUploadBaseUrl = {
      val baseUrl = configModule.serviceConfig.baseUrl("file-upload")
      val pathPrefix = configModule.serviceConfig.getConfString("file-upload.path-prefix", "")
      baseUrl + pathPrefix + "/file-upload"
    }
    new FileUploadConnector(wSHttpModule.auditableWSHttp, fileUploadBaseUrl)
  }

  val authenticatedRequestActions: AuthenticatedRequestActions = new AuthenticatedRequestActions(
    gformBackendModule.gformConnector,
    fileUploadConnector,
    authModule.authService,
    configModule.appConfig,
    configModule.frontendAppConfig,
    authModule.authConnector,
    playBuiltInsModule.i18nSupport,
    playBuiltInsModule.langs,
    builtInComponents.defaultActionBuilder,
    errResponder,
    sessionCookieBaker,
    graphModule.recalculation,
    graphModule.smartStringEvaluatorFactory
  )

  val messagesControllerComponents: MessagesControllerComponents = new DefaultMessagesControllerComponents(
    new DefaultMessagesActionBuilderImpl(builtInComponents.defaultBodyParser, playBuiltInsModule.messagesApi),
    builtInComponents.defaultActionBuilder,
    builtInComponents.playBodyParsers,
    playBuiltInsModule.messagesApi,
    playBuiltInsModule.langs,
    builtInComponents.fileMimeTypes,
    ec
  )

}
