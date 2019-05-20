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

package uk.gov.hmrc.gform.controllers

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gform.FormService
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule

class ControllersModule(
  configModule: ConfigModule,
  authModule: AuthModule,
  gformBackendModule: GformBackendModule,
  playBuiltInsModule: PlayBuiltInsModule,
  auditingModule: AuditingModule
)(
  implicit ec: ExecutionContext
) {

  val errResponder: ErrResponder = new ErrResponder(
    configModule.frontendAppConfig,
    auditingModule.httpAuditingService,
    playBuiltInsModule.i18nSupport
  )

  val errorHandler: ErrorHandler = new ErrorHandler(
    playBuiltInsModule.context.environment,
    playBuiltInsModule.context.initialConfiguration,
    playBuiltInsModule.context.sourceMapper,
    errResponder
  )

  val authenticatedRequestActions: AuthenticatedRequestActions = new AuthenticatedRequestActions(
    gformBackendModule.gformConnector,
    authModule.authService,
    configModule.appConfig,
    configModule.frontendAppConfig,
    authModule.authConnector,
    playBuiltInsModule.i18nSupport,
    playBuiltInsModule.langs,
    errResponder
  )
}
