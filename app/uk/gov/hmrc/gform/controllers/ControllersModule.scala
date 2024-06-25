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

package uk.gov.hmrc.gform.controllers

import play.api.BuiltInComponents
import play.api.i18n.MessagesApi
import play.api.mvc.{ DefaultMessagesActionBuilderImpl, DefaultMessagesControllerComponents, MessagesControllerComponents }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.graph.GraphModule
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule

import scala.concurrent.ExecutionContext

class ControllersModule(
  configModule: ConfigModule,
  authModule: AuthModule,
  gformBackendModule: GformBackendModule,
  playBuiltInsModule: PlayBuiltInsModule,
  builtInComponents: BuiltInComponents,
  errResponder: ErrResponder,
  graphModule: GraphModule,
  lookupOptions: LocalisedLookupOptions
)(implicit
  ec: ExecutionContext
) {

  private implicit val messagesApi: MessagesApi = builtInComponents.messagesApi

  val nonAuthenticatedRequestActions: NonAuthenticatedRequestActions = new NonAuthenticatedRequestActions(
    playBuiltInsModule.langs,
    builtInComponents.defaultActionBuilder
  )

  val authenticatedRequestActions: AuthenticatedRequestActions = new AuthenticatedRequestActions(
    gformBackendModule.gformConnector,
    authModule.authService,
    configModule.appConfig,
    authModule.authConnector,
    playBuiltInsModule.i18nSupport,
    playBuiltInsModule.langs,
    builtInComponents.defaultActionBuilder,
    errResponder,
    graphModule.recalculation,
    graphModule.smartStringEvaluatorFactory,
    lookupOptions
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
