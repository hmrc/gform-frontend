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

package uk.gov.hmrc.gform.controllers

import javax.inject.Inject

import play.api.i18n.{ I18nSupport, MessagesApi }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule

class ControllersModule @Inject() (
    messagesApi: MessagesApi,
    configModule: ConfigModule,
    authModule: AuthModule,
    gformBackendModule: GformBackendModule
) {
  self =>

  //Instead of extending trait inject it and explicitly import it's implicits
  val i18nSupport: I18nSupport = new I18nSupport {
    override def messagesApi: MessagesApi = self.messagesApi
  }

  val authenticatedRequestActions = new AuthenticatedRequestActions(gformBackendModule.gformConnector, authModule, configModule, configModule.users, i18nSupport)
}

