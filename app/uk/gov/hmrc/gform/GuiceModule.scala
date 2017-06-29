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

package uk.gov.hmrc.gform

import com.google.inject.AbstractModule
import uk.gov.hmrc.play.config.ServicesConfig
import uk.gov.hmrc.gform.controllers.{ BformsAuthenticationProvider, SecuredActions, SecuredActionsImpl }
import uk.gov.hmrc.play.frontend.auth.GovernmentGateway
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector

class GuiceModule extends AbstractModule with ServicesConfig {
  override def configure(): Unit = {
    bind(classOf[AuthConnector]).to(classOf[FrontendAuthConnector])
    bind(classOf[SecuredActions]).to(classOf[SecuredActionsImpl])
    bind(classOf[GovernmentGateway]).to(classOf[BformsAuthenticationProvider])
  }
}
