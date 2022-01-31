/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.wshttp

import com.typesafe.config.ConfigValueFactory
import play.api.libs.ws.WSClient
import play.api.libs.ws.ahc.AhcWSComponents
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

class WSHttpModule(
  auditingModule: AuditingModule,
  configModule: ConfigModule,
  akkaModule: AkkaModule,
  ahcWSComponents: AhcWSComponents
) {
  val auditableWSHttp: WSHttp = new WSHttpImpl(
    configModule.appConfig.appName,
    auditingModule.auditConnector,
    configModule.typesafeConfig,
    akkaModule.actorSystem,
    wsClient
  )

  val auditableWSHttpCustomAppName: FormTemplateId => WSHttp = formTemplateId => {
    val appName = configModule.typesafeConfig.getString("appName")
    val config =
      configModule.typesafeConfig
        .withValue("appName", ConfigValueFactory.fromAnyRef(appName + "/" + formTemplateId.value))
    new WSHttpImpl(
      configModule.appConfig.appName,
      auditingModule.auditConnector,
      config,
      akkaModule.actorSystem,
      wsClient
    )
  }

  def wsClient: WSClient = ahcWSComponents.wsClient
}
