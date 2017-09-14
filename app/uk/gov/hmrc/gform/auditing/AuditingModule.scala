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

package uk.gov.hmrc.gform.auditing

import javax.inject.Inject

import play.api.mvc.Headers
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.play.audit.http.HttpAuditing
import uk.gov.hmrc.play.audit.http.config.{ AuditingConfig, LoadAuditingConfig }
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.http.HeaderCarrier
import uk.gov.hmrc.play.http.hooks.HttpHook

class AuditingModule @Inject() (configModule: ConfigModule) { self =>

  lazy val auditConnectorImpl: AuditConnector = new AuditConnector {
    //WARN: LoadAuditingConfig uses play deprecations.
    //Thus you can not instantiate this class if play application is not running
    override def auditingConfig: AuditingConfig = LoadAuditingConfig(s"auditing")
  }

  lazy val httpAuditing: HttpAuditing = new HttpAuditing {
    override def auditConnector: AuditConnector = self.auditConnectorImpl
    override def appName: String = configModule.appConfig.appName
  }

  lazy val httpAuditingHook: HttpHook = httpAuditing.AuditingHook

  lazy val auditService = new AuditService {
    override def auditConnector = auditConnectorImpl
  }
}

object loggingHelpers {
  def cleanHeaders(headers: Headers) = s", headers: '${headers.remove("Authorization", "token")}'"
  def cleanHeaderCarrierHeader(hc: HeaderCarrier): String = s"headers: ' ${hc.sessionId} ${hc.deviceID} ${hc.requestId} ${hc.requestChain}'"
}
