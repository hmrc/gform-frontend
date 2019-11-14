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

package uk.gov.hmrc.gform.auditing

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.gform.akka.AkkaModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.bootstrap.audit.DefaultAuditConnector

class AuditingModule(configModule: ConfigModule, akkaModule: AkkaModule, playBuiltInsModule: PlayBuiltInsModule)(
  implicit ec: ExecutionContext
) {
  self =>

  lazy val auditConnector: AuditConnector =
    new DefaultAuditConnector(configModule.auditingConfig)

  lazy val auditService = new AuditService {
    override def auditConnector = self.auditConnector
  }

  val httpAuditingService: HttpAuditingService = new HttpAuditingService(configModule.appConfig.appName, auditConnector)
}
