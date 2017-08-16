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

package uk.gov.hmrc.gform.prepop

import javax.inject.Inject
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.wshttp.WSHttpModule

class PrepopModule @Inject() (
    configModule: ConfigModule,
    wSHttpModule: WSHttpModule
) {

  lazy val prepopService = new PrepopService(eeittConnector, authContextPrepop)

  /********************* private *********************/

  private val authContextPrepop = new AuthContextPrepop

  private lazy val eeittConnector = new EeittConnector(
    configModule.serviceConfig.baseUrl("eeitt"),
    wSHttpModule.auditableWSHttp
  )
}
