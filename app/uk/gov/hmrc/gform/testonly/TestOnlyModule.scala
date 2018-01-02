/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.controllers.helpers.ProxyActions
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.playcomponents.PlayBuiltInsModule

class TestOnlyModule(
    playBuiltInsModule: PlayBuiltInsModule,
    gformBackendModule: GformBackendModule
) {

  private val proxyActions = new ProxyActions(playBuiltInsModule.ahcWSComponents.wsClient)

  val testOnlyController = new TestOnlyController(
    proxyActions,
    gformBackendModule.gformConnector
  )

}
