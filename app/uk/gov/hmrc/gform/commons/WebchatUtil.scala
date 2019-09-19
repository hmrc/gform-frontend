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

package uk.gov.hmrc.gform.commons

import play.api.libs.ws.{ WS, WSClient }
import uk.gov.hmrc.csp.config.ApplicationConfig
import uk.gov.hmrc.csp.{ CachedStaticHtmlPartialProvider, WebchatClient }
import uk.gov.hmrc.gform.InjectionDodge

object WebchatUtil {
  val wsClient: WSClient = {
    import play.api.Play.current
    WS.client
  }

  val appConfig = new ApplicationConfig(InjectionDodge.mode, InjectionDodge.configuration)
  val cachedStaticHtmlPartialProvider =
    new CachedStaticHtmlPartialProvider(wsClient, InjectionDodge.configuration, InjectionDodge.actorSystem)
  val WebchatClient = new WebchatClient(cachedStaticHtmlPartialProvider, appConfig)
}
