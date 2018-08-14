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

package uk.gov.hmrc.gform.auth

import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.config.{ AppConfig, FeatureToggle, GoogleAnalytics }
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.gform.gform.EeittService
import uk.gov.hmrc.play.config.ServicesConfig

class AuthServiceSpec extends Spec with ExampleData {

  behavior of "Authentication and authorisation Service"

  private val googleAnalytics = new GoogleAnalytics("token", "host")

  private val featureToggle = FeatureToggle(false, false)

  val appConfig = AppConfig(
    appName = "appName",
    `google-analytics` = googleAnalytics,
    `government-gateway-sign-in-url` = "government-gateway-sign-in-url",
    `gform-frontend-base-url` = "gform-frontend-base-url",
    `agent-subscription-frontend-base-url` = "agent-subscription-frontend-base-url",
    feature = featureToggle,
    formMaxAttachmentSizeMB = 1,
    /*we can't override list in app-config-base:*/
    contentTypesSeparatedByPipe = "csv|txt"
  )

  val mockEeittConnector = new EeittConnector("", null)

  val mockServicesConfig: ServicesConfig = new ServicesConfig {}

  val mockEeittDelegate: EeittAuthorisationDelegate =
    new EeittAuthorisationDelegate(mockEeittConnector, mockServicesConfig)

  val mockEeittService = new EeittService(mockEeittConnector)

  val authService = new AuthService(appConfig, mockEeittDelegate, mockEeittService)
}

trait Fixture extends ExampleData {
  def status: Int
  def responseJson: Option[JsValue]
  lazy val r = HttpResponse(
    responseStatus = status,
    responseJson = responseJson
  )
  implicit lazy val hc: HeaderCarrier = HeaderCarrier()
}
