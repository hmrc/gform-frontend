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
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.eeitt.{ Agent, BusinessUser }
import uk.gov.hmrc.gform.models.userdetails.GroupId
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate.RegimeId
import uk.gov.hmrc.gform.wshttp.{ StubbedWSHttp, WSHttp }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import uk.gov.hmrc.gform.auth.AuthConnector

import scala.concurrent.{ ExecutionContext, Future }

class AuthServiceSpec extends Spec with ExampleData {

  behavior of "Authentication and authorisation Service"

  lazy val wSHttp = new StubbedWSHttp(HttpResponse(0))

  val mockGformConnector = new GformConnector(wSHttp, "baseUrl") {
//    override def prepopulationBusinessUser(groupId: GroupId, regimeId: RegimeId)(
//      implicit hc: HeaderCarrier,
//      ec: ExecutionContext): Future[BusinessUser] =
//      Future.successful(BusinessUser("TESTREGNUM"))

  }

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

//  val authConnector = AuthConnector()
//
//
//  val authService = new AuthService(mockGformConnector, appConfig, frontendAppConfig)
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
