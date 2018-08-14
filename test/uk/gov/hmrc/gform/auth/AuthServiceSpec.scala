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

import play.api.mvc.{AnyContent, Request}
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.{AuthBlocked, AuthForbidden, AuthSuccessful, MaterialisedRetrievals}
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.gform.EeittService
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.config.ServicesConfig

import scala.concurrent.Future

class AuthServiceSpec extends Spec with ExampleData {

  behavior of "Authentication and authorisation Service"

  val appConfig = AppConfig(
    appName = "appName",
    `google-analytics` = null,
    `government-gateway-sign-in-url` = "government-gateway-sign-in-url",
    `gform-frontend-base-url` = "gform-frontend-base-url",
    `agent-subscription-frontend-base-url` = "agent-subscription-frontend-base-url",
    feature = null,
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

  implicit val hc = HeaderCarrier()

  val request: Request[AnyContent] = null

  val legacyCredentials = OneTimeLogin

  val materialisedRetrievals =
    MaterialisedRetrievals(legacyCredentials, enrolments, None, None, None, userDetails, None, None)

  val materialisedRetrievalsAgent =
    MaterialisedRetrievals(legacyCredentials, enrolments, Some(uk.gov.hmrc.auth.core.AffinityGroup.Agent), None, None, userDetails, None, None)

  def ggAuthorisedSuccessful(ggAuthorisedParams: GGAuthorisedParams) = Future.successful(AuthSuccessful(materialisedRetrievals))

  def ggAuthorisedSuccessfulAgent(ggAuthorisedParams: GGAuthorisedParams) = Future.successful(AuthSuccessful(materialisedRetrievalsAgent))

  val authConfigAgentDenied = HMRCAuthConfigWithRegimeId(authConfigModule, Some(DenyAnyAgentAffinityUser), serviceId, regimeId)
  val formTemplateAgentDenied = formTemplate.copy( authConfig = authConfigAgentDenied )

  val authConfigAnyAgentAllowed = HMRCAuthConfigWithRegimeId(authConfigModule, Some(AllowAnyAgentAffinityUser), serviceId, regimeId)
  val formTemplateAnyAgentAllowed = formTemplate.copy( authConfig = authConfigAnyAgentAllowed )

  it should "authorise a gg authentication only user when no agentAccess config" in {
    val result = authService.authenticateAndAuthorise(formTemplate, request, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "authorise a gg authentication only non-agent when agent access is configured to agent denied" in {
    val result = authService.authenticateAndAuthorise(formTemplateAgentDenied, request, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "block a gg authentication only agent when agent access is configured to agent denied" in {
    val result = authService.authenticateAndAuthorise(formTemplateAgentDenied, request, ggAuthorisedSuccessfulAgent)
    result.futureValue should be(AuthBlocked("Agents cannot access this form"))
  }

  it should "authorise a gg authentication only agent when agent access is configured to allow any agent" in {
    val result = authService.authenticateAndAuthorise(formTemplateAnyAgentAllowed, request, ggAuthorisedSuccessfulAgent)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsAgent))
  }



}
