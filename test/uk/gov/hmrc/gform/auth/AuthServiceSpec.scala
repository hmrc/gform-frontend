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

import play.api.libs.json.{ JsBoolean, JsObject }
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.{ Enrolment, Enrolments }
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.gform.EeittService
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

import scala.concurrent.Future
import Function.const

class AuthServiceSpec extends Spec with ExampleData {

  behavior of "Authentication and authorisation Service"

  val appConfig = AppConfig(
    appName = "appName",
    `google-analytics` = null,
    `google-tag-manager` = null,
    `government-gateway-sign-in-url` = "government-gateway-sign-in-url",
    `gform-frontend-base-url` = "gform-frontend-base-url",
    `agent-subscription-frontend-base-url` = "agent-subscription-frontend-base-url",
    feature = null,
    formMaxAttachmentSizeMB = 1,
    /*we can't override list in app-config-base:*/
    contentTypesSeparatedByPipe = "csv|txt"
  )

  lazy val wSHttp = new StubbedWSHttp(
    HttpResponse(responseStatus = 200, responseJson = Some(JsObject(Map("isAllowed" -> JsBoolean(true))))))
  val mockEeittConnector = new EeittConnector("", wSHttp)
  val mockEeittDelegate = new EeittAuthorisationDelegate(mockEeittConnector, "/eeitt-frontend-base")
  val mockEeittService = new EeittService(mockEeittConnector)
  val authService = new AuthService(appConfig, mockEeittDelegate, mockEeittService)

  lazy val wSHttpNotAllowed = new StubbedWSHttp(
    HttpResponse(responseStatus = 200, responseJson = Some(JsObject(Map("isAllowed" -> JsBoolean(false))))))
  val mockEeittConnectorNotAllowed = new EeittConnector("", wSHttpNotAllowed)
  val mockEeittDelegateNotAllowed = new EeittAuthorisationDelegate(mockEeittConnectorNotAllowed, "/eeitt-frontend-base")
  val mockEeittServiceNotAllowed = new EeittService(mockEeittConnectorNotAllowed)
  val authServiceNotAllowed = new AuthService(appConfig, mockEeittDelegateNotAllowed, mockEeittServiceNotAllowed)

  implicit val hc = HeaderCarrier()

  implicit val request: Request[AnyContent] = null

  val legacyCredentials = OneTimeLogin

  val materialisedRetrievals =
    MaterialisedRetrievals(legacyCredentials, enrolments, None, None, None, userDetails, None, None)

  val getAffinityGroup: Unit => Future[Option[AffinityGroup]] = const(Future.successful(None))

  val materialisedRetrievalsAgent =
    MaterialisedRetrievals(
      legacyCredentials,
      enrolments,
      Some(uk.gov.hmrc.auth.core.AffinityGroup.Agent),
      None,
      None,
      userDetails,
      None,
      None)

  val materialisedRetrievalsEnrolledAgent =
    MaterialisedRetrievals(
      legacyCredentials,
      Enrolments(Set(Enrolment("HMRC-AS-AGENT"))),
      Some(uk.gov.hmrc.auth.core.AffinityGroup.Agent),
      None,
      None,
      userDetails,
      None,
      None)

  val materialisedRetrievalsOrganisation =
    MaterialisedRetrievals(
      legacyCredentials,
      enrolments,
      Some(uk.gov.hmrc.auth.core.AffinityGroup.Organisation),
      None,
      None,
      userDetails,
      None,
      None)

  val materialisedRetrievalsIndividual =
    MaterialisedRetrievals(
      legacyCredentials,
      enrolments,
      Some(uk.gov.hmrc.auth.core.AffinityGroup.Individual),
      None,
      None,
      userDetails,
      None,
      None)

  val requestUri = "/submissions/test"

  private def factory[A](a: A): PartialFunction[Throwable, AuthResult] => Predicate => Future[A] =
    const(const(Future.successful(a)))

  val ggAuthorisedSuccessful = factory(AuthSuccessful(materialisedRetrievals))
  val ggAuthorisedSuccessfulIndividual = factory(AuthSuccessful(materialisedRetrievalsIndividual))
  val ggAuthorisedSuccessfulOrganisation = factory(AuthSuccessful(materialisedRetrievalsOrganisation))
  val ggAuthorisedSuccessfulAgent = factory(AuthSuccessful(materialisedRetrievalsAgent))
  val ggAuthorisedSuccessfulEnrolledAgent = factory(AuthSuccessful(materialisedRetrievalsEnrolledAgent))
  val ggAuthorisedRedirect = factory(AuthRedirect(""))

  val enrolmentAuth = EnrolmentAuth(serviceId, DoCheck(Always, RejectAccess, RegimeIdCheck(regimeId)))

  val authConfigAgentDenied = HmrcAgentWithEnrolmentModule(DenyAnyAgentAffinityUser, enrolmentAuth)
  val formTemplateAgentDenied = formTemplate.copy(authConfig = authConfigAgentDenied)

  val authConfigAnyAgentAllowed = HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuth)
  val formTemplateAnyAgentAllowed = formTemplate.copy(authConfig = authConfigAnyAgentAllowed)

  val authConfigRequireMTDAgentEnrolment = HmrcAgentWithEnrolmentModule(RequireMTDAgentEnrolment, enrolmentAuth)
  val formTemplateRequireMTDAgentEnrolment = formTemplate.copy(authConfig = authConfigRequireMTDAgentEnrolment)

  val authEeitt = EeittModule(RegimeId("TT"))
  val formTemplateEeitt = formTemplate.copy(authConfig = authEeitt)

  it should "authorise a gg authentication only user when no agentAccess config" in {
    val result =
      authService.authenticateAndAuthorise(formTemplate, lang, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "authorise a gg authentication only non-agent when agent access is configured to agent denied" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateAgentDenied, lang, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "authorise a gg authentication only individual when agent access is configured to agent denied" in {
    val result = authService
      .authenticateAndAuthorise(
        formTemplateAgentDenied,
        lang,
        requestUri,
        getAffinityGroup,
        ggAuthorisedSuccessfulIndividual)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsIndividual))
  }

  it should "authorise a gg authentication only organisation when agent access is configured to agent denied" in {
    val result = authService
      .authenticateAndAuthorise(
        formTemplateAgentDenied,
        lang,
        requestUri,
        getAffinityGroup,
        ggAuthorisedSuccessfulOrganisation)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsOrganisation))
  }

  it should "block a gg authentication only agent when agent access is configured to agent denied" in {
    val result =
      authService
        .authenticateAndAuthorise(
          formTemplateAgentDenied,
          lang,
          requestUri,
          getAffinityGroup,
          ggAuthorisedSuccessfulAgent)
    result.futureValue should be(AuthBlocked("Agents cannot access this form"))
  }

  it should "authorise a gg authentication only agent when agent access is configured to allow any agent" in {
    val result = authService
      .authenticateAndAuthorise(
        formTemplateAnyAgentAllowed,
        lang,
        requestUri,
        getAffinityGroup,
        ggAuthorisedSuccessfulAgent)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsAgent))
  }

  it should "authorise a gg authentication only agent with enrolment when agent access is configured to allow agent with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          formTemplateRequireMTDAgentEnrolment,
          lang,
          requestUri,
          getAffinityGroup,
          ggAuthorisedSuccessfulEnrolledAgent)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolledAgent))
  }

  it should "redirect a gg authentication only agent with enrolment when agent access is configured to allow agent with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          formTemplateRequireMTDAgentEnrolment,
          lang,
          requestUri,
          getAffinityGroup,
          ggAuthorisedRedirect)
    result.futureValue should be(AuthRedirect(""))
  }

  it should "redirect a gg authentication only user when no agentAccess config" in {
    val result =
      authService.authenticateAndAuthorise(formTemplateEeitt, lang, requestUri, getAffinityGroup, ggAuthorisedRedirect)
    result.futureValue should be(AuthRedirect(""))
  }

  it should "authorise an eeitt authorised user when user is eeitt enrolled" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateEeitt, lang, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "redirect an eeitt authorised user when user is not eeitt enrolled" in {
    val result =
      authServiceNotAllowed
        .authenticateAndAuthorise(formTemplateEeitt, lang, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(
      AuthRedirectFlashingFormName(
        "/eeitt-frontend-base/eeitt-auth/enrollment-verification?callbackUrl=%2Fsubmissions%2Ftest"))
  }

  it should "redirect an eeitt authorised user when gg authentication fails" in {
    val result =
      authService.authenticateAndAuthorise(formTemplateEeitt, lang, requestUri, getAffinityGroup, ggAuthorisedRedirect)
    result.futureValue should be(AuthRedirect(""))
  }

}
