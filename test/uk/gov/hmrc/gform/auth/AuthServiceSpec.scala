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

package uk.gov.hmrc.gform.auth

import play.api.libs.json.{ JsBoolean, JsObject }
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.{ Spec, SpecWithFakeApp }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.connectors.EeittConnector
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT }
import uk.gov.hmrc.gform.gform.EeittService
import uk.gov.hmrc.gform.models.mappings.{ NINO => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.gform.models.mappings.{ NINO => MNINO, VATReg => MVATReg }

import scala.concurrent.Future
import Function.const

class AuthServiceSpec extends ExampleData with SpecWithFakeApp {

  behavior of "Authentication and authorisation Service"

  implicit val l: LangADT = LangADT.En

  val appConfig = AppConfig(
    appName = "appName",
    `google-analytics` = null,
    `google-tag-manager` = null,
    `government-gateway-sign-in-url` = "government-gateway-sign-in-url",
    `gform-frontend-base-url` = "gform-frontend-base-url",
    `agent-subscription-frontend-base-url` = "agent-subscription-frontend-base-url",
    feature = null,
    formMaxAttachmentSizeMB = 1,
    `auth-module` = null,
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

  implicit val request: Request[AnyContent] = null

  val legacyCredentials = OneTimeLogin

  val getAffinityGroup: Unit => Future[Option[AffinityGroup]] = const(Future.successful(None))

  private def materialisedRetrievalsBuilder(affinityGroup: Option[AffinityGroup], enrolments: Enrolments) =
    AuthenticatedRetrievals(legacyCredentials, enrolments, affinityGroup, None, None, userDetails, None, None)

  val materialisedRetrievalsAgent =
    materialisedRetrievalsBuilder(Some(uk.gov.hmrc.auth.core.AffinityGroup.Agent), enrolments)

  val materialisedRetrievalsEnrolledAgent =
    materialisedRetrievalsBuilder(
      Some(uk.gov.hmrc.auth.core.AffinityGroup.Agent),
      Enrolments(Set(Enrolment("HMRC-AS-AGENT"))))

  val materialisedRetrievalsOrganisation =
    materialisedRetrievalsBuilder(Some(uk.gov.hmrc.auth.core.AffinityGroup.Organisation), enrolments)

  val materialisedRetrievalsIndividual =
    materialisedRetrievalsBuilder(Some(uk.gov.hmrc.auth.core.AffinityGroup.Individual), enrolments)

  val materialisedRetrievalsEnrolment =
    materialisedRetrievalsBuilder(
      Some(uk.gov.hmrc.auth.core.AffinityGroup.Individual),
      Enrolments(
        Set(Enrolment("HMRC-ORG-OBTDS").copy(
          identifiers = List(EnrolmentIdentifier("EtmpRegistrationNumber", "12AB567890")))))
    )

  val requestUri = "/submissions/test"

  private def factory[A](a: A): PartialFunction[Throwable, AuthResult] => Predicate => Future[A] =
    const(const(Future.successful(a)))

  val ggAuthorisedSuccessful = factory(AuthSuccessful(materialisedRetrievals))
  val ggAuthorisedSuccessfulIndividual = factory(AuthSuccessful(materialisedRetrievalsIndividual))
  val ggAuthorisedSuccessfulOrganisation = factory(AuthSuccessful(materialisedRetrievalsOrganisation))
  val ggAuthorisedSuccessfulAgent = factory(AuthSuccessful(materialisedRetrievalsAgent))
  val ggAuthorisedSuccessfulEnrolledAgent = factory(AuthSuccessful(materialisedRetrievalsEnrolledAgent))
  val ggAuthorisedRedirect = factory(AuthRedirect(""))
  val ggAuthorisedEnrolment = factory(AuthSuccessful(materialisedRetrievalsEnrolment))

  val enrolmentAuthNoCheck = EnrolmentAuth(serviceId, DoCheck(Always, RejectAccess, NoCheck))
  val enrolmentAuthCheck =
    EnrolmentAuth(ServiceId("HMRC-ORG-OBTDS"), DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("AB"))))

  val authConfigAgentDenied = HmrcAgentWithEnrolmentModule(DenyAnyAgentAffinityUser, enrolmentAuthNoCheck)
  val formTemplateAgentDenied = formTemplate.copy(authConfig = authConfigAgentDenied)

  val authConfigAnyAgentAllowed = HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuthNoCheck)
  val formTemplateAnyAgentAllowed = formTemplate.copy(authConfig = authConfigAnyAgentAllowed)

  val authConfigRequireMTDAgentEnrolment = HmrcAgentWithEnrolmentModule(RequireMTDAgentEnrolment, enrolmentAuthNoCheck)
  val formTemplateRequireMTDAgentEnrolment = formTemplate.copy(authConfig = authConfigRequireMTDAgentEnrolment)

  val authConfigEnrolment = HmrcAgentWithEnrolmentModule(RequireMTDAgentEnrolment, enrolmentAuthCheck)
  val formTemplateEnrolment = formTemplate.copy(authConfig = authConfigEnrolment)

  val authEeitt: AuthConfig = EeittModule(RegimeId("TT"))
  val formTemplateEeitt = formTemplate.copy(authConfig = authEeitt)

  val authAWSALB: AuthConfig = AWSALBAuth
  val formTemplateAWSALB = formTemplate.copy(authConfig = authAWSALB)

  it should "authorise a gg authentication only user when no agentAccess config" in {
    val result =
      authService.authenticateAndAuthorise(formTemplate, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "authorise a gg authentication only non-agent when agent access is configured to agent denied" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateAgentDenied, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "authorise a gg authentication only individual when agent access is configured to agent denied" in {
    val result = authService
      .authenticateAndAuthorise(formTemplateAgentDenied, requestUri, getAffinityGroup, ggAuthorisedSuccessfulIndividual)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsIndividual))
  }

  it should "authorise a gg authentication only organisation when agent access is configured to agent denied" in {
    val result = authService
      .authenticateAndAuthorise(
        formTemplateAgentDenied,
        requestUri,
        getAffinityGroup,
        ggAuthorisedSuccessfulOrganisation)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsOrganisation))
  }

  it should "block a gg authentication only agent when agent access is configured to agent denied" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateAgentDenied, requestUri, getAffinityGroup, ggAuthorisedSuccessfulAgent)
    result.futureValue should be(AuthBlocked("Agents cannot access this form"))
  }

  it should "authorise a gg authentication only agent when agent access is configured to allow any agent" in {
    val result = authService
      .authenticateAndAuthorise(formTemplateAnyAgentAllowed, requestUri, getAffinityGroup, ggAuthorisedSuccessfulAgent)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsAgent))
  }

  it should "authorise a gg authentication only agent with enrolment when agent access is configured to allow agent with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          formTemplateRequireMTDAgentEnrolment,
          requestUri,
          getAffinityGroup,
          ggAuthorisedSuccessfulEnrolledAgent)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolledAgent))
  }

  it should "authorise a gg authentication with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateEnrolment, requestUri, getAffinityGroup, ggAuthorisedEnrolment)
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolment))
  }

  it should "redirect a gg authentication only agent with enrolment when agent access is configured to allow agent with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          formTemplateRequireMTDAgentEnrolment,
          requestUri,
          getAffinityGroup,
          ggAuthorisedRedirect)
    result.futureValue should be(AuthRedirect(""))
  }

  it should "redirect a gg authentication only user when no agentAccess config" in {
    val result =
      authService.authenticateAndAuthorise(formTemplateEeitt, requestUri, getAffinityGroup, ggAuthorisedRedirect)
    result.futureValue should be(AuthRedirect(""))
  }

  it should "authorise an eeitt authorised user when user is eeitt enrolled" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateEeitt, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(materialisedRetrievals))
  }

  it should "redirect an eeitt authorised user when user is not eeitt enrolled" in {
    val result =
      authServiceNotAllowed
        .authenticateAndAuthorise(formTemplateEeitt, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(
      AuthRedirectFlashingFormName(
        "/eeitt-frontend-base/eeitt-auth/enrollment-verification?callbackUrl=%2Fsubmissions%2Ftest"))
  }

  it should "redirect an eeitt authorised user when gg authentication fails" in {
    val result =
      authService.authenticateAndAuthorise(formTemplateEeitt, requestUri, getAffinityGroup, ggAuthorisedRedirect)
    result.futureValue should be(AuthRedirect(""))
  }

  it should "not authorise an Ofsted user when they have not been successfully authenticated by the AWS ALB" in {
    val result =
      authService
        .authenticateAndAuthorise(formTemplateAWSALB, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthBlocked("You are not authorized to access this service"))
  }

  it should "authorise an Ofsted user when they have been successfully authenticated by the AWS ALB" in {
    val jwt =
      "eyJ0eXAiOiJKV1QiLCJraWQiOiI5MTgyZjVlZS0xMTBiLTQ3NWItOWUzNC02OTcyZjdjMTFhYjQiLCJhbGciOiJFUzI1NiIsImlzcyI6Imh0dHBzOi8vY29nbml0by1pZHAuZXUtd2VzdC0yLmFtYXpvbmF3cy5jb20vZXUtd2VzdC0yX0dpSDBTWTBqOSIsImNsaWVudCI6InZjdWZ1Zm05YjdvaTVzazJkdW1mZXJnbG8iLCJzaWduZXIiOiJhcm46YXdzOmVsYXN0aWNsb2FkYmFsYW5jaW5nOmV1LXdlc3QtMjo0MjAyMDgyNTUwODg6bG9hZGJhbGFuY2VyL2FwcC9hbGItZ2Zvcm1zLXFhLW9mc3RlZC8xZWRiY2FjNmQxNjVjYzkyIiwiZXhwIjoxNTU4MDg1ODQxfQ==.eyJzdWIiOiIyMGU5YjI0My03NDcxLTQwODEtYmUxZS1mY2I1ZGEzM2ZkNWEiLCJlbWFpbF92ZXJpZmllZCI6InRydWUiLCJlbWFpbCI6Im1pa2FpbC5raGFuQGRpZ2l0YWwuaG1yYy5nb3YudWsiLCJ1c2VybmFtZSI6IjIwZTliMjQzLTc0NzEtNDA4MS1iZTFlLWZjYjVkYTMzZmQ1YSIsImV4cCI6MTU1ODA4NTg0MSwiaXNzIjoiaHR0cHM6Ly9jb2duaXRvLWlkcC5ldS13ZXN0LTIuYW1hem9uYXdzLmNvbS9ldS13ZXN0LTJfR2lIMFNZMGo5In0=.7mBYP9FKoIPf7JBZ9qMcm90n1UNICrWyCBadi5xcjs0pKGGWFlDZLVKmvHSmpCK731JCrz49VPwDQYfsY_I_UA=="
    implicit val hc: HeaderCarrier = HeaderCarrier(otherHeaders = Seq(("x-amzn-oidc-data" -> jwt)))
    val result =
      authService
        .authenticateAndAuthorise(formTemplateAWSALB, requestUri, getAffinityGroup, ggAuthorisedSuccessful)
    result.futureValue should be(AuthSuccessful(AWSALBRetrievals("20e9b243-7471-4081-be1e-fcb5da33fd5a")))
  }

  forAll(taxTypeTable) { (enrolment, serviceName, identifiers, value) =>
    it should s"retrieve $value for an $enrolment with $identifiers for a given $serviceName" in {
      val retrievals =
        materialisedRetrievalsAgent.copy(enrolments = Enrolments(Set(enrolment.copy(identifiers = identifiers))))
      val actual = retrievals.getTaxIdValue(serviceName.asInstanceOf[ServiceNameAndTaxId])

      actual should be(value)
    }
  }

  lazy val taxTypeTable = Table(
    ("Enrolments", "service name", "Identifier", "TaxIdValue expected"),
    (Enrolment("IR-SA"), IRSA(), List(EnrolmentIdentifier("UTR", "321")), "321"),
    (Enrolment("IR-CT"), IRCT(), List(EnrolmentIdentifier("UTR", "888")), "888"),
    (Enrolment("HMRC-OBTDS-ORG"), HMRCOBTDSORG(), List(EnrolmentIdentifier("EtmpRegistrationNumber", "123")), "123"),
    (Enrolment("NINO"), MNINO(), List(EnrolmentIdentifier("NINO", "321")), "321"),
    (Enrolment("VATRegNo"), MVATReg(), List(EnrolmentIdentifier("VATRegNo", "888")), "888")
  )
}
