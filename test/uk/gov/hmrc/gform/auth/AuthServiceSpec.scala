/*
 * Copyright 2023 HM Revenue & Customs
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

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.auth.core.authorise.Predicate
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.models.mappings.{ NINO => MNINO, VATReg => MVATReg, _ }
import uk.gov.hmrc.gform.models.userdetails.Nino
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Individual
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.http.HeaderCarrier

import scala.Function.const
import scala.concurrent.Future
import scala.concurrent.duration.{ FiniteDuration, SECONDS }

class AuthServiceSpec extends ExampleData with Spec with TableDrivenPropertyChecks {

  behavior of "Authentication and authorisation Service"

  implicit val l: LangADT = LangADT.En
  implicit val hc: HeaderCarrier = HeaderCarrier()

  val appConfig = AppConfig(
    appName = "appName",
    `government-gateway-sign-in-url` = "government-gateway-sign-in-url",
    `gform-frontend-base-url` = "gform-frontend-base-url",
    `agent-subscription-frontend-base-url` = "agent-subscription-frontend-base-url",
    feature = null,
    formMaxAttachmentSizeMB = 1,
    `auth-module` = null,
    restrictedFileExtensionList = List("JFIF", "PNG"),
    albAdminIssuerUrl = "",
    `case-worker-assumed-identity-cookie` = "caseworker-assumed-identity",
    fileMaxUploadedSizeMB = 145,
    `proxy-timeout` = FiniteDuration.apply(120, SECONDS)
  )

  implicit val request: Request[AnyContent] = null

  val governmentGatewayId = GovernmentGatewayId("")

  val getAffinityGroup: Unit => Future[Option[AffinityGroup]] = const(Future.successful(None))

  val getGovernmentGatewayId: Unit => Future[Option[GovernmentGatewayId]] = const(Future.successful(None))

  private def materialisedRetrievalsBuilder(
    affinityGroup: AffinityGroup,
    enrolments: Enrolments,
    credentialRole: Option[CredentialRole],
    maybeNino: Option[Nino] = None,
    maybeConfidenceLevel: Option[ConfidenceLevel] = None
  ) =
    AuthenticatedRetrievals(
      governmentGatewayId,
      enrolments,
      affinityGroup,
      "TestGroupId",
      maybeNino,
      OtherRetrievals.empty,
      maybeConfidenceLevel.getOrElse(ConfidenceLevel.L50),
      credentialRole
    )

  val materialisedRetrievalsOfsted =
    AuthenticatedRetrievals(
      governmentGatewayId = governmentGatewayId,
      enrolments = enrolments,
      affinityGroup = uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Individual,
      groupIdentifier = "20e9b243-7471-4081-be1e-fcb5da33fd5a",
      maybeNino = None,
      otherRetrievals = OtherRetrievals.empty,
      ConfidenceLevel.L50,
      credentialRole = None
    )

  val materialisedRetrievalsAgent =
    materialisedRetrievalsBuilder(uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Agent, enrolments, None)

  val materialisedRetrievalsEnrolledAgent =
    materialisedRetrievalsBuilder(
      uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Agent,
      Enrolments(Set(Enrolment("HMRC-AS-AGENT"))),
      None
    )

  val materialisedRetrievalsIndividualWithSA =
    materialisedRetrievalsBuilder(
      Individual,
      Enrolments(Set(Enrolment("IR-SA"))),
      None
    )

  val materialisedRetrievalsIndividualCL200 =
    materialisedRetrievalsBuilder(Individual, enrolments, None, None, Some(ConfidenceLevel.L200))

  val materialisedRetrievalsIndividualCL200WithNino =
    materialisedRetrievalsBuilder(Individual, enrolments, None, Some(Nino("AB123456C")), Some(ConfidenceLevel.L200))

  val materialisedRetrievalsOrganisation =
    materialisedRetrievalsBuilder(uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Organisation, enrolments, None)

  val materialisedRetrievalsIndividual =
    materialisedRetrievalsBuilder(uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Individual, enrolments, None)

  val materialisedRetrievalsEnrolment =
    materialisedRetrievalsBuilder(
      uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Individual,
      Enrolments(
        Set(
          Enrolment("HMRC-ORG-OBTDS").copy(
            identifiers = List(EnrolmentIdentifier("EtmpRegistrationNumber", "12AB567890"))
          )
        )
      ),
      None
    )

  val materialisedRetrievalsEnrolmentOrgAssistant =
    materialisedRetrievalsBuilder(
      uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Organisation,
      Enrolments(
        Set(
          Enrolment("HMRC-ORG-OBTDS").copy(
            identifiers = List(EnrolmentIdentifier("EtmpRegistrationNumber", "12AB567890"))
          )
        )
      ),
      Some(Assistant)
    )

  val materialisedRetrievalsNoEnrolmentOrgAssistant =
    materialisedRetrievalsBuilder(
      uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Organisation,
      Enrolments(
        Set(
          Enrolment("HMRC-ORG-OBTDS").copy(
            identifiers = List(EnrolmentIdentifier("EtmpRegistrationNumber", "12NO567890"))
          )
        )
      ),
      Some(Assistant)
    )

  private def factory[A](a: A): PartialFunction[Throwable, AuthResult] => Predicate => Future[A] =
    const(const(Future.successful(a)))

  val ggAuthorisedSuccessful = factory(AuthSuccessful(materialisedRetrievals, Role.Customer))
  val ggAuthorisedSuccessfulIndividual = factory(AuthSuccessful(materialisedRetrievalsIndividual, Role.Customer))
  val ggAuthorisedSuccessfulIndividualWithSA = factory(
    AuthSuccessful(materialisedRetrievalsIndividualWithSA, Role.Customer)
  )
  val ggAuthorisedSuccessfulIndividualCL200 = factory(
    AuthSuccessful(materialisedRetrievalsIndividualCL200, Role.Customer)
  )
  val ggAuthorisedSuccessfulIndividualCL200WithNino = factory(
    AuthSuccessful(materialisedRetrievalsIndividualCL200WithNino, Role.Customer)
  )
  val ggAuthorisedSuccessfulOrganisation = factory(AuthSuccessful(materialisedRetrievalsOrganisation, Role.Customer))
  val ggAuthorisedSuccessfulAgent = factory(AuthSuccessful(materialisedRetrievalsAgent, Role.Customer))
  val ggAuthorisedSuccessfulEnrolledAgent = factory(AuthSuccessful(materialisedRetrievalsEnrolledAgent, Role.Customer))
  val ggAuthorisedRedirect = factory(AuthRedirect(""))
  val ggAuthorisedEnrolment = factory(AuthSuccessful(materialisedRetrievalsEnrolment, Role.Customer))
  val ggAuthorisedEnrolmentAssistant = factory(
    AuthSuccessful(materialisedRetrievalsEnrolmentOrgAssistant, Role.Customer)
  )
  val ggAuthorisedNoEnrolmentAssistant = factory(
    AuthSuccessful(materialisedRetrievalsNoEnrolmentOrgAssistant, Role.Customer)
  )

  val enrolmentAuthNoCheck = EnrolmentAuth(serviceId, DoCheck(Always, RejectAccess, NoCheck), enrolmentOutcomes)
  val enrolmentAuthCheck =
    EnrolmentAuth(
      ServiceId("HMRC-ORG-OBTDS"),
      DoCheck(Always, RequireEnrolment(enrolmentSection, NoAction), RegimeIdCheck(RegimeId("AB"))),
      enrolmentOutcomes
    )

  val ivUpliftRedirect = AuthRedirect(
    "/mdtp/uplift?origin=gForm&completionURL=%2Fnew-form%2Faaa999&failureURL=%2Fidentity-verification%2Ffailure%2Faaa999&confidenceLevel=200"
  )

  val authConfigAgentDenied = HmrcAgentWithEnrolmentModule(DenyAnyAgentAffinityUser, enrolmentAuthNoCheck)
  val formTemplateAgentDenied = buildFormTemplate.copy(authConfig = authConfigAgentDenied)

  val authConfigAnyAgentAllowed = HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuthNoCheck)
  val formTemplateAnyAgentAllowed = buildFormTemplate.copy(authConfig = authConfigAnyAgentAllowed)

  val authConfigRequireMTDAgentEnrolment = HmrcAgentWithEnrolmentModule(RequireMTDAgentEnrolment, enrolmentAuthNoCheck)
  val formTemplateRequireMTDAgentEnrolment = buildFormTemplate.copy(authConfig = authConfigRequireMTDAgentEnrolment)

  val authConfigEnrolment = HmrcAgentWithEnrolmentModule(RequireMTDAgentEnrolment, enrolmentAuthCheck)
  val authConfigEnrolmentOrg = HmrcEnrolmentModule(enrolmentAuthCheck)
  val formTemplateEnrolment = buildFormTemplate.copy(authConfig = authConfigEnrolment)
  val formTemplateEnrolmentOrg = buildFormTemplate.copy(authConfig = authConfigEnrolmentOrg)

  val authAWSALB: AuthConfig = AWSALBAuth
  val formTemplateAWSALB = buildFormTemplate.copy(authConfig = authAWSALB)

  val authConfigHmrcVerifiedMTDAgent = HmrcVerified(
    LocalisedString(Map(LangADT.En -> "test")),
    RequireMTDAgentEnrolment,
    "200",
    allowOrganisations = true,
    allowSAIndividuals = false
  )
  val authConfigHmrcVerifiedAnyAgent = HmrcVerified(
    LocalisedString(Map(LangADT.En -> "test")),
    AllowAnyAgentAffinityUser,
    "200",
    allowOrganisations = true,
    allowSAIndividuals = false
  )
  val authConfigHmrcVerifiedAllowSAIndividuals = HmrcVerified(
    LocalisedString(Map(LangADT.En -> "test")),
    DenyAnyAgentAffinityUser,
    "200",
    allowOrganisations = false,
    allowSAIndividuals = true
  )
  val authConfigHmrcVerifiedAllowOrganisations = HmrcVerified(
    LocalisedString(Map(LangADT.En -> "test")),
    DenyAnyAgentAffinityUser,
    "200",
    allowOrganisations = true,
    allowSAIndividuals = false
  )

  val authService = new AuthService(appConfig)

  it should "authorise a gg authentication only user when no agentAccess config" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate, None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessful,
        None
      )
    result.futureValue should be(AuthSuccessful(materialisedRetrievals, Role.Customer))
  }

  it should "authorise a gg authentication only non-agent when agent access is configured to agent denied" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateAgentDenied, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedSuccessful,
          None
        )
    result.futureValue should be(AuthSuccessful(materialisedRetrievals, Role.Customer))
  }

  it should "authorise a gg authentication only individual when agent access is configured to agent denied" in {
    val result = authService
      .authenticateAndAuthorise(
        FormTemplateContext.basicContext(formTemplateAgentDenied, None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulIndividual,
        None
      )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsIndividual, Role.Customer))
  }

  it should "authorise a gg authentication only organisation when agent access is configured to agent denied" in {
    val result = authService
      .authenticateAndAuthorise(
        FormTemplateContext.basicContext(formTemplateAgentDenied, None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulOrganisation,
        None
      )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsOrganisation, Role.Customer))
  }

  it should "block a gg authentication only agent when agent access is configured to agent denied" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateAgentDenied, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedSuccessfulAgent,
          None
        )
    result.futureValue should be(AuthBlocked("Agents cannot access this form"))
  }

  it should "authorise a gg authentication only agent when agent access is configured to allow any agent" in {
    val result = authService
      .authenticateAndAuthorise(
        FormTemplateContext.basicContext(formTemplateAnyAgentAllowed, None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulAgent,
        None
      )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsAgent, Role.Customer))
  }

  it should "authorise a gg authentication only agent with enrolment when agent access is configured to allow agent with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateRequireMTDAgentEnrolment, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedSuccessfulEnrolledAgent,
          None
        )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolledAgent, Role.Customer))
  }

  it should "authorise a gg authentication with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateEnrolment, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedEnrolment,
          None
        )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolment, Role.Customer))
  }

  it should "authorise a gg authentication with enrolment and Assistant Credential role" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateEnrolmentOrg, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedEnrolmentAssistant,
          None
        )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolmentOrgAssistant, Role.Customer))
  }

  it should "Redirect a gg authentication with Assistant Credential role with no enrolment to insuf creds to enrol page via /enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateEnrolmentOrg, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedNoEnrolmentAssistant,
          None
        )
    result.futureValue should be(AuthRedirect("/enrolment/aaa999"))
  }

  it should "redirect a gg authentication only agent with enrolment when agent access is configured to allow agent with enrolment" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateRequireMTDAgentEnrolment, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedRedirect,
          None
        )
    result.futureValue should be(AuthRedirect(""))
  }

  it should "not authorise an Ofsted user when they have not been successfully authenticated by the AWS ALB" in {
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateAWSALB, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedSuccessful,
          None
        )
    result.futureValue should be(AuthBlocked("You are not authorized to access this service"))
  }

  it should "authorise an Ofsted user when they have been successfully authenticated by the AWS ALB" in {
    val jwt =
      "eyJ0eXAiOiJKV1QiLCJraWQiOiI5MTgyZjVlZS0xMTBiLTQ3NWItOWUzNC02OTcyZjdjMTFhYjQiLCJhbGciOiJFUzI1NiIsImlzcyI6Imh0dHBzOi8vY29nbml0by1pZHAuZXUtd2VzdC0yLmFtYXpvbmF3cy5jb20vZXUtd2VzdC0yX0dpSDBTWTBqOSIsImNsaWVudCI6InZjdWZ1Zm05YjdvaTVzazJkdW1mZXJnbG8iLCJzaWduZXIiOiJhcm46YXdzOmVsYXN0aWNsb2FkYmFsYW5jaW5nOmV1LXdlc3QtMjo0MjAyMDgyNTUwODg6bG9hZGJhbGFuY2VyL2FwcC9hbGItZ2Zvcm1zLXFhLW9mc3RlZC8xZWRiY2FjNmQxNjVjYzkyIiwiZXhwIjoxNTU4MDg1ODQxfQ==.eyJzdWIiOiIyMGU5YjI0My03NDcxLTQwODEtYmUxZS1mY2I1ZGEzM2ZkNWEiLCJlbWFpbF92ZXJpZmllZCI6InRydWUiLCJlbWFpbCI6Im1pa2FpbC5raGFuQGRpZ2l0YWwuaG1yYy5nb3YudWsiLCJ1c2VybmFtZSI6IjIwZTliMjQzLTc0NzEtNDA4MS1iZTFlLWZjYjVkYTMzZmQ1YSIsImV4cCI6MTU1ODA4NTg0MSwiaXNzIjoiaHR0cHM6Ly9jb2duaXRvLWlkcC5ldS13ZXN0LTIuYW1hem9uYXdzLmNvbS9ldS13ZXN0LTJfR2lIMFNZMGo5In0=.7mBYP9FKoIPf7JBZ9qMcm90n1UNICrWyCBadi5xcjs0pKGGWFlDZLVKmvHSmpCK731JCrz49VPwDQYfsY_I_UA=="
    implicit val hc: HeaderCarrier = HeaderCarrier(otherHeaders = Seq(("X-Amzn-Oidc-Data" -> jwt)))
    val result =
      authService
        .authenticateAndAuthorise(
          FormTemplateContext.basicContext(formTemplateAWSALB, None),
          getAffinityGroup,
          getGovernmentGatewayId,
          ggAuthorisedSuccessful,
          None
        )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsOfsted, Role.Customer))
  }

  it should "block authorization for GG-authenticated users with hmrcVerified and AgentAccess configured with RequireMTDAgentEnrolment only" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedMTDAgent), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulAgent,
        None
      )
    result.futureValue should be(AuthRedirect(routes.AgentEnrolmentController.prologue(buildFormTemplate._id).url))
  }

  it should "authorize GG-authenticated users with hmrcVerified and AgentAccess configured with RequireMTDAgentEnrolment and valid enrolment" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAnyAgent), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulEnrolledAgent,
        None
      )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolledAgent, Role.Customer))
  }

  it should "authorize GG-authenticated users with hmrcVerified and AgentAccess configured with AllowAnyAgentAffinityUser" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAnyAgent), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulEnrolledAgent,
        None
      )
    result.futureValue should be(AuthSuccessful(materialisedRetrievalsEnrolledAgent, Role.Customer))
  }

  it should "authorize GG-authenticated CL200 Individual with hmrcVerified with a NINO" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAnyAgent), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulIndividualCL200WithNino,
        None
      )

    result.futureValue should be(AuthSuccessful(materialisedRetrievalsIndividualCL200WithNino, Role.Customer))
  }

  it should "redirect to IV uplift GG-authenticated CL200 Individual with hmrcVerified and no NINO" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAnyAgent), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulIndividualCL200,
        None
      )

    result.futureValue should be(ivUpliftRedirect)
  }

  it should "redirect to IV uplift GG-authenticated CL50 Individual with hmrcVerified and allowSAIndividuals set to false" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext.basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAnyAgent), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessful,
        None
      )

    result.futureValue should be(ivUpliftRedirect)
  }

  it should "authorize GG-authenticated CL50 Individual with hmrcVerified and allowSAIndividuals set to true" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext
          .basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAllowSAIndividuals), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulIndividualWithSA,
        None
      )

    result.futureValue should be(AuthSuccessful(materialisedRetrievalsIndividualWithSA, Role.Customer))
  }

  it should "authorize GG-authenticated Organisation with hmrcVerified and allowOrganisations set to true" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext
          .basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAllowOrganisations), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulOrganisation,
        None
      )

    result.futureValue should be(AuthSuccessful(materialisedRetrievalsOrganisation, Role.Customer))
  }

  it should "block GG-authenticated Organisation with hmrcVerified and allowOrganisations set to false" in {
    val result =
      authService.authenticateAndAuthorise(
        FormTemplateContext
          .basicContext(buildFormTemplate.copy(authConfig = authConfigHmrcVerifiedAllowSAIndividuals), None),
        getAffinityGroup,
        getGovernmentGatewayId,
        ggAuthorisedSuccessfulOrganisation,
        None
      )

    result.futureValue should be(AuthBlocked("Organisations cannot access this form"))
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
    (Enrolment("VATRegNo"), MVATReg(), List(EnrolmentIdentifier("VATRegNo", "888")), "888"),
    (Enrolment("HMRC-MTD-VAT"), VRN(), List(EnrolmentIdentifier("VRN", "444")), "444")
  )
}
