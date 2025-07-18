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

package uk.gov.hmrc.gform.graph.processor

/* import cats.Id
 * import cats.data.NonEmptyList
 * import cats.syntax.applicative._
 * import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
 * import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolment, EnrolmentIdentifier, Enrolments }
 * import uk.gov.hmrc.gform.Helpers.toSmartString
 * import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, GovernmentGatewayId }
 * import uk.gov.hmrc.gform.graph.{ NonConvertible, RecalculationOp }
 * import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowAnyAgentAffinityUser, Always, AuthConfig, DoCheck, EnrolmentAuth, EnrolmentSection, FormCtx, HmrcAgentWithEnrolmentModule, HmrcEnrolmentModule, HmrcSimpleModule, IdentifierRecipe, Never, NoAction, RegimeId, RegimeIdCheck, RequireEnrolment, ServiceId, UserCtx, UserField }
 * import uk.gov.hmrc.http.logging.SessionId
 */
import uk.gov.hmrc.auth.core.{ Assistant, ConfidenceLevel, Enrolments, User }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId, OtherRetrievals }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup
import uk.gov.hmrc.gform.sharedmodel.formtemplate.UserField

class UserCtxEvaluatorProcessorTest extends Spec {
  "processEvaluation" should "correctly retrieve and process a user credential role of 'User'" in {
    lazy val materialisedRetrievalsUser =
      AuthenticatedRetrievals(
        GovernmentGatewayId(""),
        Enrolments(Set.empty),
        AffinityGroup.Individual,
        "TestGroupId",
        None,
        OtherRetrievals(None),
        ConfidenceLevel.L50,
        Some(User)
      )

    val result: String =
      UserCtxEvaluatorProcessor.processEvaluation(materialisedRetrievalsUser, UserField.CredentialRole, authConfig)
    val expectedResult: String = "user"

    result shouldBe expectedResult
  }

  it should "correctly retrieve and process a user credential role of 'Assistant'" in {
    lazy val materialisedRetrievalsAssistant =
      AuthenticatedRetrievals(
        GovernmentGatewayId(""),
        Enrolments(Set.empty),
        AffinityGroup.Agent,
        "TestGroupId",
        None,
        OtherRetrievals(None),
        ConfidenceLevel.L50,
        Some(Assistant)
      )

    val result: String =
      UserCtxEvaluatorProcessor.processEvaluation(materialisedRetrievalsAssistant, UserField.CredentialRole, authConfig)
    val expectedResult: String = "assistant"

    result shouldBe expectedResult
  }

  it should "correctly retrieve and process an empty user credential role" in {
    lazy val materialisedRetrievalsEmpty =
      AuthenticatedRetrievals(
        GovernmentGatewayId(""),
        Enrolments(Set.empty),
        AffinityGroup.Individual,
        "TestGroupId",
        None,
        OtherRetrievals(None),
        ConfidenceLevel.L50,
        None
      )

    val result: String =
      UserCtxEvaluatorProcessor.processEvaluation(materialisedRetrievalsEmpty, UserField.CredentialRole, authConfig)
    val expectedResult: String = ""

    result shouldBe expectedResult
  }

  /*
   *   forAll(userCtxTable) { (enr, field, serviceId, authConfig, expected) =>
   *     it should s"evaluate a user ctx with $enr, $field, $serviceId and $authConfig return $expected" in new UserCtxEvaluatorProcessor[
   *       Id] {
   *       val retrievals: AuthenticatedRetrievals = materialisedRetrievalsAgent.copy(enrolments = Enrolments(Set(enr)))
   *
   *       processEvaluation(retrievals, UserCtx(field), authConfig) should be(expected)
   *     }
   *   }
   *
   *   it should "return an empty string when authModule in authConfig is set anonymous" in new UserCtxEvaluatorProcessor[Id] {
   *     processEvaluation(AnonymousRetrievals(SessionId("id")), UserCtx(UserField.EnrolledIdentifier), authConfig) should be(
   *       NonConvertible(RecalculationOp.noChange.pure[Id]))
   *   }
   *
   *   it should "return enrolledIdentifier value when authModule in authConfig are set" in new UserCtxEvaluatorProcessor[Id] {
   *     val retrievals: AuthenticatedRetrievals = materialisedRetrievalsAgent.copy(enrolments = Enrolments(multi))
   *     val enrSec =
   *       EnrolmentSection(toSmartString("title"), None, Nil, NonEmptyList.one(IdentifierRecipe("key", FormCtx(""))), Nil)
   *     val auth = HmrcEnrolmentModule(
   *       EnrolmentAuth(
   *         ServiceId("IR-CT"),
   *         DoCheck(Always, RequireEnrolment(enrSec, NoAction), RegimeIdCheck(RegimeId("XX")))))
   *
   *     processEvaluation(retrievals, UserCtx(UserField.EnrolledIdentifier), auth) should be(
   *       NonConvertible(RecalculationOp.newValue("12XX567890").pure[Id]))
   *   }
   *
   *   // format:off
   *   lazy val userCtxTable = Table(
   *     ("enr", "userField", "serviceId", "authConfig", "NoncovertibleResult"),
   *     (irctEnrolment, UserField.EnrolledIdentifier, ServiceId("IR-CT"), hmrcModule("IR-CT"), nonConvertible("CT value")),
   *     (irsaEnrolment, UserField.EnrolledIdentifier, ServiceId("IR-SA"), hmrcModule("IR-SA"), nonConvertible("SA value")),
   *     (
   *       irsaEnrolment,
   *       UserField.EnrolledIdentifier,
   *       ServiceId("IR-SA"),
   *       hmrcAgentWithEnrolmentModule,
   *       nonConvertible("SA value")),
   *     (irctEnrolment, UserField.EnrolledIdentifier, ServiceId(""), hmrcModule(""), nonConvertible("")),
   *     (irctEnrolment, UserField.AffinityGroup, ServiceId(""), HmrcSimpleModule, nonConvertible("agent"))
   *   )
   *   // format:on
   *
   *   lazy val hmrcModule: String => AuthConfig = id => HmrcEnrolmentModule(EnrolmentAuth(ServiceId(id), Never))
   *   lazy val nonConvertible: String => NonConvertible[Id] = value =>
   *     NonConvertible(RecalculationOp.newValue(value).pure[Id])
   *   lazy val hmrcAgentWithEnrolmentModule =
   *     HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, EnrolmentAuth(ServiceId("IR-SA"), Never))
   *
   *   lazy val materialisedRetrievalsAgent =
   *     AuthenticatedRetrievals(
   *       GovernmentGatewayId(""),
   *       Enrolments(Set(irsaEnrolment)),
   *       AffinityGroup.Agent,
   *       "TestGroupId",
   *       None
   *     )
   *
   *   lazy val irsaEnrolment = Enrolment("IR-SA").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "SA value")))
   *   lazy val irctEnrolment = Enrolment("IR-CT").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "CT value")))
   *
   *   lazy val multi = Set(
   *     Enrolment("IR-CT").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "12AB567890"))),
   *     Enrolment("IR-CT").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "12XX567890")))
   *   )
   */
}
