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

package uk.gov.hmrc.gform.graph.processor

import cats.Id
import cats.data.NonEmptyList
import cats.syntax.applicative._
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments, AffinityGroup => CoreAffinityGroup }
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals }
import uk.gov.hmrc.gform.graph.{ NoChange, NonConvertible, RecalculationOp }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowAnyAgentAffinityUser, Always, AuthConfig, DoCheck, EnrolledIdentifier, EnrolmentAuth, EnrolmentSection, FormCtx, HmrcAgentWithEnrolmentModule, HmrcEnrolmentModule, HmrcSimpleModule, IdentifierRecipe, Never, NoAction, RegimeId, RegimeIdCheck, RequireEnrolment, ServiceId, UserCtx, AffinityGroup => FTAffinityGroup }
import uk.gov.hmrc.http.logging.SessionId

class UserCtxEvaluatorProcessorTest extends Spec {

  forAll(userCtxTable) { (enr, field, serviceId, authConfig, expected) =>
    it should s"evaluate a user ctx with $enr, $field, $serviceId and $authConfig return $expected" in new UserCtxEvaluatorProcessor[
      Id] {
      val retrievals: AuthenticatedRetrievals = materialisedRetrievalsAgent.copy(enrolments = Enrolments(Set(enr)))

      processEvaluation(retrievals, UserCtx(field), authConfig) should be(expected)
    }
  }

  it should "return an empty string when authModule in authConfig is set anonymous" in new UserCtxEvaluatorProcessor[Id] {
    processEvaluation(AnonymousRetrievals(SessionId("id")), UserCtx(EnrolledIdentifier), authConfig) should be(
      NonConvertible(RecalculationOp.noChange.pure[Id]))
  }

  it should "return enrolledIdentifier value when authModule in authConfig are set" in new UserCtxEvaluatorProcessor[Id] {
    val retrievals: AuthenticatedRetrievals = materialisedRetrievalsAgent.copy(enrolments = Enrolments(multi))
    val enrSec = EnrolmentSection(
      toLocalisedString("title"),
      None,
      Nil,
      NonEmptyList.one(IdentifierRecipe("key", FormCtx(""))),
      Nil)
    val auth = HmrcEnrolmentModule(
      EnrolmentAuth(
        ServiceId("IR-CT"),
        DoCheck(Always, RequireEnrolment(enrSec, NoAction), RegimeIdCheck(RegimeId("XX")))))

    processEvaluation(retrievals, UserCtx(EnrolledIdentifier), auth) should be(
      NonConvertible(RecalculationOp.newValue("12XX567890").pure[Id]))
  }

  // format:off
  lazy val userCtxTable = Table(
    ("enr", "userField", "serviceId", "authConfig", "NoncovertibleResult"),
    (irctEnrolment, EnrolledIdentifier, ServiceId("IR-CT"), hmrcModule("IR-CT"), nonConvertible("CT value")),
    (irsaEnrolment, EnrolledIdentifier, ServiceId("IR-SA"), hmrcModule("IR-SA"), nonConvertible("SA value")),
    (irsaEnrolment, EnrolledIdentifier, ServiceId("IR-SA"), hmrcAgentWithEnrolmentModule, nonConvertible("SA value")),
    (irctEnrolment, EnrolledIdentifier, ServiceId(""), hmrcModule(""), nonConvertible("")),
    (irctEnrolment, FTAffinityGroup, ServiceId(""), HmrcSimpleModule, nonConvertible("agent"))
  )
  // format:on

  lazy val hmrcModule: String => AuthConfig = id => HmrcEnrolmentModule(EnrolmentAuth(ServiceId(id), Never))
  lazy val nonConvertible: String => NonConvertible[Id] = value =>
    NonConvertible(RecalculationOp.newValue(value).pure[Id])
  lazy val hmrcAgentWithEnrolmentModule =
    HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, EnrolmentAuth(ServiceId("IR-SA"), Never))

  lazy val materialisedRetrievalsAgent = AuthenticatedRetrievals(
    OneTimeLogin,
    Enrolments(Set(irsaEnrolment)),
    Some(CoreAffinityGroup.Agent),
    None,
    None,
    userDetails,
    None,
    None)

  lazy val irsaEnrolment = Enrolment("IR-SA").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "SA value")))
  lazy val irctEnrolment = Enrolment("IR-CT").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "CT value")))

  lazy val multi = Set(
    Enrolment("IR-CT").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "12AB567890"))),
    Enrolment("IR-CT").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "12XX567890")))
  )
}
