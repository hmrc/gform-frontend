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
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments, AffinityGroup => CoreAffinityGroup }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals }
import uk.gov.hmrc.gform.graph.NonConvertible
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowAnyAgentAffinityUser, EnrolledIdentifier, EnrolmentAuth, HmrcAgentWithEnrolmentModule, HmrcEnrolmentModule, HmrcSimpleModule, Never, ServiceId, UserCtx, AffinityGroup => FTAffinityGroup }
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
      NonConvertible(""))
  }

  lazy val userCtxTable = Table(
    ("enr", "userField", "serviceId", "authConfig", "NoncovertibleResult"),
    (
      irctEnrolment,
      EnrolledIdentifier,
      ServiceId("IR-CT"),
      HmrcEnrolmentModule(EnrolmentAuth(ServiceId("IR-CT"), Never)),
      NonConvertible("CT value")),
    (
      irsaEnrolment,
      EnrolledIdentifier,
      ServiceId("IR-SA"),
      HmrcEnrolmentModule(EnrolmentAuth(ServiceId("IR-SA"), Never)),
      NonConvertible("SA value")),
    (
      irsaEnrolment,
      EnrolledIdentifier,
      ServiceId("IR-SA"),
      HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, EnrolmentAuth(ServiceId("IR-SA"), Never)),
      NonConvertible("SA value")),
    (
      irctEnrolment,
      EnrolledIdentifier,
      ServiceId(""),
      HmrcEnrolmentModule(EnrolmentAuth(ServiceId(""), Never)),
      NonConvertible("")),
    (irctEnrolment, FTAffinityGroup, ServiceId(""), HmrcSimpleModule, NonConvertible("agent"))
  )

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
}
