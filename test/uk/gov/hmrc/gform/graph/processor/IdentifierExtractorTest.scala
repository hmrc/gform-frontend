/*
 * Copyright 2021 HM Revenue & Customs
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

import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class IdentifierExtractorTest extends Spec {

  it should "Retrieve enrolment value head option for a given service id when regime id do not exist" in new IdentifierExtractor {
    val enrolmentAuth = EnrolmentAuth(ServiceId("HMRC-ORG-OBTDS"), Never)

    identifierValue(enrolmentWithMultipleIdentifier, enrolmentAuth) should be("12XX567890")
  }

  it should "Retrieve enrolment value for a given service id and regime id when exist" in new IdentifierExtractor {
    val doCheck = DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("AB")))
    val enrolmentAuth = EnrolmentAuth(ServiceId("HMRC-ORG-OBTDS"), doCheck)

    identifierValue(enrolmentWithMultipleIdentifier, enrolmentAuth) should be("12AB567890")
  }

  it should "Retrieve enrolment value for a given service id and regime id when exist with multiple Enrolment" in new IdentifierExtractor {
    val doCheck = DoCheck(Always, RejectAccess, RegimeIdCheck(RegimeId("AA")))
    val enrolmentAuth = EnrolmentAuth(ServiceId("HMRC-ORG-OBTDS"), doCheck)

    identifierValue(multiEnrolments, enrolmentAuth) should be("12AA567890")
  }

  lazy val enrolmentWithMultipleIdentifier = Enrolments(
    Set(
      Enrolment("HMRC-ORG-OBTDS").copy(identifiers = List(
        EnrolmentIdentifier("EtmpRegistrationNumber", "12XX567890"),
        EnrolmentIdentifier("EtmpRegistrationNumber", "AABB567890"),
        EnrolmentIdentifier("UTR", "XXXXXXX"),
        EnrolmentIdentifier("EtmpRegistrationNumber", "12AB567890")
      ))))

  lazy val multiEnrolments = Enrolments(
    Set(
      Enrolment("HMRC-ORG-OBTDS").copy(
        identifiers = List(
          EnrolmentIdentifier("EtmpRegistrationNumber", "12BB567890")
        )),
      Enrolment("HMRC-ORG-OBTDS").copy(
        identifiers = List(
          EnrolmentIdentifier("EtmpRegistranntionNumber", "12AA567890")
        ))
    ))
}
