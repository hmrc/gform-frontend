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

package uk.gov.hmrc.gform.gform

import com.softwaremill.quicklens._
import play.api.libs.json.Json
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.auth.core.{ Enrolment, EnrolmentIdentifier, Enrolments, AffinityGroup => CoreAffinityGroup }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.AuthenticatedRetrievals
import uk.gov.hmrc.gform.graph.processor.IdentifierExtractor
import FrontEndSubmissionVariablesBuilder._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormTemplateGen
import uk.gov.hmrc.gform.sharedmodel.FrontEndSubmissionVariables

class FrontEndSubmissionVariablesBuilderTest extends Spec with FormTemplateGen {

  forAll(formTemplateGen) { template =>
    it should s"Build a data structure with valid key value pair for ${template._id}" in new IdentifierExtractor {
      val userCtx = UserCtx(uk.gov.hmrc.gform.sharedmodel.formtemplate.EnrolledIdentifier)
      val enrolledIdType: ComponentType = Text(BasicText, userCtx)
      val retrievals: AuthenticatedRetrievals =
        materialisedRetrievalsAgent.copy(enrolments = Enrolments(Set(irsaEnrolment)))
      val enrolmentAuth = EnrolmentAuth(ServiceId("IR-SA"), Never)

      val actual = FrontEndSubmissionVariablesBuilder(
        retrievals,
        template
          .modify(_.sections.each.fields.each.`type`)
          .setTo(enrolledIdType)
          .modify(_.authConfig)
          .setTo(HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuth)),
        CustomerId("cid")
      )

      processContext(retrievals, HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuth)) shouldBe "SA value"
      actual shouldBe FrontEndSubmissionVariables(
        Json.parse("""{"user":{"enrolledIdentifier":"SA value","customerId":"cid"}}"""))
    }
  }

  forAll(formTemplateGen, formComponentGen(), formComponentGen()) { (template, cmp1, cmp2) =>
    it should s"Build a data structure with valid key value pair for ${template._id} with multiple fields type" in new IdentifierExtractor {
      val userCtx = UserCtx(uk.gov.hmrc.gform.sharedmodel.formtemplate.EnrolledIdentifier)
      val enrolledId: ComponentType = Text(BasicText, userCtx)
      val valueComponentType: ComponentType = Text(BasicText, Value)
      val retrievals: AuthenticatedRetrievals =
        materialisedRetrievalsAgent.copy(enrolments = Enrolments(Set(irsaEnrolment)))
      val enrolmentAuth = EnrolmentAuth(ServiceId("IR-SA"), Never)

      val usrCtxComponent = cmp1.modify(_.`type`).setTo(enrolledId)
      val valueComponent = cmp2.modify(_.`type`).setTo(valueComponentType)

      val templateWithAtLeastTwoFields =
        template
          .modify(_.sections.each.fields)
          .setTo(List(valueComponent, usrCtxComponent))
          .modify(_.authConfig)
          .setTo(HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuth))

      val actual = FrontEndSubmissionVariablesBuilder(retrievals, templateWithAtLeastTwoFields, CustomerId("cid"))
      actual shouldBe FrontEndSubmissionVariables(
        Json.parse("""{"user":{"enrolledIdentifier":"SA value","customerId":"cid"}}"""))
    }
  }

  val irsaEnrolment = Enrolment("IR-SA").copy(identifiers = Seq(EnrolmentIdentifier("UTR", "SA value")))

  val materialisedRetrievalsAgent = AuthenticatedRetrievals(
    OneTimeLogin,
    Enrolments(Set(irsaEnrolment)),
    Some(CoreAffinityGroup.Agent),
    None,
    None,
    userDetails,
    None,
    None
  )

}
