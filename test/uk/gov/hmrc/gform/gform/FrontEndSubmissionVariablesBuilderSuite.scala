/*
 * Copyright 2024 HM Revenue & Customs
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

import munit.FunSuite
import org.apache.commons.text.StringEscapeUtils
import play.api.i18n.Messages
import play.api.libs.json.Json
import play.api.test.Helpers
import uk.gov.hmrc.auth.core.{ ConfidenceLevel, Enrolment, EnrolmentIdentifier, Enrolments }
import uk.gov.hmrc.gform.Helpers.mkDataOutOfDate
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, GovernmentGatewayId, MaterialisedRetrievals, OtherRetrievals }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.{ mkFormComponent, mkFormTemplate, mkSection }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FormModelSupport, VariadicFormDataSupport }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, FrontEndSubmissionVariables, LangADT }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllowAnyAgentAffinityUser, EnrolmentAuth, HmrcAgentWithEnrolmentModule, Never, ServiceId, ShortText, Text, UserCtx, UserField, Value }
import uk.gov.hmrc.http.SessionId

class FrontEndSubmissionVariablesBuilderSuite extends FunSuite with FormModelSupport with VariadicFormDataSupport {

  implicit val lang: LangADT = LangADT.En
  implicit val messages: Messages = Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty))

  test(
    "FrontEndSubmissionVariablesBuilder with AnonymousRetrievals should construct the correct JSON object with the backslash"
  ) {

    val retrievals: MaterialisedRetrievals = AnonymousRetrievals(SessionId("dummy-sessionId"))
    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Text(ShortText.default, Value)),
          mkFormComponent("b", Text(ShortText.default, Value))
        )
      )
    )
    val formTemplate = mkFormTemplate(sections)
    val data = mkDataOutOfDate(
      "a" -> "test",
      "b" -> "123"
    )

    val formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
      mkFormModelOpticsMongo(formTemplate, data)
    val customerId = CustomerId("""\/324%@£$£%$^%*&^(_^&@$\@£%*^&(&~+\nnewline\ttab""")

    val actual = FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, formModelVisibilityOptics, customerId)
    val expectedJson = Json.parse(s"""
                                     |{
                                     |  "user":
                                     |    {
                                     |      "enrolledIdentifier":"",
                                     |      "customerId": "${StringEscapeUtils.escapeJson(customerId.id)}"
                                     |     }
                                     |}""".stripMargin)
    val expected = FrontEndSubmissionVariables(expectedJson)

    assertEquals(actual, expected)
  }

  test("FrontEndSubmissionVariablesBuilder with AuthenticatedRetrievals should construct the correct JSON object") {

    val enrolmentAuth = EnrolmentAuth(ServiceId("IR-SA"), Never)
    val enrolmentIdentifierValue = "SA value"
    val irSaEnrolment = Enrolment("IR-SA").copy(identifiers = Seq(EnrolmentIdentifier("UTR", enrolmentIdentifierValue)))
    val userCtx = UserCtx(UserField.EnrolledIdentifier)
    val retrievals: MaterialisedRetrievals = AuthenticatedRetrievals(
      GovernmentGatewayId("id"),
      Enrolments(Set(irSaEnrolment)),
      AffinityGroup.Agent,
      "TestGroupId",
      None,
      OtherRetrievals.empty,
      ConfidenceLevel.L200,
      None
    )

    val sections = List(
      mkSection(
        List(
          mkFormComponent("a", Text(ShortText.default, Value)),
          mkFormComponent("b", Text(ShortText.default, userCtx))
        )
      )
    )
    val formTemplate =
      mkFormTemplate(sections).copy(authConfig = HmrcAgentWithEnrolmentModule(AllowAnyAgentAffinityUser, enrolmentAuth))

    val data = mkDataOutOfDate(
      "a" -> "test",
      "b" -> "123"
    )

    val formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo] =
      mkFormModelOpticsMongo(formTemplate, data)
    val customerId = CustomerId("""\/324%@£$£%$^%*&^(_^&@$\@£%*^&(&~+\nnewline\ttab""")

    val actual = FrontEndSubmissionVariablesBuilder(retrievals, formTemplate, formModelVisibilityOptics, customerId)
    val expectedJson = Json.parse(s"""
                                     |{
                                     |  "user":
                                     |    {
                                     |      "enrolledIdentifier": "$enrolmentIdentifierValue",
                                     |      "customerId": "${StringEscapeUtils.escapeJson(customerId.id)}"
                                     |     }
                                     |}""".stripMargin)
    val expected = FrontEndSubmissionVariables(expectedJson)

    assertEquals(actual, expected)
  }
}
