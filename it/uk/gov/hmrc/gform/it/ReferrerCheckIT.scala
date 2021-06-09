/*
 * Copyright 2020 HM Revenue & Customs
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
package uk.gov.hmrc.gform.it

import cats.data.NonEmptyList
import org.jsoup.Jsoup
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.it.stubs.{ FileUploadStubs, GFormStubs }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ ReferrerConfig, ReferrerUrlPattern }

class ReferrerCheckIT extends ITSpec with GFormStubs with FileUploadStubs {

  "new form with referer config" should "Deny access if 'Referer' header is missing" in {
    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with referer config")
    gformFormTemplateStub(
      formTemplateAuthAnonymous.copy(referrerConfig =
        Some(
          ReferrerConfig(
            NonEmptyList.one(ReferrerUrlPattern("http://example.com")),
            toLocalisedString("Access denied")
          )
        )
      )
    )

    When("I request for a new form without 'Referer' header")
    val newFormResponse = get("/submissions/new-form/form-template-anonymous").send()

    Then("I am shown the HTTP 403 'Forbidden' page with message 'Access denied'")
    newFormResponse.status shouldBe 403
    newFormResponse.body contains "<p>Access denied</p>"
  }

  it should "Deny access if 'Referer' header is wrong" in {
    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with referer config")
    gformFormTemplateStub(
      formTemplateAuthAnonymous.copy(referrerConfig =
        Some(
          ReferrerConfig(
            NonEmptyList.one(ReferrerUrlPattern("http://example.com")),
            toLocalisedString("Access denied")
          )
        )
      )
    )

    When("I request for a new form with wrong 'Referer' header")
    val newFormResponse =
      get("/submissions/new-form/form-template-anonymous", Seq("Referer" -> "http://unknown-host.com")).send()

    Then("I am shown the HTTP 403 'Forbidden' page with message 'Access denied'")
    newFormResponse.status shouldBe 403
    newFormResponse.body contains "<p>Access denied</p>"
  }

  it should "Allow access if 'Referer' header matches exactly" in {
    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with referer config, form and envelope")
    gformFormTemplateStub(
      formTemplateAuthAnonymous.copy(referrerConfig =
        Some(
          ReferrerConfig(
            NonEmptyList.one(ReferrerUrlPattern("http://example.com")),
            toLocalisedString("Access denied")
          )
        )
      )
    )
    gformFormStub(formTemplateAuthAnonymous)
    getFileUploadEnvelopeStub()

    When("I request for a new form with correct 'Referer' header")
    val newFormResponse =
      get("/submissions/new-form/form-template-anonymous", Seq("Referer" -> "http://example.com")).send()

    Then("I am shown 'continue or start again' page")
    newFormResponse.status shouldBe 200
    val document = Jsoup.parse(newFormResponse.body)
    document.title() shouldBe "Do you want to continue your saved version? - Form with anonymous auth - GOV.UK"
  }

  it should "allow access if 'Referer' header matches via pattern match" in {
    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with referer config, form and envelope")
    gformFormTemplateStub(
      formTemplateAuthAnonymous.copy(referrerConfig =
        Some(
          ReferrerConfig(
            NonEmptyList.one(ReferrerUrlPattern("http://*.com")),
            toLocalisedString("Access denied")
          )
        )
      )
    )
    gformFormStub(formTemplateAuthAnonymous)
    getFileUploadEnvelopeStub()

    When("I request for a new form with correct 'Referer' header")
    val newFormResponse =
      get("/submissions/new-form/form-template-anonymous", Seq("Referer" -> "http://example.com")).send()

    Then("I am shown 'continue or start again' page")
    newFormResponse.status shouldBe 200
    val document = Jsoup.parse(newFormResponse.body)
    document.title() shouldBe "Do you want to continue your saved version? - Form with anonymous auth - GOV.UK"
  }
}
