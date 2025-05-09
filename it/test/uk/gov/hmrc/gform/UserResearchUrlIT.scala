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

package uk.gov.hmrc.gform.it

import org.jsoup.Jsoup
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import uk.gov.hmrc.gform.it.stubs.{ EnvelopeStubs, GFormStubs }

class UserResearchUrlIT extends ITSpec with GFormStubs with EnvelopeStubs {

  implicit val wsClient: StandaloneAhcWSClient = buildWSClient

  "requesting new form with 'userResearchUrl'" should "display HMRC User Research Banner" in {

    Given("I have a form template with 'userResearchUrl'")
    gformFormTemplateStub(formTemplateEmailAuth)
    gformFormTemplateBehaviourStub(formTemplateEmailAuth._id)

    When("I request for a new form")
    val newFormResponse = get("/submissions/new-form/form-template-with-email-auth").send()

    And("I am redirected to the first page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)
    val form = responseBody.getElementsByClass("hmrc-user-research-banner__container")

    form.text should include("Help make GOV.UK better")
    form.text should include("You could be given a Love2Shop voucher as a thank you for feedback (opens in new tab)")
    form.text should include("Hide message Hide message.")
    form.text should include("I do not want to take part in research")

    val link = responseBody.getElementsByClass("hmrc-user-research-banner__link")

    link.attr("href") shouldBe "https://test.service.gov.uk"
  }

  "requesting new form without 'userResearchUrl'" should "not display HMRC User Research Banner" in {

    Given("I have a form template without 'userResearchUrl'")
    gformFormTemplateStub(formTemplateEmailAuthWithoutUserResearchUrl)
    gformFormTemplateBehaviourStub(formTemplateEmailAuthWithoutUserResearchUrl._id)

    When("I request for a new form")
    val newFormResponse = get("/submissions/new-form/form-template-with-email-auth").send()

    And("I am redirected to the first page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)
    val form = responseBody.getElementsByClass("hmrc-user-research-banner__container")

    form.text shouldNot include("Help improve HMRC services")
    form.text shouldNot include("Sign up to take part in user research (opens in new tab)")
    form.text shouldNot include("No thanks")
  }
}
