/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import org.jsoup.Jsoup
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import uk.gov.hmrc.gform.it.ITSpec
import uk.gov.hmrc.gform.it.stubs.{ EnvelopeStubs, GFormStubs }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

class SpecimenWatermarkIT extends ITSpec with GFormStubs with EnvelopeStubs {
  implicit val wsClient: StandaloneAhcWSClient = buildWSClient

  val specimenTemplateId = FormTemplateId("specimen-" + formTemplateId.value)
  val specimenFormTemplate = formTemplateEmailAuth.copy(_id = specimenTemplateId)

  "Requesting new specimen form" should "display specimen watermark" in {
    Given("I have a specimen form template")
    gformFormTemplateStub(specimenFormTemplate)
    gformFormTemplateBehaviourStub(specimenFormTemplate._id)

    When("I request for a new form")
    val newFormResponse = get(s"/submissions/new-form/${specimenTemplateId.value}").send()

    And("I am redirected to the first page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)

    Then("There should be a style element within the head that sets the body background")
    val styleElement = responseBody.selectXpath("head/style")
    styleElement.size shouldBe 1
    styleElement.first.html should include("body {")
    styleElement.first.html should include(
      "background-image: url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' version='1.1' height='220px' width='220px'><text transform='translate(20, 220) rotate(-45)' stroke='rgb(245,45,45,0.1)' stroke-width='1.5px' fill='rgb(245,45,45,0.03)' font-family='GDS Transport, arial, sans-serif' font-size='40' font-weight='900'>SPECIMEN</text></svg>\");"
    )
  }

  "Requesting new non-specimen form" should "not display specimen watermark" in {
    Given("I have a non specimen form template")
    gformFormTemplateStub(formTemplateEmailAuth)
    gformFormTemplateBehaviourStub(formTemplateEmailAuth._id)

    When("I request for a new form")
    val newFormResponse = get(s"/submissions/new-form/${formTemplateEmailAuth._id.value}").send()

    And("I am redirected to the first page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)

    Then("There should not be a style element within the head")
    val styleElement = responseBody.selectXpath("head/style")
    styleElement.size shouldBe 0
  }
}
