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

import org.jsoup.Jsoup
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import uk.gov.hmrc.gform.it.stubs.{ FileUploadStubs, GFormStubs }
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService.DigitalContact
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailTemplateId }

class EmailAuthIT extends ITSpec with GFormStubs with FileUploadStubs {

  override implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(15, Seconds), interval = Span(500, Millis))

  "requesting new form configured with 'email' authModule" should "redirect user to 'enter email' page" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuth)

    And("Gform get form returns 404 not found")
    gformFormNotFoundStub(formTemplateEmailAuth)

    When("I request for a new form")
    val newFormResponse = get("/submissions/new-form/form-template-with-email-auth").send()

    And("I am redirected to the 'enter email' page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)
    val form = responseBody.getElementById("gf-form")
    form.attr(
      "action"
    ) shouldBe "/submissions/email-auth/send-email/form-template-with-email-auth?continue=%2Fsubmissions%2Fnew-form%2Fform-template-with-email-auth%3FemailSessionClear%3Dtrue"
    form.attr("method") shouldBe "POST"
    form.getElementsByClass("govuk-body").text shouldNot include("EmailUseInfo")
    val emailInput = form.getElementById("email")
    emailInput.nodeName() shouldBe "input"
    emailInput.attr("name") shouldBe "email"
    emailInput.attr("type") shouldBe "email"
    val submitButton = form.getElementsByAttributeValue("name", "submitButton").get(0)
    submitButton.nodeName() shouldBe "button"
    submitButton.attr("type") shouldBe "submit"
  }

  "requesting new form configured with 'email' authModule having optional EmailAuthConfig details" should "redirect user to 'enter email' page" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuthWithOptionalDetails)

    And("Gform get form returns 404 not found")
    gformFormNotFoundStub(formTemplateEmailAuth)

    When("I request for a new form")
    val newFormResponse = get("/submissions/new-form/form-template-with-email-auth").send()

    And("I am redirected to the 'enter email' page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)
    val form = responseBody.getElementById("gf-form")
    form.attr(
      "action"
    ) shouldBe "/submissions/email-auth/send-email/form-template-with-email-auth?continue=%2Fsubmissions%2Fnew-form%2Fform-template-with-email-auth%3FemailSessionClear%3Dtrue"
    form.attr("method") shouldBe "POST"
    form.getElementsByClass("govuk-body").text should include("EmailUseInfo")
    val emailInput = form.getElementById("email")
    emailInput.nodeName() shouldBe "input"
    emailInput.attr("name") shouldBe "email"
    emailInput.attr("type") shouldBe "email"
    val submitButton = form.getElementsByAttributeValue("name", "submitButton").get(0)
    submitButton.nodeName() shouldBe "button"
    submitButton.attr("type") shouldBe "submit"
  }

  "submitting 'enter email' form with valid email id" should "redirect user to 'confirm code' page" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuth)

    And("Gform get form returns 404 not found")
    gformFormNotFoundStub(formTemplateEmailAuth)

    And("Gform email notification service returns 204 NoContent")
    gformEmailStub(DigitalContact(EmailTemplateId("code_template"), None))

    When("I request for a new form and POST the 'enter email' form with an email id")
    val emailForm = get("/submissions/new-form/form-template-with-email-auth").send()
    val emailFormBody = Jsoup.parse(emailForm.body)
    val confirmCodeForm = post(
      s"email=test@test.com&csrfToken=${emailFormBody.fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    ).to(emailFormBody.getElementById("gf-form").attr("action")).send()

    Then("The 'confirm code' page is presented")
    val responseBody = Jsoup.parse(confirmCodeForm.body)
    val form = responseBody.getElementById("gf-form")
    form.attr(
      "action"
    ) shouldBe "/submissions/email-auth/confirm-code/form-template-with-email-auth?continue=%2Fsubmissions%2Fnew-form%2Fform-template-with-email-auth%3FemailSessionClear%3Dtrue"
    form.attr("method") shouldBe "POST"
    form.getElementsByClass("govuk-details").text shouldNot include("EmailCodeHelp")
    val emailInput = form.getElementById("email")
    emailInput.nodeName() shouldBe "input"
    emailInput.attr("name") shouldBe "email"
    emailInput.attr("type") shouldBe "hidden"
    emailInput.attr("value") shouldBe "test@test.com"
    val codeInput = form.getElementById("code")
    codeInput.nodeName() shouldBe "input"
    codeInput.attr("name") shouldBe "code"
    codeInput.attr("type") shouldBe "text"
    val submitButton = form.getElementsByAttributeValue("name", "submitButton").get(0)
    submitButton.nodeName() shouldBe "button"
    submitButton.attr("type") shouldBe "submit"
  }

  "submitting 'enter email' form with valid email id having optional EmailAuthConfig details" should "redirect user to 'confirm code' page" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuthWithOptionalDetails)

    And("Gform get form returns 404 not found")
    gformFormNotFoundStub(formTemplateEmailAuth)

    And("Gform email notification service returns 204 NoContent")
    gformEmailStub(DigitalContact(EmailTemplateId("code_template"), None))

    When("I request for a new form and POST the 'enter email' form with an email id")
    val emailForm = get("/submissions/new-form/form-template-with-email-auth").send()
    val emailFormBody = Jsoup.parse(emailForm.body)
    val confirmCodeForm = post(
      s"email=test@test.com&csrfToken=${emailFormBody.fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    ).to(emailFormBody.getElementById("gf-form").attr("action")).send()

    Then("The 'confirm code' page is presented")
    val responseBody = Jsoup.parse(confirmCodeForm.body)
    val form = responseBody.getElementById("gf-form")
    form.attr(
      "action"
    ) shouldBe "/submissions/email-auth/confirm-code/form-template-with-email-auth?continue=%2Fsubmissions%2Fnew-form%2Fform-template-with-email-auth%3FemailSessionClear%3Dtrue"
    form.attr("method") shouldBe "POST"
    form.getElementsByClass("govuk-details").text should include("EmailCodeHelp")
    val emailInput = form.getElementById("email")
    emailInput.nodeName() shouldBe "input"
    emailInput.attr("name") shouldBe "email"
    emailInput.attr("type") shouldBe "hidden"
    emailInput.attr("value") shouldBe "test@test.com"
    val codeInput = form.getElementById("code")
    codeInput.nodeName() shouldBe "input"
    codeInput.attr("name") shouldBe "code"
    codeInput.attr("type") shouldBe "text"
    val submitButton = form.getElementsByAttributeValue("name", "submitButton").get(0)
    submitButton.nodeName() shouldBe "button"
    submitButton.attr("type") shouldBe "submit"
  }

  "submitting 'confirm code' form with correct code" should "redirect to first page in 'form-template-with-email-auth' form" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuth)

    And("Gform get form returns 404 not found")
    gformFormNotFoundStub(formTemplateEmailAuth)

    And("Gform email notification service returns 204 NoContent")
    gformEmailStub(DigitalContact(EmailTemplateId("code_template"), None))

    And("Gform get form returns 200 OK")
    gformFormStub(formTemplateEmailAuth)

    And("FileUpload get envelopes returns 200 OK")
    getFileUploadEnvelopeStub()

    When("I request for a new form, enter email and submit the confirmation code")
    val emailForm = get("/submissions/new-form/form-template-with-email-auth").send()
    val emailFormBody = Jsoup.parse(emailForm.body)
    val confirmCodeForm = post(
      s"email=test@test.com&csrfToken=${emailFormBody.fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    )
      .to(emailFormBody.getElementById("gf-form").attr("action"))
      .send()
    val confirmCodeFormBody = Jsoup.parse(confirmCodeForm.body)
    val code = getRequestBody[ConfirmationCodeWithEmailService]("/gform/email").code.code
    val confirmCodeFormResponse = post(
      s"email=${confirmCodeFormBody.fieldValue("email")}&code=${code.toString.toLowerCase}&csrfToken=${confirmCodeFormBody
        .fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    ).to(confirmCodeFormBody.getElementById("gf-form").attr("action")).send()

    And("I am redirected successfully to the 'form-template-with-email-auth' form")
    confirmCodeFormResponse.status shouldBe 200
  }

  "submitting 'confirm code' form with correct code having optional EmailAuthConfig details" should "redirect user to 'email confirmation' page" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuthWithOptionalDetails)

    And("Gform get form returns 200 OK")
    gformFormStub(formTemplateEmailAuth)

    And("Gform email notification service returns 204 NoContent")
    gformEmailStub(DigitalContact(EmailTemplateId("code_template"), None))

    And("FileUpload get envelopes returns 200 OK")
    getFileUploadEnvelopeStub()

    When("I request for a new form, enter email and submit the confirmation code")
    val emailForm = get("/submissions/new-form/form-template-with-email-auth").send()
    val emailFormBody = Jsoup.parse(emailForm.body)
    val confirmCodeForm = post(
      s"email=test@test.com&csrfToken=${emailFormBody.fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    )
      .to(emailFormBody.getElementById("gf-form").attr("action"))
      .send()
    val confirmCodeFormBody = Jsoup.parse(confirmCodeForm.body)
    val code = getRequestBody[ConfirmationCodeWithEmailService]("/gform/email").code.code
    val emailConfirmationForm = post(
      s"email=${confirmCodeFormBody.fieldValue("email")}&code=${code.toString.toLowerCase}&csrfToken=${confirmCodeFormBody
        .fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    ).to(confirmCodeFormBody.getElementById("gf-form").attr("action")).send()

    Then("The 'email confirmation' page is presented")
    val responseBody = Jsoup.parse(emailConfirmationForm.body)
    val form = responseBody.getElementById("gf-form")
    form.attr(
      "action"
    ) shouldBe "/submissions/email-auth/email-confirmed-continue?continue=%2Fsubmissions%2Fnew-form%2Fform-template-with-email-auth%3FemailSessionClear%3Dtrue"
    form.attr("method") shouldBe "POST"
    form.getElementsByClass("govuk-body").text should include("EmailConfirmation")
    val submitButton = form.getElementsByAttributeValue("name", "submitButton").get(0)
    submitButton.nodeName() shouldBe "button"
    submitButton.attr("type") shouldBe "submit"
  }

  /*  "confirming on 'email confirmation' page" should "redirect to first page in 'form-template-with-email-auth' form" in {

    implicit val wsClient: StandaloneAhcWSClient = buildWSClient

    Given("I have a form template with 'email' auth")
    gformFormTemplateStub(formTemplateEmailAuthWithOptionalDetails)

    And("Gform email notification service returns 204 NoContent")
    gformEmailStub(DigitalContact(EmailTemplateId("code_template")))

    And("Gform get form returns 200 OK")
    gformFormStub(formTemplateEmailAuth)

    And("FileUpload get envelopes returns 200 OK")
    getFileUploadEnvelopeStub()

    When("I request for a new form, enter email and submit the confirmation code")
    val emailForm = get("/submissions/new-form/form-template-with-email-auth").send()
    val emailFormBody = Jsoup.parse(emailForm.body)
    val confirmCodeForm = post(
      s"email=test@test.com&csrfToken=${emailFormBody.fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    )
      .to(emailFormBody.getElementById("gf-form").attr("action"))
      .send()
    val confirmCodeFormBody = Jsoup.parse(confirmCodeForm.body)
    val code = getRequestBody[ConfirmationCodeWithEmailService]("/gform/email").code.code
    val emailConfirmationForm = post(
      s"email=${confirmCodeFormBody.fieldValue("email")}&code=${code.toString.toLowerCase}&csrfToken=${confirmCodeFormBody
        .fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    ).to(confirmCodeFormBody.getElementById("gf-form").attr("action")).send()
    val emailConfirmationFormBody = Jsoup.parse(emailConfirmationForm.body)
    val emailConfirmationFormResponse = post(
      s"csrfToken=${confirmCodeFormBody
        .fieldValue("csrfToken")}",
      Seq("Content-Type" -> "application/x-www-form-urlencoded")
    ).to(emailConfirmationFormBody.getElementById("gf-form").attr("action")).send()

    And("I am redirected successfully to the 'form-template-with-email-auth' form")
    emailConfirmationFormResponse.status shouldBe 200
  }*/
}
