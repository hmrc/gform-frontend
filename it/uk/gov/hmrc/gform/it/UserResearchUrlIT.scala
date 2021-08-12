package uk.gov.hmrc.gform.it

import org.jsoup.Jsoup
import play.api.libs.ws.ahc.StandaloneAhcWSClient
import uk.gov.hmrc.gform.it.stubs.{ FileUploadStubs, GFormStubs }

class UserResearchUrlIT extends ITSpec with GFormStubs with FileUploadStubs {

  implicit val wsClient: StandaloneAhcWSClient = buildWSClient

  "requesting new form with 'userResearchUrl'" should "display HMRC User Research Banner" in {

    Given("I have a form template with 'userResearchUrl'")
    gformFormTemplateStub(formTemplateEmailAuth)

    When("I request for a new form")
    val newFormResponse = get("/submissions/new-form/form-template-with-email-auth").send()

    And("I am redirected to the first page")
    newFormResponse.status shouldBe 200
    val responseBody = Jsoup.parse(newFormResponse.body)
    val form = responseBody.getElementsByClass("hmrc-user-research-banner__container")

    form.text should include("Help improve HMRC services")
    form.text should include("Sign up to take part in user research (opens in new tab)")
    form.text should include("No thanks")

    val link = responseBody.getElementsByClass("hmrc-user-research-banner__link")

    link.attr("href") shouldBe "https://test.service.gov.uk"
  }

  "requesting new form without 'userResearchUrl'" should "not display HMRC User Research Banner" in {

    Given("I have a form template without 'userResearchUrl'")
    gformFormTemplateStub(formTemplateEmailAuthWithoutUserResearchUrl)

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
