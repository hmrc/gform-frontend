package uk.gov.hmrc.gform.it.stubs

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import uk.gov.hmrc.gform.it.sample.FormTemplateSample
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService.DigitalContact
import uk.gov.hmrc.gform.sharedmodel.UserId
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailConfirmationCode, EmailTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, JsonUtils }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import org.typelevel.ci._

trait GFormStubs extends FormTemplateSample {

  def gformFormTemplateStub(formTemplate: FormTemplate) =
    stubFor(
      WireMock
        .get(s"/gform/formtemplates/${formTemplate._id.value}")
        .willReturn(ok(JsonUtils.toJsonStr(formTemplate)))
    )

  def gformFormStub(formTemplate: FormTemplate) =
    stubFor(
      WireMock
        .get(urlMatching(s"/gform/forms/.*/${formTemplate._id.value}"))
        .willReturn(
          ok(
            JsonUtils.toJsonStr(
              Form(
                FormId("some-form-id"),
                EnvelopeId("some-envelope-id"),
                UserId("some-user-id"),
                formTemplate._id,
                FormData(List.empty),
                InProgress,
                VisitIndex(Set.empty),
                ThirdPartyData.empty,
                None,
                FormComponentIdToFileIdMapping.empty
              )
            )
          )
        )
    )

  def gformNewFormStub(formTemplate: FormTemplate) = stubFor(
    WireMock
      .post(urlMatching(s"/gform/new-form/${formTemplate._id.value}/.*"))
      .willReturn(ok(JsonUtils.toJsonStr(FormIdData.Plain(UserId(""), formTemplate._id))))
  )

  def gformEmailStub() =
    stubFor(
      WireMock
        .post("/gform/email")
        .withRequestBody(
          matching(
            JsonUtils
              .toJsonStr(
                ConfirmationCodeWithEmailService(
                  NotifierEmailAddress("test@test.com"),
                  EmailConfirmationCode(ci"[A-Z]+"),
                  DigitalContact(EmailTemplateId("code_template"))
                )
              )
              .replaceAllLiterally("{", "\\{")
              .replaceAllLiterally("}", "\\}")
          )
        )
        .willReturn(noContent())
    )
}
