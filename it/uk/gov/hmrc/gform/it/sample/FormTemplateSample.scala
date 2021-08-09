package uk.gov.hmrc.gform.it.sample

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.email.{ EmailTemplateId, LocalisedEmailTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.NonRepeatingPage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AcknowledgementSection, Anonymous, AuthCtx, ContinueOrDeletePage, DeclarationSection, EmailAuthConfig, FormComponent, FormComponentId, FormTemplate, FormTemplateId, GG, OnePerUser, Page, SummarySection, TextConstraint }
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, EmailVerifierService, LangADT, LocalisedString }

trait FormTemplateSample {
  val formTemplateEmailAuth = FormTemplate(
    _id = FormTemplateId("form-template-with-email-auth"),
    formName = LocalisedString(Map(LangADT.En -> "Form with email auth")),
    developmentPhase = None,
    webChat = None,
    draftRetrievalMethod = OnePerUser(ContinueOrDeletePage.Show),
    formCategory = uk.gov.hmrc.gform.sharedmodel.formtemplate.Default,
    languages = AvailableLanguages.default,
    parentFormSubmissionRefs = List.empty,
    summarySection = SummarySection(
      toSmartString("Check your answers"),
      toSmartString("Make sure the information you have given is correct"),
      toSmartString("Now send your form"),
      Some(toSmartString("Continue"))
    ),
    authConfig =
      EmailAuthConfig(EmailVerifierService.digitalContact(EmailTemplateId("code_template"), None), None, None, None),
    displayHMRCLogo = true,
    sections = List(
      NonRepeatingPage(
        Page[Basic](
          title = toSmartString("Page1"),
          id = None,
          noPIITitle = None,
          description = None,
          shortName = None,
          fields = List(
            FormComponent(
              id = FormComponentId("textField1"),
              `type` = uk.gov.hmrc.gform.sharedmodel.formtemplate.Text(
                constraint = TextConstraint.default,
                value = uk.gov.hmrc.gform.sharedmodel.formtemplate.Value
              ),
              label = toSmartString("Text field 1"),
              helpText = None,
              shortName = None,
              includeIf = None,
              validIf = None,
              mandatory = true,
              editable = true,
              submissible = true,
              derived = false,
              errorMessage = None
            )
          ),
          includeIf = None,
          validators = None,
          continueLabel = None,
          continueIf = None,
          instruction = None,
          presentationHint = None
        )
      )
    ),
    destinations = DestinationList(
      destinations = NonEmptyList.one(
        HmrcDms(
          id = DestinationId("HMRCDMS"),
          dmsFormId = "HMRCDMS",
          customerId = AuthCtx(GG),
          businessArea = "BusinessArea",
          classificationType = "ClassificationType",
          includeIf = "true",
          failOnError = true,
          roboticsXml = false,
          formdataXml = false,
          backscan = None,
          includeInstructionPdf = true
        )
      ),
      acknowledgementSection = AcknowledgementSection(
        title = toSmartString("Acknowledgement Page"),
        description = None,
        shortName = None,
        fields = List.empty,
        showReference = true,
        pdf = None,
        instructionPdf = None,
        displayFeedbackLink = true
      ),
      declarationSection = DeclarationSection(
        title = toSmartString("Declaration Page"),
        noPIITitle = None,
        description = None,
        shortName = None,
        continueLabel = None,
        fields = List.empty
      )
    ),
    emailTemplateId = LocalisedEmailTemplateId("some_email_template", None),
    emailParameters = None,
    save4LaterInfoText = None
  )

  val formTemplateEmailAuthWithOptionalDetails = formTemplateEmailAuth.copy(authConfig =
    EmailAuthConfig(
      EmailVerifierService.digitalContact(EmailTemplateId("code_template"), None),
      Some(LocalisedString(Map(LangADT.En -> "EmailUseInfo"))),
      Some(LocalisedString(Map(LangADT.En -> "EmailCodeHelp"))),
      Some(LocalisedString(Map(LangADT.En -> "EmailConfirmation")))
    )
  )

  val formTemplateAuthAnonymous = formTemplateEmailAuth
    .copy(
      _id = FormTemplateId("form-template-anonymous"),
      formName = LocalisedString(Map(LangADT.En -> "Form with anonymous auth")),
      authConfig = Anonymous
    )

}
