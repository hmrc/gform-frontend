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

package uk.gov.hmrc.gform.it.sample

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.models.Basic
import uk.gov.hmrc.gform.sharedmodel.email.{ EmailTemplateId, LocalisedEmailTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.InstructionPdfFields
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AcknowledgementSection, Anonymous, AuthCtx, AuthInfo, ContinueOrDeletePage, DeclarationSection, EmailAuthConfig, FormComponent, FormComponentId, FormKind, FormTemplate, FormTemplateId, FormTemplateVersion, KeyDisplayWidth, LayoutDisplayWidth, OnePerUser, Page, SummarySection, TextConstraint, UserResearchUrl }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.Section.NonRepeatingPage
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DataOutputFormat, DestinationId, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, EmailVerifierService, LangADT, LocalisedString }

trait FormTemplateSample {
  val formTemplateId = FormTemplateId("form-template-with-email-auth")
  val formTemplateEmailAuth = FormTemplate(
    _id = formTemplateId,
    originalId = formTemplateId,
    version = FormTemplateVersion(1),
    legacyFormIds = None,
    formName = LocalisedString(Map(LangADT.En -> "Form with email auth")),
    developmentPhase = None,
    webChat = None,
    draftRetrievalMethod = OnePerUser(ContinueOrDeletePage.Show),
    draftRetrieval = None,
    formCategory = uk.gov.hmrc.gform.sharedmodel.formtemplate.Default,
    languages = AvailableLanguages.default,
    parentFormSubmissionRefs = List.empty,
    summarySection = SummarySection(
      toSmartString("Check your answers"),
      Some(toSmartString("Caption")),
      toSmartString("Make sure the information you have given is correct"),
      toSmartString("Now send your form"),
      Some(toSmartString("Continue")),
      None,
      LayoutDisplayWidth.M,
      KeyDisplayWidth.S,
      None,
      None,
      None
    ),
    submitSection = None,
    authConfig =
      EmailAuthConfig(EmailVerifierService.digitalContact(EmailTemplateId("code_template"), None), None, None, None),
    displayHMRCLogo = true,
    formKind = FormKind
      .Classic(
        List(
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
                  isPageHeading = false,
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
              continueLabel = None,
              continueIf = None,
              instruction = None,
              presentationHint = None,
              dataRetrieve = None,
              confirmation = None,
              redirects = None,
              hideSaveAndComeBackButton = None,
              removeItemIf = None,
              displayWidth = None
            )
          )
        )
      ),
    destinations = DestinationList(
      destinations = NonEmptyList.one(
        HmrcDms(
          id = DestinationId("HMRCDMS"),
          dmsFormId = "HMRCDMS",
          customerId = AuthCtx(AuthInfo.GG),
          businessArea = "BusinessArea",
          classificationType = "ClassificationType",
          includeIf = HandlebarValue("true"),
          failOnError = true,
          dataOutputFormat = Some(DataOutputFormat.XML),
          formdataXml = false,
          backscan = None,
          instructionPdfFields = Some(InstructionPdfFields.Ordered),
          convertSingleQuotes = None,
          payload = None,
          payloadType = TemplateType.XML
        )
      ),
      acknowledgementSection = AcknowledgementSection(
        title = Some(toSmartString("Acknowledgement Page")),
        description = None,
        shortName = None,
        fields = List.empty,
        showReference = true,
        pdf = None,
        instructionPdf = None,
        displayFeedbackLink = true,
        noPIITitle = None,
        showBanner = true
      ),
      declarationSection = Some(
        DeclarationSection(
          title = toSmartString("Declaration Page"),
          noPIITitle = None,
          description = None,
          shortName = None,
          continueLabel = None,
          fields = List.empty,
          includeIf = None
        )
      )
    ),
    emailTemplateId = Some(LocalisedEmailTemplateId("some_email_template", None)),
    emailParameters = None,
    save4LaterInfoText = None,
    allowedFileTypes = FileInfoConfig.allAllowedFileTypes,
    fileSizeLimit = None,
    userResearchUrl = Some(UserResearchUrl("https://test.service.gov.uk")),
    referrerConfig = None,
    emailExpr = None,
    accessibilityUrl = None,
    exitPages = None,
    expressionsOutput = None,
    displayWidth = None,
    emailCodeParameters = None,
    dataRetrieve = None,
    accessiblePdf = false,
    displayAccountHeader = false,
    serviceStartPageUrl = None,
    downloadPreviousSubmissionPdf = true
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

  val formTemplateEmailAuthWithoutUserResearchUrl = formTemplateEmailAuth.copy(userResearchUrl = None)

}
