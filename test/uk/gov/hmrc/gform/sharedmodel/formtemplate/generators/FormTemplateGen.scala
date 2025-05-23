/*
 * Copyright 2023 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.formtemplate.generators

import cats.data.NonEmptyList
import org.scalacheck.Gen
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroup, AvailableLanguages, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

trait FormTemplateGen {
  def formTemplateIdGen: Gen[FormTemplateId] = PrimitiveGen.nonEmptyAlphaNumStrGen.map(FormTemplateId(_))
  def formTemplateIdLowerCase(formTemplateId: FormTemplateId): FormTemplateId = FormTemplateId(
    formTemplateId.value.toLowerCase
  )
  def legacyFromIdListGen: Gen[Option[NonEmptyList[FormTemplateId]]] =
    Gen.option(PrimitiveGen.oneOrMoreGen(PrimitiveGen.nonEmptyAlphaNumStrGen.map(FormTemplateId(_))))
  def formNameGen: Gen[LocalisedString] = LocalisedStringGen.localisedStringGen
  def developmentPhaseGen: Gen[DevelopmentPhase] = Gen.oneOf(AlphaBanner, BetaBanner, ResearchBanner, LiveBanner)
  def formCategoryGen: Gen[FormCategory] = Gen.oneOf(HMRCReturnForm, HMRCClaimForm, Default)

  def draftRetrievalMethodGen: Gen[DraftRetrievalMethod] =
    for {
      continueOrDeletePage <- ContinueOrDeletePageGen.continueOrDeletePageGen
      draftRetrievalMethod <-
        Gen
          .oneOf(OnePerUser(continueOrDeletePage), FormAccessCodeForAgents(continueOrDeletePage), BySubmissionReference)
    } yield draftRetrievalMethod

  def draftRetrievalGen: Gen[DraftRetrieval] =
    for {
      affinityGroup        <- Gen.oneOf(List(AffinityGroup.Individual, AffinityGroup.Agent, AffinityGroup.Organisation))
      continueOrDeletePage <- ContinueOrDeletePageGen.continueOrDeletePageGen
      draftRetrievalMethod <-
        Gen
          .oneOf(
            OnePerUser(continueOrDeletePage),
            FormAccessCode(continueOrDeletePage),
            BySubmissionReference,
            NotPermitted
          )
    } yield DraftRetrieval(mapping = Map(affinityGroup -> draftRetrievalMethod))

  def emailTemplateIdGen: Gen[Option[LocalisedEmailTemplateId]] =
    Gen.option(LocalisedEmailTemplateIdGen.localisedEmailTemplateIdGen)

  def emailParameterGen: Gen[EmailParameter] =
    for {
      emailTemplateVariable <- Gen.alphaNumStr
      value                 <- ExprGen.exprGen()

    } yield EmailParameter(emailTemplateVariable, value)

  def emailParameterListGen: Gen[Option[NonEmptyList[EmailParameter]]] =
    Gen.option(PrimitiveGen.oneOrMoreGen(emailParameterGen))

  def templateNameGen: Gen[TemplateName] =
    for {
      templateName <- Gen.alphaNumStr

    } yield TemplateName(templateName)

  def webChatGen: Gen[WebChat] =
    for {
      roomId       <- PrimitiveGen.nonEmptyAlphaNumStrGen
      templateName <- templateNameGen
    } yield WebChat(ChatRoomId(roomId), templateName)

  def userResearchUrlGen: Gen[UserResearchUrl] = PrimitiveGen.urlGen.map(UserResearchUrl(_))

  def serviceStartPageUrlGen: Gen[ServiceStartPageUrl] = PrimitiveGen.urlGen.map(ServiceStartPageUrl(_))

  def emailCodeTemplateIdGen: Gen[Option[LocalisedEmailTemplateId]] =
    Gen.option(LocalisedEmailTemplateIdGen.localisedEmailTemplateIdGen)

  def emailCodeParameterGen: Gen[EmailCodeParameter] =
    for {
      emailTemplateVariable <- Gen.alphaNumStr
      value                 <- LocalisedStringGen.localisedStringGen

    } yield EmailCodeParameter(emailTemplateVariable, value)

  def emailCodeParameterListGen: Gen[Option[NonEmptyList[EmailCodeParameter]]] =
    Gen.option(PrimitiveGen.oneOrMoreGen(emailCodeParameterGen))

  def formTemplateGen: Gen[FormTemplate] =
    for {
      id                       <- formTemplateIdGen
      version                  <- FormTemplateVersionGen.formTemplateVersionGen
      legacyFormIds            <- legacyFromIdListGen
      name                     <- formNameGen
      developmentPhase         <- Gen.option(developmentPhaseGen)
      category                 <- formCategoryGen
      draftRetrievalMethod     <- draftRetrievalMethodGen
      draftRetrieval           <- Gen.option(draftRetrievalGen)
      destinations             <- DestinationsGen.destinationsGen
      authConfig               <- AuthConfigGen.authConfigGen
      emailTemplateId          <- emailTemplateIdGen
      emailParameters          <- emailParameterListGen
      webChat                  <- Gen.option(webChatGen)
      sections                 <- PrimitiveGen.oneOrMoreGen(SectionGen.sectionGen)
      parentFormSubmissionRefs <- PrimitiveGen.zeroOrMoreGen(FormComponentGen.formComponentIdGen)
      save4LaterInfoText       <- Gen.option(Save4LaterInfoTextGen.save4LaterInfoTextGen)
      summarySection           <- SummarySectionGen.summarySectionGen
      submitSection            <- Gen.option(SubmitSectionGen.submitSectionGen)
      displayHMRCLogo          <- PrimitiveGen.booleanGen
      userResearchUrl          <- Gen.option(userResearchUrlGen)
      emailCodeParameters      <- emailCodeParameterListGen
      serviceStartPageUrl      <- Gen.option(serviceStartPageUrlGen)
    } yield FormTemplate(
      formTemplateIdLowerCase(id),
      id,
      version,
      legacyFormIds,
      name,
      developmentPhase,
      category,
      draftRetrievalMethod,
      draftRetrieval,
      destinations,
      authConfig,
      emailTemplateId,
      emailParameters,
      webChat,
      FormKind.Classic(sections.toList),
      parentFormSubmissionRefs,
      AvailableLanguages.default,
      save4LaterInfoText,
      summarySection,
      submitSection,
      displayHMRCLogo,
      FileInfoConfig.allAllowedFileTypes,
      None,
      userResearchUrl,
      None,
      None,
      None,
      None,
      None,
      None,
      emailCodeParameters,
      None,
      false,
      false,
      serviceStartPageUrl,
      true,
      None
    )
}

object FormTemplateGen extends FormTemplateGen
