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

package uk.gov.hmrc.gform.sharedmodel

import java.time.{ LocalDateTime, LocalTime }
import cats.data.NonEmptyList
import play.api.ApplicationLoader.Context
import play.api.i18n.Lang
import play.api.{ Environment, Mode }
import uk.gov.hmrc.gform.auth.models.OtherRetrievals
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.{ Basic, DataRetrieveAll, PageMode }
import uk.gov.hmrc.gform.sharedmodel.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.{ ConfidenceLevel, Enrolments }
import uk.gov.hmrc.gform.Helpers.{ toLocalisedString, toSmartString }
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId }
import uk.gov.hmrc.gform.config.{ AuthModule, FrontendAppConfig, JSConfig }
import uk.gov.hmrc.gform.eval.{ EvaluationContext, FileIdsWithMapping }
import uk.gov.hmrc.gform.objectStore.{ Envelope, EnvelopeWithMapping }
import uk.gov.hmrc.gform.graph.FormTemplateBuilder.ls
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ DataOutputFormat, DestinationId, TemplateType }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.{ DmsMetaData, Submission, SubmissionId }
import uk.gov.hmrc.hmrcfrontend.config.{ AccessibilityStatementConfig, TrackingConsentConfig }
import uk.gov.hmrc.hmrcfrontend.views.html.helpers.HmrcTrackingConsentSnippet
import uk.gov.hmrc.http.HeaderCarrier
import org.typelevel.ci._
import play.api.test.Helpers
import uk.gov.hmrc.gform.sharedmodel.email.LocalisedEmailTemplateId

import scala.collection.immutable.List
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationIncludeIf.HandlebarValue

object ExampleData extends ExampleData

trait ExampleData
    extends ExampleFormTemplate with ExampleFieldId with ExampleFieldValue with ExampleFormField with ExampleSection
    with ExampleSectionNumber with ExampleForm with ExampleAuthConfig with ExampleFrontendAppConfig
    with ExampleAuthContext with ExampleInstruction with ExampleSubmissionRef with ExampleDmsMetaData
    with ExampleSubmission with ExampleEvaluationContext with ExampleDestination

trait ExampleEvaluationContext {
  self: ExampleFormTemplate with ExampleAuthContext with ExampleAuthConfig with ExampleSubmissionRef =>

  val evaluationContext: EvaluationContext =
    EvaluationContext(
      formTemplateId,
      submissionRef,
      None,
      authContext,
      ThirdPartyData.empty,
      authConfig,
      HeaderCarrier(),
      Option.empty[FormPhase],
      FileIdsWithMapping.empty,
      Map.empty,
      Set.empty,
      Set.empty,
      Set.empty,
      Map.empty,
      LangADT.En,
      Helpers.stubMessages(Helpers.stubMessagesApi(Map.empty)),
      List.empty,
      Set.empty,
      FileSizeLimit(1),
      LocalisedLookupOptions(Map()),
      DataRetrieveAll.empty,
      Set.empty[ModelComponentId],
      Map.empty,
      Set.empty
    )
}

trait ExampleSubmission {
  self: ExampleForm with ExampleSubmissionRef with ExampleDmsMetaData =>
  def submission(implicit localDateTime: LocalDateTime) =
    Submission(SubmissionId(formIdData.toFormId, envelopeId), localDateTime, submissionRef, envelopeId, dmsMetaData)
}

trait ExampleDmsMetaData {
  self: ExampleFormTemplate =>
  val dmsMetaData = DmsMetaData(formTemplateId)
}

trait ExampleSubmissionRef {
  val submissionRef = SubmissionRef("submission-ref")
}

trait ExampleInstruction {
  val instruction = Instruction(Some(toSmartString("some-instruction")), Some(1))

  def buildInstruction(name: String, order: Option[Int] = None) =
    instruction.copy(name = Some(toSmartString(name)), order = order)
}

trait ExampleDestination { self: ExampleAuthConfig =>

  val formComponent = List(buildFormComponent("fieldInAcknowledgementSections", Value))

  val hmrcDms = HmrcDms(
    DestinationId("TestHmrcDmsId"),
    "TestHmrcDmsFormId",
    Constant("TestHmrcDmsCustomerId"),
    "TestHmrcDmsClassificationType",
    "TestHmrcDmsBusinessArea",
    HandlebarValue(""),
    true,
    Some(DataOutputFormat.XML),
    true,
    Some(true),
    false,
    None,
    None,
    TemplateType.XML
  )

  val ackSection =
    AcknowledgementSection(
      Some(toSmartString("ack section with email param field")),
      None,
      None,
      formComponent,
      true,
      Some(
        PdfCxt(
          Some(toSmartString("It's a Acknowledgement Section Pdf header.")),
          Some(toSmartString("It's a Acknowledgement Section Pdf footer.")),
          None,
          None
        )
      ),
      None,
      true,
      None,
      true
    )

  val decFormComponent = List(buildFormComponent("fieldInDeclarationSections", Value))

  val decSection =
    DeclarationSection(
      toSmartString("declaration section"),
      None,
      None,
      None,
      Some(toSmartString("ContinueLabel")),
      decFormComponent,
      None
    )

  def mkDecSection(noPIITitle: Option[String] = None) =
    DeclarationSection(
      toSmartString("declaration section"),
      noPIITitle.map(toSmartString),
      None,
      None,
      Some(toSmartString("ContinueLabel")),
      decFormComponent,
      None
    )

  def destinationList = DestinationList(NonEmptyList.of(hmrcDms), ackSection, Some(decSection))
}

trait ExampleAuthConfig {

  def buildFormComponent(name: String, expr: Expr, instruction: Option[Instruction] = None): FormComponent =
    buildFormComponent(name, Text(TextConstraint.default, expr), instruction)

  def buildFormComponentWithTextConstraint(
    name: String,
    expr: Expr,
    textConstraint: TextConstraint,
    instruction: Option[Instruction] = None
  ): FormComponent =
    buildFormComponent(name, Text(textConstraint, expr), instruction)

  def buildFormComponent(name: String, componentType: ComponentType, instruction: Option[Instruction]): FormComponent =
    FormComponent(
      FormComponentId(name),
      componentType,
      toSmartString(name),
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None,
      Nil,
      instruction
    )

  def regimeId = RegimeId("TestRegimeId")

  def serviceId = ServiceId("TestServiceId")

  def enrolmentSection = EnrolmentSection(
    toSmartString("Some enrolment section title"),
    Some(toSmartString("Some noPII enrolment section title")),
    None,
    List.empty,
    NonEmptyList.one(IdentifierRecipe("key", FormCtx(FormComponentId("field")))),
    List.empty,
    None
  )

  val enrolmentOutcome = EnrolmentOutcome(toSmartString("title"), toSmartString("content"))
  val enrolmentOutcomes =
    EnrolmentOutcomes(enrolmentOutcome, enrolmentOutcome, enrolmentOutcome, enrolmentOutcome, enrolmentOutcome)

  def authConfig =
    HmrcAgentWithEnrolmentModule(
      AllowAnyAgentAffinityUser,
      EnrolmentAuth(
        serviceId,
        DoCheck(Always, RequireEnrolment(enrolmentSection, NoAction), NoCheck),
        enrolmentOutcomes
      )
    )
}

trait ExampleFieldId {

  def `fieldId - facePhoto` = FormComponentId("facePhoto")
  def `fieldId - address` = FormComponentId("address")
  def `fieldId - surname` = FormComponentId("surname")
  def `fieldId - firstName` = FormComponentId("firstName")
  def `fieldId - timeOfCall` = FormComponentId("timeOfCall")
  def `fieldId - iptRegNum` = FormComponentId("iptRegNum")
  def `fieldId - businessName` = FormComponentId("nameOfBusiness")
  def `fieldId - startDate` = FormComponentId("startDate")

  def `fieldId - startDate-year` = FormComponentId("startDate-year")
  def `fieldId - startDate-day` = FormComponentId("startDate-day")
  def `fieldId - startDate-month` = FormComponentId("startDate-month")
  def `fieldId - number` = FormComponentId("number")
  def `fieldId - choice` = FormComponentId("choice")
  def `fieldId - revealingChoice` = FormComponentId("revealingChoice")
  def default = FormComponentId("test")

  //fieldId when submitting form
  def `fieldId - save` = FormComponentId("save")

}

trait ExampleFieldValue { dependecies: ExampleFieldId =>

  def validIf: Option[ValidIf] = None

  def `fieldValue - address` = FormComponent(
    `fieldId - address`,
    Address(false, List.empty[Address.Configurable.Mandatory], false, None),
    toSmartString("Address"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = false,
    false,
    None,
    instruction = instruction("Address - instruction")
  )

  def `fieldValue - facePhoto` = FormComponent(
    `fieldId - facePhoto`,
    FileUpload(None, None),
    toSmartString("Attach evidence of your smile"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    false,
    None,
    None
  )

  def `fieldValue - firstName` = FormComponent(
    `fieldId - firstName`,
    Text(TextConstraint.default, Constant("any text")),
    toSmartString("First Name"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    false,
    None,
    None
  )

  def `fieldValue - surname` = FormComponent(
    `fieldId - surname`,
    Text(TextConstraint.default, Constant("any text")),
    toSmartString("Last Name"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    false,
    None,
    None
  )

  def `fieldValue - timeOfCall` = FormComponent(
    `fieldId - timeOfCall`,
    Time(List(Range(StartTime(LocalTime.parse("00:00")), EndTime(LocalTime.parse("23:59")))), IntervalMins(15)),
    toSmartString("Time of call"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    false,
    None,
    None
  )

  def `fieldValue - iptRegNum` = FormComponent(
    `fieldId - iptRegNum`,
    Text(TextConstraint.default, Constant("any text")),
    toSmartString("Insurance Premium Tax (IPT) number"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    false,
    None,
    None
  )

  def `fieldValue - businessName` = FormComponent(
    `fieldId - businessName`,
    Text(TextConstraint.default, Constant("any text")),
    toSmartString("Name of business"),
    false,
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    false,
    None,
    None
  )

  def `fieldValue - startDate` = FormComponent(
    `fieldId - startDate`,
    Date(AnyDate, Offset(0), None),
    toSmartString("Your Start Date"),
    false,
    None,
    None,
    None,
    None,
    true,
    true,
    true,
    true,
    false,
    None,
    None
  )

  def `fieldValue - info` = FormComponent(
    `fieldId - businessName`,
    InformationMessage(NoFormat, toSmartString("some text")),
    toSmartString("someLabel"),
    false,
    None,
    None,
    None,
    validIf,
    false,
    false,
    false,
    true,
    false,
    None
  )

  def `group - type` = Group(
    fields = List(`fieldValue - firstName`),
    repeatsMax = None,
    repeatsMin = None,
    repeatLabel = None,
    repeatAddAnotherText = None
  )

  def `fieldValue - group` = FormComponent(
    id = FormComponentId("GroupFieldValueId"),
    `type` = `group - type`,
    label = toSmartString("group FieldValue label"),
    isPageHeading = false,
    helpText = None,
    shortName = None,
    includeIf = None,
    validIf = None,
    mandatory = true,
    editable = false,
    submissible = true,
    derived = true,
    errorMessage = None
  )

  def `fieldValue - number` = FormComponent(
    `fieldId - number`,
    Text(Number(), Value),
    toSmartString("sample label"),
    false,
    None,
    None,
    None,
    validIf,
    true,
    false,
    false,
    false,
    false,
    None
  )

  private def toOptionData(xs: NonEmptyList[String]): NonEmptyList[OptionData.IndexBased] =
    xs.map(l => OptionData.IndexBased(toSmartString(l), None, None, None))

  private def toOptionData(s: String): OptionData.IndexBased = OptionData.IndexBased(toSmartString(s), None, None, None)

  def `fieldValue - choice` = FormComponent(
    `fieldId - choice`,
    Choice(
      Radio,
      toOptionData(NonEmptyList.of("u", "v")),
      Vertical,
      List(),
      None,
      None,
      None,
      LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
      None,
      None,
      false
    ),
    toSmartString("sample label"),
    false,
    None,
    None,
    None,
    validIf,
    true,
    true,
    true,
    true,
    true,
    None
  )

  def `fieldValue - text1` =
    fieldValue(
      Text(TextConstraint.default, Constant("value1")),
      fcId = FormComponentId("text1"),
      label = "text1",
      instruction = instruction("text1 - instruction")
    )

  def `fieldValue - text2` =
    fieldValue(
      Text(TextConstraint.default, Constant("value2")),
      fcId = FormComponentId("text2"),
      label = "text2",
      instruction = instruction("text2 - instruction")
    )

  def `fieldValue - revealingChoice` =
    FormComponent(
      `fieldId - revealingChoice`,
      RevealingChoice(
        List(
          RevealingChoiceElement(
            toOptionData("choice1"),
            List(`fieldValue - text1`, `fieldValue - text2`),
            None,
            true
          ),
          RevealingChoiceElement(toOptionData("choice2"), List(`fieldValue - address`), None, true)
        ),
        true
      ),
      toSmartString("Revealing Choice"),
      false,
      None,
      None,
      None,
      validIf,
      true,
      true,
      true,
      true,
      true,
      None,
      instruction = instruction("Revealing Choice - instruction")
    )

  def fieldValue(
    text: Text,
    fcId: FormComponentId = default,
    label: String = "sample label",
    instruction: Option[Instruction] = None
  ) = FormComponent(
    fcId,
    text,
    toSmartString(label),
    false,
    None,
    None,
    None,
    None,
    true,
    false,
    false,
    true,
    false,
    None,
    instruction = instruction
  )

  def addToListQuestion(addAnotherQuestionName: String): FormComponent =
    FormComponent(
      FormComponentId(addAnotherQuestionName),
      Choice(
        YesNo,
        toOptionData(NonEmptyList.of("yes", "no")),
        Vertical,
        List.empty,
        None,
        None,
        None,
        LocalisedString(Map(LangADT.En -> "or", LangADT.Cy -> "neu")),
        None,
        None,
        false
      ),
      ls,
      false,
      None,
      None,
      None,
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )

  def instruction(name: String, order: Int = 1): Option[Instruction] =
    Some(Instruction(name = Some(toSmartString(name)), order = Some(order)))
}

trait ExampleSection { dependecies: ExampleFieldId with ExampleFieldValue =>
  def nonRepeatingPageSection(
    title: String = "About you",
    noPIITitle: Option[String] = None,
    fields: List[FormComponent] = List(`fieldValue - firstName`, `fieldValue - surname`, `fieldValue - facePhoto`),
    includeIf: Option[IncludeIf] = None,
    instruction: Option[Instruction] = None,
    presentationHint: Option[PresentationHint] = None
  ) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(title),
        None,
        noPIITitle.map(toSmartString),
        None,
        None,
        None,
        includeIf,
        fields,
        None,
        None,
        instruction,
        presentationHint,
        None,
        None,
        None,
        None,
        None,
        None
      )
    )

  def `section - about you`: Section =
    nonRepeatingPageSection(
      fields =
        List(`fieldValue - firstName`, `fieldValue - surname`, `fieldValue - facePhoto`, `fieldValue - timeOfCall`)
    )

  def `section - businessDetails` =
    nonRepeatingPageSection(
      title = "Business details",
      fields = List(`fieldValue - businessName`, `fieldValue - startDate`, `fieldValue - iptRegNum`)
    )

  def `repeating section` =
    repeatingSection("Repeating section", List(`fieldValue - surname`), None, FormCtx(`fieldId - firstName`))

  def `section - group` =
    nonRepeatingPageSection(
      fields = List(`fieldValue - group`)
    )

  def allSections = List(`section - about you`, `section - businessDetails`)

  def repeatingSection(
    title: String,
    fields: List[FormComponent],
    instruction: Option[Instruction] = None,
    repeatsExpr: Expr,
    presentationHint: Option[PresentationHint] = None
  ) =
    Section.RepeatingPage(
      Page(
        toSmartString(title),
        None,
        None,
        None,
        None,
        None,
        None,
        fields,
        None,
        None,
        instruction,
        presentationHint,
        None,
        None,
        None,
        None,
        None,
        None
      ),
      repeats = repeatsExpr
    )

  def addToListSection(
    title: String,
    description: String,
    summaryDescription: String,
    shortName: String,
    summaryName: String,
    addAnotherQuestion: FormComponent,
    instruction: Option[Instruction],
    pages: List[Page[Basic]],
    presentationHint: Option[PresentationHint] = None,
    infoMessage: Option[String] = None,
    errorMessage: Option[String] = None,
    defaultPage: Option[Page[Basic]] = None,
    cyaPage: Option[CheckYourAnswersPage] = None
  ): Section.AddToList =
    Section.AddToList(
      toSmartString(title),
      None,
      Some(toSmartString(title)),
      toSmartString(description),
      toSmartString(summaryDescription),
      toSmartString(shortName),
      toSmartString(summaryName),
      None,
      NonEmptyList.fromListUnsafe(pages),
      None,
      None,
      None,
      addAnotherQuestion,
      instruction,
      presentationHint,
      infoMessage.map(toSmartString),
      errorMessage.map(toSmartString),
      defaultPage,
      cyaPage
    )

  def toPage(
    title: String,
    instruction: Option[Instruction],
    formComponents: List[FormComponent],
    presentationHint: Option[PresentationHint] = None
  ): Page[Basic] =
    mkPage[Basic](title, instruction, formComponents, presentationHint)

  def mkPage[T <: PageMode](
    title: String,
    instruction: Option[Instruction],
    formComponents: List[FormComponent],
    presentationHint: Option[PresentationHint] = None
  ): Page[T] = Page[T](
    toSmartString(title),
    None,
    None,
    None,
    None,
    None,
    None,
    formComponents,
    None,
    None,
    instruction,
    presentationHint,
    None,
    None,
    None,
    None,
    None,
    None
  )
}

trait ExampleSectionNumber {
  val `sectionNumber-1` = SectionNumber.Classic(-1)
  val sectionNumber0 = SectionNumber.Classic(0)
  val sectionNumber1 = SectionNumber.Classic(1)
  val sectionNumber2 = SectionNumber.Classic(2)
  val sectionNumber3 = SectionNumber.Classic(3)
}

trait ExampleFormTemplate {
  dependsOn: ExampleAuthConfig with ExampleSection with ExampleFieldId with ExampleFieldValue with ExampleDestination =>

  def formTemplateOriginalId = FormTemplateId("AAA999")
  def legacyFormIds =
    Some(
      NonEmptyList.of(
        FormTemplateId("AAA100")
      )
    )
  def formTemplateId = FormTemplateId(formTemplateOriginalId.value.toLowerCase)
  def formName = toLocalisedString("AAA999 dev test template")

  def emailTemplateId = Some(LocalisedEmailTemplateId("test-email-template-id", None))

  def emailParameters =
    Some(
      NonEmptyList
        .of(
          EmailParameter("fullNameVariable", FormCtx(FormComponentId("fullName"))),
          EmailParameter("emailVariable", FormCtx(FormComponentId("email")))
        )
    )

  def webChat = None

  def acknowledgementSection =
    AcknowledgementSection(
      Some(toSmartString("Acknowledgement Page")),
      Some(toSmartString("this page is to acknowledge submission")),
      Some(toSmartString("shortName for acknowledgement")),
      List(`fieldValue - info`),
      true,
      None,
      None,
      true,
      None,
      true
    )

  def declarationSection =
    DeclarationSection(
      toSmartString("Declaration"),
      None,
      None,
      None,
      Some(toSmartString("ContinueLabel")),
      Nil,
      None
    )

  def summarySection =
    SummarySection(
      toSmartString("Summary Title"),
      Some(toSmartString("Summary Caption")),
      toSmartString("Summary Header"),
      toSmartString("Summary Footer"),
      Some(toSmartString("Summary ContinueLabel")),
      None,
      LayoutDisplayWidth.M,
      KeyDisplayWidth.S,
      None,
      None,
      None
    )

  def userResearchUrl = UserResearchUrl("https://test.service.gov.uk")

  def buildFormTemplateContext: FormTemplateContext =
    FormTemplateContext.basicContext(buildFormTemplate, None)
  def buildFormTemplate: FormTemplate = buildFormTemplate(destinationList, allSections)

  def buildFormTemplate(destinationList: DestinationList, sections: List[Section]): FormTemplate =
    FormTemplate(
      formTemplateId,
      formTemplateOriginalId,
      FormTemplateVersion(1),
      legacyFormIds,
      formName,
      Some(ResearchBanner),
      Default,
      OnePerUser(ContinueOrDeletePage.Show),
      destinationList,
      authConfig,
      emailTemplateId,
      emailParameters,
      webChat,
      FormKind.Classic(sections),
      Nil,
      AvailableLanguages.default,
      None,
      summarySection,
      None,
      true,
      FileInfoConfig.allAllowedFileTypes,
      None,
      Some(userResearchUrl),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      false,
      false,
      None
    )
}

trait ExampleFormField { dependsOn: ExampleFormTemplate with ExampleFieldId =>

  private def mkFormField(formComponentId: FormComponentId, value: String): FormField = FormField(
    formComponentId.modelComponentId,
    value
  )

  def `formField - facePhoto` = mkFormField(`fieldId - facePhoto`, "face-photo.jpg")
  def `formField - firstName` = mkFormField(`fieldId - firstName`, "James")
  def `formField - surname` = mkFormField(`fieldId - surname`, "Bond")
  def `formField - iptRegNum` = mkFormField(`fieldId - iptRegNum`, "666CasinoRoyale")
  def `formField - businessName` = mkFormField(`fieldId - businessName`, "Quantum of Solace")
  def `formField - startDateDay` = mkFormField(`fieldId - startDate-day`, "11")
  def `formField - startDateMonth` = mkFormField(`fieldId - startDate-month`, "10")
  def `formField - startDateYear` = mkFormField(`fieldId - startDate-year`, "2008")
  def `formField - number` = mkFormField(`fieldId - number`, "1,234")
  def `formField - choice` = mkFormField(`fieldId - choice`, "u")

  //actions:

  def `formField - Save` = mkFormField(`fieldId - save`, "Save")
  def `formField - Continue` = mkFormField(`fieldId - save`, "Continue")
  def `formField - Back` = mkFormField(`fieldId - save`, "Back")
  def `formField - AddGroup` = mkFormField(`fieldId - save`, "AddGroup")
  def `formField - RemoveGroup` = mkFormField(`fieldId - save`, "RemoveGroup")

  def data: Map[FormComponentId, FormField] = Map(
    `fieldId - save`            -> `formField - Save`,
    `fieldId - facePhoto`       -> `formField - facePhoto`,
    `fieldId - firstName`       -> `formField - firstName`,
    `fieldId - surname`         -> `formField - surname`,
    `fieldId - iptRegNum`       -> `formField - iptRegNum`,
    `fieldId - startDate-year`  -> `formField - startDateYear`,
    `fieldId - startDate-month` -> `formField - startDateMonth`,
    `fieldId - startDate-day`   -> `formField - startDateDay`,
    `fieldId - businessName`    -> `formField - businessName`,
    `fieldId - number`          -> `formField - number`,
    `fieldId - choice`          -> `formField - choice`
  )

//  def rawDataFromBrowser: Map[FormComponentId, Seq[String]] = data.mapValues(x => Seq(x.value))
  //def rawDataFromBrowser: VariadicFormData = VariadicFormData(data.mapValues(x => VariadicValue.One(x.value)))
  /* def formDataRecalculated: FormDataRecalculated =
   *   FormDataRecalculated.empty.copy(recData = RecData.fromData(rawDataFromBrowser)) */
}

trait ExampleForm { dependsOn: ExampleFormField with ExampleFormTemplate =>

  def userId = UserId("James007")

  def materialisedRetrievals =
    AuthenticatedRetrievals(
      GovernmentGatewayId(""),
      Enrolments(Set()),
      AffinityGroup.Individual,
      userId.value,
      None,
      OtherRetrievals.empty,
      ConfidenceLevel.L50,
      None
    )

  def formIdData = FormIdData(materialisedRetrievals, formTemplateId, None)

  def accessCode = AccessCode("1234-0000-ABCD")

  def formFields: List[FormField] = data.values.toList

  def formData = FormData(formFields)

  def formDataNil = FormData(fields = Nil)

  def envelopeId = EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307")

  def envelope = Envelope.empty

  def envelopeWithMapping = EnvelopeWithMapping.empty

  val envelopeExpiryDate = Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1).withNano(0)))

  def buildForm: Form = buildForm(formData)

  def buildForm(formData: FormData): Form = Form(
    formIdData.toFormId,
    envelopeId,
    userId,
    formTemplateId,
    None,
    formData,
    InProgress,
    VisitIndex.Classic(Set.empty),
    ThirdPartyData.empty,
    envelopeExpiryDate,
    FormComponentIdToFileIdMapping.empty
  )
}

trait ExampleAuthContext {

  def authContext =
    AuthenticatedRetrievals(
      governmentGatewayId = GovernmentGatewayId(""),
      enrolments = enrolments,
      affinityGroup = affinityGroup,
      groupIdentifier = "TestGroupId",
      maybeNino = None,
      otherRetrievals = OtherRetrievals.empty,
      ConfidenceLevel.L50,
      None
    )

  def affinityGroup: AffinityGroup = Organisation

  def internalId =
    None

  def externalId =
    None

  def credentialStrength =
    None

  def agentCode =
    None

  def enrolments =
    Enrolments(Set())

}

trait ExampleFrontendAppConfig {

  private val env: Environment = Environment.simple(mode = Mode.Test)
  private val context = Context.create(env)

  val frontendAppConfig = FrontendAppConfig(
    albAdminIssuerUrl = "",
    governmentGatewaySignInUrl = "http://gofernment.gateway.signin.url",
    gformFrontendBaseUrl = "gform.frontend.base.url",
    betaFeedbackUrlNoAuth = "beta.feedback.url.no.auth",
    signOutUrl = "http://localhost:9025/loggedout",
    footerAccessibilityStatementUrl = "",
    authModule = AuthModule(
      JSConfig(false, 0, 0, "", ""),
      JSConfig(false, 0, 0, "", ""),
      JSConfig(false, 0, 0, "", ""),
      JSConfig(false, 0, 0, "", "")
    ),
    availableLanguages = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy")),
    routeToSwitchLanguageDataChange =
      uk.gov.hmrc.gform.gform.routes.LanguageSwitchController.switchToLanguageDataChange,
    routeToSwitchLanguageNoDataChange =
      uk.gov.hmrc.gform.gform.routes.LanguageSwitchController.switchToLanguageNoDataChange,
    optimizelyUrl = None,
    trackingConsentSnippet = new HmrcTrackingConsentSnippet(new TrackingConsentConfig(context.initialConfiguration)),
    emailAuthStaticCodeEmails = Some(NonEmptyList.of(ci"test1@test.com", ci"test2@test.com")),
    accessibilityStatementConfig = new AccessibilityStatementConfig(context.initialConfiguration),
    refreshSessionUrl = "refreshSessionUrl",
    isProd = true,
    configuration = context.initialConfiguration
  )
}
