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

package uk.gov.hmrc.gform.sharedmodel

import java.time.{ LocalDateTime, LocalTime }

import cats.data.NonEmptyList
import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolments }
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, GovernmentGatewayId }
import uk.gov.hmrc.gform.config.{ AuthModule, FrontendAppConfig, JSConfig }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.FormModel
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.Helpers.{ toLocalisedString, toSmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destination.HmrcDms
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList

import scala.collection.immutable
import scala.collection.immutable.List

object ExampleData extends ExampleData

trait ExampleData
    extends ExampleFormTemplate with ExampleFieldId with ExampleFieldValue with ExampleFormField with ExampleValidator
    with ExampleSection with ExampleSectionNumber with ExampleForm with ExampleAuthConfig with ExampleFrontendAppConfig
    with ExampleAuthContext

trait ExampleAuthConfig {

  val hmrcDms = HmrcDms(
    DestinationId("TestHmrcDmsId"),
    "TestHmrcDmsFormId",
    Constant("TestHmrcDmsCustomerId"),
    "TestHmrcDmsClassificationType",
    "TestHmrcDmsBusinessArea",
    "",
    true,
    true,
    Some(true)
  )

  val formComponent = List(buildFormComponent("fieldInAcknowledgementSections", Value))

  val ackSection =
    AcknowledgementSection(
      toSmartString("ack section with email param field"),
      None,
      None,
      formComponent,
      true,
      Some(
        AcknowledgementSectionPdf(
          Some(toSmartString("It's a Acknowledgement Section Pdf header.")),
          Some(toSmartString("It's a Acknowledgement Section Pdf footer."))))
    )

  val decFormComponent = List(buildFormComponent("fieldInDeclarationSections", Value))

  val decSection =
    DeclarationSection(toSmartString("declaration section"), None, None, decFormComponent)

  private def buildFormComponent(name: String, expr: Expr) =
    FormComponent(
      FormComponentId(name),
      Text(BasicText, expr),
      toSmartString(name),
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

  def destinationList = DestinationList(NonEmptyList.of(hmrcDms), ackSection, decSection)

  def regimeId = RegimeId("TestRegimeId")

  def serviceId = ServiceId("TestServiceId")

  def authConfig =
    HmrcAgentWithEnrolmentModule(
      AllowAnyAgentAffinityUser,
      EnrolmentAuth(serviceId, DoCheck(Always, RejectAccess, NoCheck)))
}

trait ExampleFieldId {

  def `fieldId - facePhoto` = FormComponentId("facePhoto")
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
  def default = FormComponentId("test")

  //fieldId when submitting form
  def `fieldId - save` = FormComponentId("save")

}

trait ExampleFieldValue { dependecies: ExampleFieldId =>

  def validIf: Option[ValidIf] = None

  def `fieldValue - facePhoto` = FormComponent(
    `fieldId - facePhoto`,
    FileUpload(),
    toSmartString("Attach evidence of your smile"),
    helpText = None,
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
    Text(BasicText, Constant("any text")),
    toSmartString("First Name"),
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
    Text(BasicText, Constant("any text")),
    toSmartString("Last Name"),
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
    Text(BasicText, Constant("any text")),
    toSmartString("Insurance Premium Tax (IPT) number"),
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
    Text(BasicText, Constant("any text")),
    toSmartString("Name of business"),
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
    helpText = None,
    shortName = None,
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

  def `fieldValue - choice` = FormComponent(
    `fieldId - choice`,
    Choice(Radio, NonEmptyList.of(toSmartString("u"), toSmartString("v")), Vertical, List(), None),
    toSmartString("sample label"),
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

  def fieldValue(text: Text) = FormComponent(
    default,
    text,
    toSmartString("sample label"),
    None,
    None,
    None,
    true,
    false,
    false,
    true,
    false,
    None
  )

}

trait ExampleSection { dependecies: ExampleFieldId with ExampleFieldValue =>
  def nonRepeatingPageSection(
    title: String = "About you",
    validators: Option[Validator] = None,
    fields: List[FormComponent] = List(`fieldValue - firstName`, `fieldValue - surname`, `fieldValue - facePhoto`),
    includeIf: Option[IncludeIf] = None) =
    Section.NonRepeatingPage(
      Page(
        toSmartString(title),
        None,
        None,
        None,
        includeIf,
        validators,
        fields,
        None,
        None
      ))

  def `section - about you`: Section =
    nonRepeatingPageSection(
      fields =
        List(`fieldValue - firstName`, `fieldValue - surname`, `fieldValue - facePhoto`, `fieldValue - timeOfCall`),
      validators = None)

  def `section - businessDetails` =
    nonRepeatingPageSection(
      title = "Business details",
      validators = None,
      fields = List(`fieldValue - businessName`, `fieldValue - startDate`, `fieldValue - iptRegNum`))

  def `repeating section` =
    Section.RepeatingPage(
      Page(
        toSmartString("Repeating section"),
        None,
        None,
        None,
        None,
        None,
        List(`fieldValue - surname`),
        None,
        None
      ),
      repeats = FormCtx(`fieldId - firstName`)
    )

  def `section - group` =
    nonRepeatingPageSection(
      fields = List(`fieldValue - group`)
    )

  def allSections = List(`section - about you`, `section - businessDetails`)

}

trait ExampleSectionNumber {
  val `sectionNumber-1` = SectionNumber(-1)
  val sectionNumber0 = SectionNumber(0)
  val sectionNumber1 = SectionNumber(1)
  val sectionNumber2 = SectionNumber(2)
  val sectionNumber3 = SectionNumber(3)
}

trait ExampleValidator {
  def defaultValidator = hMRCUTRPostcodeCheckValidator
  def hMRCUTRPostcodeCheckValidator =
    HmrcRosmRegistrationCheckValidator(
      toSmartString("The UTR could not be foundor the postcode did not match. | <Welsh...>"),
      "ITSA",
      FormCtx(FormComponentId("utrToCheck")),
      FormCtx(FormComponentId("postcodeToCheck"))
    )
  def bankAccountModulusCheckValidator =
    BankAccountModulusCheck(
      toSmartString("This is an error message for Bank"),
      FormCtx(FormComponentId("accountNumber")),
      FormCtx(FormComponentId("sortCode")))
  //todo other example validators
}

trait ExampleFormTemplate {
  dependsOn: ExampleAuthConfig with ExampleSection with ExampleFieldId with ExampleFieldValue =>

  def formTemplateId = FormTemplateId("AAA999")

  def formName = toLocalisedString("AAA999 dev test template")

  def emailTemplateId = "test-email-template-id"

  def emailParameters =
    Some(
      NonEmptyList
        .of(
          EmailParameter("fullNameVariable", FormCtx(FormComponentId("fullName"))),
          EmailParameter("emailVariable", FormCtx(FormComponentId("email")))))

  def webChat = None

  def acknowledgementSection =
    AcknowledgementSection(
      toSmartString("Acknowledgement Page"),
      Some(toSmartString("this page is to acknowledge submission")),
      Some(toSmartString("shortName for acknowledgement")),
      List(`fieldValue - info`),
      true,
      None
    )

  def declarationSection =
    DeclarationSection(
      toSmartString("Declaration"),
      None,
      None,
      Nil
    )

  def summarySection =
    SummarySection(toSmartString("Summary Title"), toSmartString("Summary Header"), toSmartString("Summary Footer"))

  def formTemplate = FormTemplate(
    formTemplateId,
    formName,
    Some(ResearchBanner),
    Default,
    OnePerUser(ContinueOrDeletePage.Show),
    destinationList,
    authConfig,
    emailTemplateId,
    emailParameters,
    webChat,
    allSections,
    Nil,
    AvailableLanguages.default,
    None,
    summarySection
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
    AuthenticatedRetrievals(GovernmentGatewayId(""), Enrolments(Set()), AffinityGroup.Individual, userId.value, None)

  def formId = FormId(materialisedRetrievals, formTemplateId, None)

  def accessCode = AccessCode("1234-0000-ABCD")

  def formFields: Seq[FormField] = data.values.toSeq

  def formData = FormData(formFields)

  def formDataNil = FormData(fields = Nil)

  def envelopeId = EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307")

  def envelope = Envelope.empty

  val envelopeExpiryDate = Some(EnvelopeExpiryDate(LocalDateTime.now.plusDays(1).withNano(0)))

  def form = Form(
    formId,
    envelopeId,
    userId,
    formTemplateId,
    formData,
    InProgress,
    VisitIndex(Set.empty),
    ThirdPartyData.empty,
    envelopeExpiryDate
//    EvaluationResults.empty
  )

}

trait ExampleAuthContext {

  def authContext =
    AuthenticatedRetrievals(
      governmentGatewayId = GovernmentGatewayId(""),
      enrolments = enrolments,
      affinityGroup = affinityGroup,
      groupIdentifier = "TestGroupId",
      maybeNino = None
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
import play.api.i18n.Lang

trait ExampleFrontendAppConfig {
  val frontendAppConfig = FrontendAppConfig(
    albAdminIssuerUrl = "",
    analyticsToken = "analyticsToken",
    analyticsHost = "analyticsHost",
    reportAProblemPartialUrl = "http://reportProblem.url",
    reportAProblemNonJSUrl = "http://reportProblem.json.url",
    governmentGatewaySignInUrl = "http://gofernment.gateway.signin.url",
    gformFrontendBaseUrl = "gform.frontend.base.url",
    betaFeedbackUrlNoAuth = "beta.feedback.url.no.auth",
    signOutUrl = "http://localhost:9025/loggedout",
    footerCookiesUrl = "",
    footerPrivacyPolicyUrl = "",
    footerTermsConditionsUrl = "",
    footerHelpUrl = "",
    footerAccessibilityStatementUrl = "",
    whitelistEnabled = true,
    googleTagManagerIdAvailable = false,
    googleTagManagerId = "",
    authModule = AuthModule(JSConfig(false, 0, 0, "", ""), JSConfig(false, 0, 0, "", ""), JSConfig(false, 0, 0, "", "")),
    availableLanguages = Map("english" -> Lang("en"), "cymraeg" -> Lang("cy")),
    routeToSwitchLanguage = uk.gov.hmrc.gform.gform.routes.LanguageSwitchController.switchToLanguage,
    contactFormServiceIdentifier = "",
    optimizelyUrl = None
  )
}
