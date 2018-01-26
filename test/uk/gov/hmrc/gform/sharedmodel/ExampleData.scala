/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.auth.core.AffinityGroup.Organisation
import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolments }
import uk.gov.hmrc.auth.core.retrieve.OneTimeLogin
import uk.gov.hmrc.gform.auth.models.{ Retrievals, UserDetails }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AnyText, _ }

import scala.collection.immutable.List

object ExampleData extends ExampleData

trait ExampleData
  extends ExampleFormTemplate
  with ExampleFieldId
  with ExampleFieldValue
  with ExampleFormField
  with ExampleSection
  with ExampleSectionNumber
  with ExampleForm
  with ExampleAuthConfig
  with ExampleFrontendAppConfig
  with ExampleAuthContext

trait ExampleAuthConfig {

  def authConfigModule = AuthConfigModule("hmrc")

  def dmsSubmission = DmsSubmission("DMS-ID-XX", TextExpression(AuthCtx(PayeNino)), "BT-NRU-Environmental", "FinanceOpsCorpT")

  def regimeId = RegimeId("TestRegimeId")

  def serviceId = ServiceId("TestServiceId")

  def authConfig = HMRCAuthConfigWithRegimeId(authConfigModule, serviceId, regimeId)
}

trait ExampleFieldId {

  def `fieldId - facePhoto` = FormComponentId("facePhoto")
  def `fieldId - surname` = FormComponentId("surname")
  def `fieldId - firstName` = FormComponentId("firstName")
  def `fieldId - iptRegNum` = FormComponentId("iptRegNum")
  def `fieldId - businessName` = FormComponentId("nameOfBusiness")
  def `fieldId - startDate` = FormComponentId("startDate")

  def `fieldId - startDate-year` = FormComponentId("startDate-year")
  def `fieldId - startDate-day` = FormComponentId("startDate-day")
  def `fieldId - startDate-month` = FormComponentId("startDate-month")
  def `fieldId - number` = FormComponentId("number")
  def default = FormComponentId("test")

  //fieldId when submitting form
  def `fieldId - save` = FormComponentId("save")

}

trait ExampleFieldValue { dependecies: ExampleFieldId =>

  def `fieldValue - facePhoto` = FormComponent(
    `fieldId - facePhoto`,
    FileUpload(), label = "Attach evidence of your smile", helpText = None, None, None, mandatory = true, editable = true, submissible = true, derived = true,
    false, None, None
  )

  def `fieldValue - firstName` = FormComponent(
    `fieldId - firstName`,
    Text(AnyText, Constant("any text")), "First Name", None, None, None, mandatory = true, editable = true, submissible = true, derived = true,
    false, None, None
  )

  def `fieldValue - surname` = FormComponent(
    `fieldId - surname`,
    Text(AnyText, Constant("any text")), "Last Name", None, None, None, mandatory = true, editable = true, submissible = true, derived = true,
    false, None, None
  )

  def `fieldValue - iptRegNum` = FormComponent(
    `fieldId - iptRegNum`,
    Text(AnyText, Constant("any text")), "Insurance Premium Tax (IPT) number", None, None, None, mandatory = true, editable = true, submissible = true, derived = true,
    false, None, None
  )

  def `fieldValue - businessName` = FormComponent(
    `fieldId - businessName`,
    Text(AnyText, Constant("any text")), "Name of business", None, None, None, mandatory = true, editable = true, submissible = true, derived = true,
    false, None, None
  )

  def `fieldValue - startDate` = FormComponent(
    `fieldId - startDate`,
    Date(AnyDate, Offset(0), None), "Your Start Date", None, None, None, true, true, true, true, false,
    None, None
  )

  def `fieldValue - info` = FormComponent(
    `fieldId - businessName`,
    InformationMessage(NoFormat, "some text"),
    "someLabel",
    None, None, None, false, false, false, true, false, None
  )

  def `group - type` = Group(
    fields = List(`fieldValue - firstName`),
    orientation = Horizontal,
    repeatsMax = None,
    repeatsMin = None,
    repeatLabel = None,
    repeatAddAnotherText = None
  )

  def `fieldValue - group` = FormComponent(
    id = FormComponentId("GroupFieldValueId"),
    `type` = `group - type`,
    label = "group FieldValue label",
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
    Text(Number(), Constant("")),
    "sample label", None, None, None, true, false, false, false, false, None
  )

  def `fieldValue - number - validIf` = FormComponent(
    `fieldId - number`,
    Text(Number(), Constant("")),
    "sample label", None, None, None, true, false, false, false, false, None
  )

  def fieldValue(text: Text) = FormComponent(
    default,
    text,
    "sample label", None, None, None, true, false, false, true, false, None
  )

  def listFieldValue = List()

}

trait ExampleSectionNumber {
  val `sectionNumber-1` = SectionNumber(-1)
  val sectionNumber0 = SectionNumber(0)
  val sectionNumber1 = SectionNumber(1)
  val sectionNumber2 = SectionNumber(2)
  val sectionNumber3 = SectionNumber(3)
}
trait ExampleSection { dependecies: ExampleFieldId with ExampleFieldValue =>

  def `section - about you` = Section("About you", None, None, None, None, None, None, None, List(
    `fieldValue - firstName`,
    `fieldValue - surname`,
    `fieldValue - facePhoto`
  ))

  def `section - businessDetails` = Section("Business details", None, None, None, None, None, None, None, List(
    `fieldValue - businessName`,
    `fieldValue - startDate`,
    `fieldValue - iptRegNum`
  ))

  def `repeating section` = Section(
    "Repeating section",
    None, None, None, None,
    repeatsMax = Some(TextExpression(FormCtx(`fieldId - firstName`.value))),
    repeatsMin = Some(TextExpression(FormCtx(`fieldId - firstName`.value))),
    None,
    List(
      `fieldValue - surname`
    )
  )

  def `section - group` = `section - about you`.copy(fields = List(`fieldValue - group`))

  def allSections = List(
    `section - about you`,
    `section - businessDetails`
  )
}

trait ExampleFormTemplate { dependsOn: ExampleAuthConfig with ExampleSection with ExampleFieldId with ExampleFieldValue =>

  def formTemplateId = FormTemplateId("AAA999")

  def formName = "AAA999 dev test template"

  def formDescription = "Fill in your insurance premium tax return form online | Llenwch eich ffurflen dreth premiwm yswiriant ar-lein"

  def submtSuccessUrl = """http://success.com"""

  def submitErrorUrl = """http://imsorry.com"""

  def acknowledgementSection =
    AcknowledgementSection("Acknowledgement Page", Some("this page is to acknowledge submission"), Some("shortName for acknowledgement"), List(`fieldValue - info`))

  def formTemplate = {
    FormTemplate(
      _id = formTemplateId,
      formName = formName,
      description = formDescription,
      formCategory = Some(Default),
      submissionReference = None,
      dmsSubmission = dmsSubmission,
      authConfig = authConfig,
      submitSuccessUrl = submtSuccessUrl,
      submitErrorUrl = submitErrorUrl,
      sections = allSections,
      acknowledgementSection = acknowledgementSection,
      declarationSection = DeclarationSection("Declaration", None, None, Nil)
    )
  }
}

trait ExampleFormField { dependsOn: ExampleFormTemplate with ExampleFieldId =>

  def `formField - facePhoto` = FormField(`fieldId - facePhoto`, "face-photo.jpg")
  def `formField - firstName` = FormField(`fieldId - firstName`, "James")
  def `formField - surname` = FormField(`fieldId - surname`, "Bond")
  def `formField - iptRegNum` = FormField(`fieldId - iptRegNum`, "666CasinoRoyale")
  def `formField - businessName` = FormField(`fieldId - businessName`, "Quantum of Solace")
  def `formField - startDateDay` = FormField(`fieldId - startDate-day`, "11")
  def `formField - startDateMonth` = FormField(`fieldId - startDate-month`, "10")
  def `formField - startDateYear` = FormField(`fieldId - startDate-year`, "2008")
  def `formField - number` = FormField(`fieldId - number`, "1,234")

  //actions:

  def `formField - Save` = FormField(`fieldId - save`, "Save")
  def `formField - Continue` = FormField(`fieldId - save`, "Continue")
  def `formField - Back` = FormField(`fieldId - save`, "Back")
  def `formField - AddGroup` = FormField(`fieldId - save`, "AddGroup")
  def `formField - RemoveGroup` = FormField(`fieldId - save`, "RemoveGroup")

  def data: Map[FormComponentId, FormField] = Map(
    `fieldId - save` -> `formField - Save`,
    `fieldId - facePhoto` -> `formField - facePhoto`,
    `fieldId - firstName` -> `formField - firstName`,
    `fieldId - surname` -> `formField - surname`,
    `fieldId - iptRegNum` -> `formField - iptRegNum`,
    `fieldId - startDate-year` -> `formField - startDateYear`,
    `fieldId - startDate-month` -> `formField - startDateMonth`,
    `fieldId - startDate-day` -> `formField - startDateDay`,
    `fieldId - businessName` -> `formField - businessName`,
    `fieldId - number` -> `formField - number`
  )

  def rawDataFromBrowser: Map[FormComponentId, Seq[String]] = data.mapValues(x => Seq(x.value))
}

trait ExampleForm { dependsOn: ExampleFormField with ExampleFormTemplate =>

  def userId = UserId("James007")

  def formId = FormId(userId, formTemplateId)

  def formFields: Seq[FormField] = data.values.toSeq

  def formData = FormData(formFields)

  def repeatingGroupStructure: Option[RepeatingGroupStructure] = None

  def formDataNil = FormData(fields = Nil)

  def envelopeId = EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307")

  def envelope = Envelope(Nil)

  def form = Form(
    formId,
    envelopeId,
    userId,
    formTemplateId,
    repeatingGroupStructure,
    formData,
    InProgress
  )

}

trait ExampleAuthContext {

  def authContext =
    Retrievals(
      authProviderId = authProviderId,
      enrolments = enrolments,
      affinityGroup = affinityGroup,
      internalId = internalId,
      externalId = externalId,
      userDetails = userDetails,
      credentialStrength = credentialStrength,
      agentCode = agentCode
    )

  def authProviderId =
    OneTimeLogin

  def affinityGroup: Option[AffinityGroup] =
    None

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

  def userDetails =
    UserDetails(
      authProviderId = None,
      authProviderType = None,
      name = "test details",
      affinityGroup = Organisation,
      groupIdentifier = "TestGroupId"
    )
}

trait ExampleFrontendAppConfig {

  val frontendAppConfig = FrontendAppConfig(
    assetsPrefix = "assetsPrefix",
    analyticsToken = "analyticsToken",
    analyticsHost = "analyticsHost",
    reportAProblemPartialUrl = "http://reportProblem.url",
    reportAProblemNonJSUrl = "http://reportProblem.json.url",
    governmentGatewaySignInUrl = "http://gofernment.gateway.signin.url",
    gformFrontendBaseUrl = "gform.frontend.base.url",
    betaFeedbackUrlNoAuth = "beta.feedback.url.no.auth",
    signOutUrl = "http://localhost:9025/loggedout",
    whitelistEnabled = true,
    sendPdfWithSubmission = true
  )
}