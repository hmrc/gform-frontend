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

package uk.gov.hmrc.gform.services

import cats.MonadError
import cats.data.NonEmptyList
import org.jsoup.Jsoup
import org.jsoup.select.Elements
import org.mockito.ArgumentMatchersSugar
import org.mockito.scalatest.IdiomaticMockito
import play.api.i18n.{ I18nSupport, Messages, MessagesApi }
import play.api.mvc.{ AnyContentAsEmpty, Request }
import uk.gov.hmrc.gform.FormTemplateKey
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, Role }
import uk.gov.hmrc.gform.config.FileInfoConfig
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.smartstring.{ RealSmartStringEvaluatorFactory, SmartStringEvaluator }
import uk.gov.hmrc.gform.eval.{ EvaluationContext, FileIdsWithMapping }
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.gform.SectionRenderingService
import uk.gov.hmrc.gform.gform.handlers.FormHandlerResult
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.graph.{ Recalculation, RecalculationResult }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, NotChecked, SourceOrigin, VariadicFormData }
import uk.gov.hmrc.gform.summary.AddressRecordLookup
import uk.gov.hmrc.gform.upscan.UpscanInitiate
import uk.gov.hmrc.gform.validation.ValidationResult
import uk.gov.hmrc.http.HeaderCarrier

import scala.collection.immutable.List
import scala.concurrent.Future
import uk.gov.hmrc.gform.lookup.LocalisedLookupOptions

class SectionRenderingServiceSpec extends Spec with ArgumentMatchersSugar with IdiomaticMockito {

  val testService = new SectionRenderingService(frontendAppConfig, new LookupRegistry(Map.empty))

  trait TestFixture {

    val mockRecalculation = mock[Recalculation[Future, Throwable]]

    val mssgApi: MessagesApi = play.api.test.Helpers.stubMessagesApi()

    implicit val messages: Messages = play.api.test.Helpers.stubMessages(mssgApi)

    val i18nSupport: I18nSupport = new I18nSupport {
      override def messagesApi: MessagesApi = mssgApi
    }

    implicit val langADT: LangADT = LangADT.En

    implicit val hc: HeaderCarrier = HeaderCarrier()

    lazy val addToListQuestionComponent: FormComponent = addToListQuestion("addToListQuestion")

    lazy val form: Form = buildForm
    lazy val formTemplate: FormTemplate = buildFormTemplate

    implicit val request: Request[AnyContentAsEmpty.type] =
      FakeRequest().addAttr(FormTemplateKey, FormTemplateContext(formTemplate, None, None, None, None))
    lazy val validationResult = ValidationResult.empty
    lazy val cache = AuthCacheWithForm(
      authContext,
      form,
      FormTemplateContext.basicContext(formTemplate, None),
      Role.Customer,
      Some(accessCode),
      LocalisedLookupOptions(Map())
    )

    lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.Normal](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue

    mockRecalculation.recalculateFormDataNew(
      *[VariadicFormData[SourceOrigin.OutOfDate]],
      *[FormModel[Interim]],
      *[FormTemplate],
      *[MaterialisedRetrievals],
      *[ThirdPartyData],
      *[EvaluationContext],
      *[Messages]
    )(*[MonadError[Future, Throwable]]) returns Future.successful(
      RecalculationResult.empty(
        EvaluationContext(
          formTemplate._id,
          submissionRef,
          Some(accessCode),
          authContext,
          ThirdPartyData.empty,
          authConfig,
          hc,
          Option.empty[FormPhase],
          FileIdsWithMapping.empty,
          Map.empty,
          Set.empty,
          Set.empty,
          Set.empty,
          Map.empty,
          LangADT.En,
          messages,
          List.empty,
          Set.empty,
          FileSizeLimit(1),
          LocalisedLookupOptions(Map()),
          DataRetrieveAll.empty,
          Map.empty,
          Set.empty
        )
      )
    )

    implicit val smartStringEvaluator: SmartStringEvaluator = new RealSmartStringEvaluatorFactory(messages)
      .apply(formModelOptics.formModelVisibilityOptics)
  }

  "renderSection" should "render text field with TelephoneNumber constraint as input field with type='tel'" in new TestFixture {

    lazy val phoneNumberField = mkFormComponent("phoneNumber", Text(TelephoneNumber, Value))

    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(phoneNumberField.modelComponentId, "1234567890")
          )
        )
      )

    lazy val sections = List(nonRepeatingPageSection(fields = List(phoneNumberField)))
    override lazy val formTemplate = buildFormTemplate(destinationList, sections)

    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        SectionNumber.Classic(0),
        FormHandlerResult(ValidationResult.empty, EnvelopeWithMapping.empty),
        formTemplate,
        None,
        envelopeId,
        formModelOptics.formModelRenderPageOptics.formModel.pages.head.asInstanceOf[Singleton[DataExpanded]],
        0,
        FileInfoConfig.allAllowedFileTypes,
        Nil,
        authContext,
        NotChecked,
        List(FastForward.Yes),
        formModelOptics,
        UpscanInitiate.empty,
        AddressRecordLookup.from(ThirdPartyData.empty)
      )

    val phoneField = Jsoup.parse(generatedHtml.body).getElementById("phoneNumber")

    phoneField.attr("type") shouldBe "tel"
  }

  it should "render text field with given labelsize" in new TestFixture {

    lazy val textField = mkFormComponentWithLabelSize("st", Text(ShortText.default, Value), Some(Medium))

    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(textField.modelComponentId, "testLabelSize")
          )
        )
      )

    lazy val sections = List(nonRepeatingPageSection(fields = List(textField)))
    override lazy val formTemplate = buildFormTemplate(destinationList, sections)

    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        SectionNumber.Classic(0),
        FormHandlerResult(ValidationResult.empty, EnvelopeWithMapping.empty),
        formTemplate,
        None,
        envelopeId,
        formModelOptics.formModelRenderPageOptics.formModel.pages.head.asInstanceOf[Singleton[DataExpanded]],
        0,
        FileInfoConfig.allAllowedFileTypes,
        Nil,
        authContext,
        NotChecked,
        List(FastForward.Yes),
        formModelOptics,
        UpscanInitiate.empty,
        AddressRecordLookup.from(ThirdPartyData.empty)
      )

    val textFieldHtml = Jsoup.parse(generatedHtml.body).getElementsByClass("govuk-label")
    textFieldHtml.attr("class") should include("govuk-label--m")
  }

  it should "render section without Save and come back later button for draftRetrievalMethod is NotPermitted " in new TestFixture {

    lazy val textField = mkFormComponentWithLabelSize("st", Text(ShortText.default, Value), Some(Medium))

    override lazy val form: Form =
      buildForm(
        FormData(
          List(
            FormField(textField.modelComponentId, "testLabelSize")
          )
        )
      )

    lazy val sections = List(nonRepeatingPageSection(fields = List(textField)))
    override lazy val formTemplate = buildFormTemplate(destinationList, sections)

    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        SectionNumber.Classic(0),
        FormHandlerResult(ValidationResult.empty, EnvelopeWithMapping.empty),
        formTemplate.copy(draftRetrievalMethod = NotPermitted),
        None,
        envelopeId,
        formModelOptics.formModelRenderPageOptics.formModel.pages.head.asInstanceOf[Singleton[DataExpanded]],
        0,
        FileInfoConfig.allAllowedFileTypes,
        Nil,
        authContext,
        NotChecked,
        List(FastForward.Yes),
        formModelOptics,
        UpscanInitiate.empty,
        AddressRecordLookup.from(ThirdPartyData.empty)
      )

    val textFieldHtml: Elements = Jsoup.parse(generatedHtml.body).select("button[type=submit][class=govuk-button]")
    textFieldHtml.attr("class") shouldNot include("govuk-button--secondary")
  }

  it should "render page with noPIITitle if defined in template" in new TestFixture {

    lazy val textField = mkFormComponent("someTextField", Value)

    override lazy val form: Form =
      buildForm(
        FormData(
          List.empty
        )
      )
    override lazy val formTemplate = buildFormTemplate(
      destinationList,
      List(
        nonRepeatingPageSection(
          title = "Some title",
          noPIITitle = Some("Some title without PII"),
          fields = List(textField)
        )
      )
    )

    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        SectionNumber.Classic(0),
        FormHandlerResult(ValidationResult.empty, EnvelopeWithMapping.empty),
        formTemplate.copy(draftRetrievalMethod = NotPermitted),
        None,
        envelopeId,
        formModelOptics.formModelRenderPageOptics.formModel.pages.head.asInstanceOf[Singleton[DataExpanded]],
        0,
        FileInfoConfig.allAllowedFileTypes,
        Nil,
        authContext,
        NotChecked,
        List(FastForward.Yes),
        formModelOptics,
        UpscanInitiate.empty,
        AddressRecordLookup.from(ThirdPartyData.empty)
      )

    Jsoup.parse(generatedHtml.body).title() shouldBe "Some title without PII - AAA999 dev test template - GOV.UK"
  }

  "renderDeclarationSection" should "render Declaration page with Button with text 'ContinueLabel'" in new TestFixture {

    override lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.WithDeclaration](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue

    val generatedHtml = testService
      .renderDeclarationSection(
        Some(accessCode),
        formTemplate,
        formModelOptics.formModelRenderPageOptics.formModel.pages.last.asInstanceOf[Singleton[DataExpanded]],
        authContext,
        ValidationResult.empty,
        formModelOptics
      )

    val declarationPageButton = Jsoup.parse(generatedHtml.body).select("button[type=submit][class=govuk-button]").first

    declarationPageButton.text shouldBe "ContinueLabel"
  }

  it should "render Declaration page with noPIITitle when defined" in new TestFixture {

    override lazy val formTemplate = buildFormTemplate(
      DestinationList(NonEmptyList.of(hmrcDms), ackSection, Some(mkDecSection(Some("Some noPII dec section title")))),
      List(
        nonRepeatingPageSection(
          title = "Some title",
          fields = List(mkFormComponent("someTextField", Value))
        )
      )
    )

    override lazy val formModelOptics: FormModelOptics[DataOrigin.Mongo] = FormModelOptics
      .mkFormModelOptics[DataOrigin.Mongo, Future, SectionSelectorType.WithDeclaration](
        cache.variadicFormData[SectionSelectorType.WithDeclaration],
        cache,
        mockRecalculation
      )
      .futureValue

    val generatedHtml = testService
      .renderDeclarationSection(
        Some(accessCode),
        formTemplate,
        formModelOptics.formModelRenderPageOptics.formModel.pages.last.asInstanceOf[Singleton[DataExpanded]],
        authContext,
        ValidationResult.empty,
        formModelOptics
      )

    Jsoup.parse(generatedHtml.body).title() shouldBe "Some noPII dec section title - AAA999 dev test template - GOV.UK"
  }

  "renderEnrolmentSection" should "render Enrolment page with noPIITitle when defined" in new TestFixture {

    val generatedHtml = testService.renderEnrolmentSection(
      formTemplate,
      Singleton[DataExpanded](enrolmentSection.toSection.page.asInstanceOf[Page[DataExpanded]]),
      authContext,
      formModelOptics,
      List.empty,
      ValidationResult.empty
    )
    Jsoup
      .parse(generatedHtml.body)
      .title() shouldBe "Some noPII enrolment section title - AAA999 dev test template - GOV.UK"
  }

  /* "SectionRenderingService" should "set a field to hidden if is onlyShowOnSummary is set to true" in {
   * val generatedHtml = testService
   *     .renderSection(
   *       Some(accessCode),
   *       form,
   *       SectionNumber.firstSection,
   *       mkFormDataRecalculated(
   *         VariadicFormData.ones(
   *           FormComponentId("nameOfBusiness")  -> "",
   *           FormComponentId("startDate-day")   -> "",
   *           FormComponentId("startDate-month") -> "",
   *           FormComponentId("startDate-year")  -> "",
   *           FormComponentId("iptRegNum")       -> ""
   *         )),
   *       formTemplate,
   *       Nil,
   *       Envelope.empty,
   *       envelopeId,
   *       ValidationResult.empty.valid,
   *       allSections.map(sc => sc.updateFields(sc.page.fields.map(f => f.copy(onlyShowOnSummary = true)))),
   *       0,
   *       Nil,
   *       retrievals,
   *       NotChecked
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
   *   val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
   *
   *   hiddenFieldNames should be(
   *     List(
   *       "csrfToken",
   *       "firstName",
   *       "surname",
   *       "timeOfCall",
   *       "nameOfBusiness",
   *       "startDate-day",
   *       "startDate-month",
   *       "startDate-year",
   *       "iptRegNum",
   *       "hiddenfacePhoto",
   *       "save"
   *     ))
   *   visibleFields should be(List())
   * }
   * "SectionRenderingService" should "add in progress indicator if it is defined" in {
   *   val generatedHtml = testService
   *     .renderSection(
   *       Some(accessCode),
   *       form,
   *       SectionNumber.firstSection,
   *       FormDataRecalculated.empty,
   *       formTemplate,
   *       Nil,
   *       Envelope.empty,
   *       envelopeId,
   *       ValidationResult.empty.valid,
   *       List(allSections.head.updateProgressIndicator(Some(toSmartString("Progress Indicator")))),
   *       0,
   *       Nil,
   *       retrievals,
   *       NotChecked
   *     )
   *   val doc: Document = Jsoup.parse(generatedHtml.body)
   *   val progressIndicator = doc.getElementsByClass("hmrc-caption-xl").first()
   *   progressIndicator.toString should be(
   *     """<p class="govuk-!-margin-bottom-0 govuk-caption-m hmrc-caption-xl"><span class="govuk-visually-hidden">this.section.is_value</span>Progress Indicator</p>""")
   * }
   *
   * it should "generate second page" in {
   *   val generatedHtml = testService
   *     .renderSection(
   *       Some(accessCode),
   *       form,
   *       SectionNumber(1),
   *       mkFormDataRecalculated(
   *         VariadicFormData.ones(
   *           FormComponentId("firstName") -> "",
   *           FormComponentId("surname")   -> "",
   *           FormComponentId("facePhoto") -> ""
   *         )),
   *       formTemplate,
   *       Nil,
   *       Envelope.empty,
   *       envelopeId,
   *       ValidationResult.empty.valid,
   *       allSections,
   *       0,
   *       Nil,
   *       retrievals,
   *       NotChecked
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *   val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
   *   val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
   *
   *   hiddenFieldNames should be(List("csrfToken", "firstName", "surname", "facePhoto", "save"))
   *   visibleFields should be(List("nameOfBusiness", "startDate-day", "startDate-month", "startDate-year", "iptRegNum"))
   * }
   *
   * it should "return the HMTL representation of provided markdown" in new ExampleData {
   *
   *   override def `fieldValue - firstName` = {
   *     val markdown =
   *       """
   *         |You are now seeing markdown.
   *         |
   *         |# This is an H1
   *         |## This is an H2
   *         |### This is an H3
   *         |
   *         |Link:
   *         |
   *         |[This is a link](https://avatars1.githubusercontent.com/u/5755339?v=3&s=200)
   *         |
   *         |An image: ![Alt Text](/template/assets/images/gov.uk_logotype_crown.png)
   *         |
   *         |Now some code:
   *         |```
   *         |This is some code
   *         |Second line of code
   *         |Third line of code
   *         |```
   *         |
   *         |Ordered list
   *         |
   *         |1. One
   *         |2. Two
   *         |3. Three
   *         |
   *         |Unordered list
   *         |
   *         |* element one
   *         |* element two
   *         |* element three
   *         |
   *         || Table         | Col           | name  |
   *         || ------------- |:-------------:| -----:|
   *         || col 3 is      | right-aligned | $1600 |
   *         || col 2 is      | centered      |   $12 |
   *         || aa a a a a aa | bbbbbbbbb     |    $1 |
   *         |
   *     """.stripMargin
   *
   *     FormComponent(
   *       id = FormComponentId("testInfoField"),
   *       `type` = InformationMessage(StandardInfo, toSmartString(markdown)),
   *       label = toSmartString("This is the field label"),
   *       helpText = None,
   *       shortName = None,
   *       validIf = None,
   *       mandatory = true,
   *       editable = false,
   *       submissible = false,
   *       derived = true,
   *       errorMessage = None
   *     )
   *   }
   *
   *   override def `section - about you` =
   *     super.`section - about you`.updateFields(List(`fieldValue - firstName`))
   *
   *   override def allSections = List(
   *     `section - about you`
   *   )
   *
   *   val generatedHtml = testService
   *     .renderSection(
   *       Some(accessCode),
   *       form,
   *       SectionNumber(0),
   *       FormDataRecalculated.empty,
   *       formTemplate,
   *       Nil,
   *       Envelope.empty,
   *       envelopeId,
   *       ValidationResult.empty.valid,
   *       allSections,
   *       0,
   *       Nil,
   *       retrievals,
   *       NotChecked
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val infoFieldDiv = doc.getElementsByClass("govuk-inset-text").first
   *
   *   infoFieldDiv.getElementsByTag("H1").last.text shouldBe "This is an H1"
   *   infoFieldDiv.getElementsByTag("H2").first.text shouldBe "This is an H2"
   *   infoFieldDiv.getElementsByTag("H3").first.text shouldBe "This is an H3"
   *   infoFieldDiv.getElementsByTag("A").size() shouldBe 1
   *   infoFieldDiv.getElementsByTag("IMG").size() shouldBe 1
   *   infoFieldDiv.getElementsByTag("CODE").size() shouldBe 1
   *   infoFieldDiv.getElementsByTag("PRE").size() shouldBe 1
   *   infoFieldDiv.getElementsByTag("OL").size() shouldBe 1
   *   infoFieldDiv.getElementsByTag("UL").size() shouldBe 1
   *
   *   val table = infoFieldDiv.getElementsByTag("TABLE")
   *   table.size() shouldBe 1
   *   table.first.getElementsByTag("TR").size shouldBe 4
   *   table.first.getElementsByTag("TD").size shouldBe 9
   * }
   *
   * it should "return HTML with dynamic groups and an add-group button (repeating groups)" in new ExampleData {
   *
   *   override def `group - type` = Group(
   *     fields = List(`fieldValue - firstName`),
   *     repeatsMax = Some(3),
   *     repeatsMin = Some(1),
   *     repeatLabel = Some(toSmartString("REPEAT_LABEL")),
   *     repeatAddAnotherText = Some(toSmartString("repeatAddAnotherText"))
   *   )
   *
   *   override def allSections = List(
   *     `section - group`
   *   )
   *
   *   val generatedHtml = testService
   *     .renderSection(
   *       Some(accessCode),
   *       form,
   *       SectionNumber(0),
   *       FormDataRecalculated.empty,
   *       formTemplate,
   *       Nil,
   *       Envelope.empty,
   *       envelopeId,
   *       ValidationResult.empty.valid,
   *       allSections,
   *       0,
   *       Nil,
   *       retrievals,
   *       NotChecked
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val addButtonValue = "AddGroup-" + `fieldValue - group`.id.value
   *   val fieldName = `fieldValue - firstName`.id.value
   *   doc
   *     .getElementsByAttributeValue("href", addButtonValue)
   *     .size shouldBe 1 withClue "no limit reached, add button shown"
   *   doc.getElementsByAttributeValue("name", fieldName).size shouldBe 1 withClue "One repeat element"
   *   doc.getElementsContainingOwnText("REPEAT_LABEL").size shouldBe 1
   * }
   *
   * it should "hide add-group button when limit has been reached (repeating groups)" in new ExampleData {
   *
   *   val thisTestService = new SectionRenderingService(frontendAppConfig, lookupRegistry)
   *
   *   override def `group - type` = Group(
   *     fields = List(`fieldValue - firstName`),
   *     repeatsMax = Some(2),
   *     repeatsMin = Some(1),
   *     repeatLabel = Some(toSmartString("REPEAT_LABEL")),
   *     repeatAddAnotherText = Some(toSmartString("repeatAddAnotherText"))
   *   )
   *
   *   override def allSections = List(
   *     `section - group`
   *   )
   *
   *   val generatedHtml = thisTestService
   *     .renderSection(
   *       Some(accessCode),
   *       form,
   *       SectionNumber(0),
   *       mkFormDataRecalculated(
   *         VariadicFormData.ones(
   *           FormComponentId("firstName")   -> "",
   *           FormComponentId("1_firstName") -> ""
   *         )),
   *       formTemplate,
   *       Nil,
   *       Envelope.empty,
   *       envelopeId,
   *       ValidationResult.empty.valid,
   *       allSections,
   *       0,
   *       Nil,
   *       retrievals,
   *       NotChecked
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val addButtonValue = "AddGroup-" + `fieldValue - group`.id.value
   *   val fieldName = `fieldValue - firstName`.id.value
   *   doc.getElementsByAttributeValue("href", addButtonValue).size shouldBe 0
   *   doc.getElementsByAttributeValue("name", fieldName).size shouldBe 1
   *   doc.getElementsByAttributeValue("name", "1_" + fieldName).size shouldBe 1
   *   doc.getElementsContainingOwnText(messages("linkText.removeRepeatedGroup")).size shouldBe 2
   * }
   *
   * it should "generate declaration page" in {
   *   val generatedHtml = testService
   *     .renderDeclarationSection(
   *       Some(accessCode),
   *       form,
   *       formTemplate,
   *       decSection,
   *       retrievals,
   *       ValidationResult.empty.valid,
   *       FormDataRecalculated.empty,
   *       Nil
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
   *   val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
   *   val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml().trim())
   *
   *   hiddenFieldNames should be(List("csrfToken", "save"))
   *   visibleFields should be(List("fieldInDeclarationSections"))
   *   buttons should be(List(messages("button.acceptAndSubmit")))
   * }
   *
   * it should "generate declaration page with submit claim button" in {
   *   val generatedHtml = testService
   *     .renderDeclarationSection(
   *       Some(accessCode),
   *       form,
   *       formTemplate.copy(formCategory = HMRCClaimForm),
   *       decSection,
   *       retrievals,
   *       ValidationResult.empty.valid,
   *       FormDataRecalculated.empty,
   *       Nil
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
   *   val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
   *   val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml().trim())
   *
   *   hiddenFieldNames should be(List("csrfToken", "save"))
   *   visibleFields should be(List("fieldInDeclarationSections"))
   *   buttons should be(List(messages("button.acceptAndSubmitForm")))
   * }
   *
   * it should "generate declaration page with submit return button" in {
   *   val generatedHtml = testService
   *     .renderDeclarationSection(
   *       Some(accessCode),
   *       form,
   *       formTemplate.copy(formCategory = HMRCReturnForm),
   *       decSection,
   *       retrievals,
   *       ValidationResult.empty.valid,
   *       FormDataRecalculated.empty,
   *       Nil
   *     )
   *
   *   val doc = Jsoup.parse(generatedHtml.body)
   *
   *   val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
   *   val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
   *   val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml().trim())
   *
   *   hiddenFieldNames should be(List("csrfToken", "save"))
   *   visibleFields should be(List("fieldInDeclarationSections"))
   *   buttons should be(List(messages("button.acceptAndSubmitForm")))
   * } */
}
