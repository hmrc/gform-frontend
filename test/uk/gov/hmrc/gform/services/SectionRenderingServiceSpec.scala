/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.syntax.validated._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.jsoup.select.Elements
import play.api.i18n.{ Lang, Messages }
import play.api.libs.typedmap.{ TypedEntry, TypedMap }
import play.api.mvc.{ AnyContentAsEmpty, Request }
import play.api.test.FakeRequest
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gform.SectionRenderingService
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, NotChecked, VariadicFormData }
import uk.gov.hmrc.gform.views.{ ViewHelpers, ViewHelpersAlgebra }
import uk.gov.hmrc.http.HeaderCarrier
import play.filters.csrf.CSRF
import play.filters.csrf.CSRF.{ Token, TokenInfo }
import play.twirl.api.{ Html, HtmlFormat }

import scala.collection.JavaConverters
import scala.collection.immutable.List

class SectionRenderingServiceSpec extends Spec {

  implicit val request: Request[AnyContentAsEmpty.type] =
    FakeRequest()
      .withAttrs(TypedMap(TypedEntry(CSRF.Token.InfoAttr, TokenInfo(Token("csrfToken", "Bar")))))

  val retrievals = authContext

  private val lookupRegistry = new LookupRegistry(Map.empty)

  private implicit val langADT = LangADT.En

  private implicit val messages = new Messages {
    override def lang: Lang = Lang.defaultLang

    override def apply(key: String, args: Any*): String = key + "_value"

    override def apply(keys: Seq[String], args: Any*): String = keys.mkString("_")

    override def translate(key: String, args: Seq[Any]): Option[String] = Some(apply(key))

    override def isDefinedAt(key: String): Boolean = true
  }

  private implicit val hc: HeaderCarrier = HeaderCarrier()

  private implicit val viewHelpers: ViewHelpersAlgebra = new ViewHelpers(null, null, null, null) {
    override def head(linkElem: Option[Html], headScripts: Option[Html]): HtmlFormat.Appendable = null

    override def footer(
      analyticsToken: scala.Option[scala.Predef.String],
      analyticsHost: scala.Predef.String,
      ssoUrl: scala.Option[scala.Predef.String],
      scriptElem: scala.Option[play.twirl.api.Html],
      gaCalls: scala.Option[(String, String) => Html],
      analyticsAnonymizeIp: scala.Boolean,
      analyticsAdditionalJs: scala.Option[play.twirl.api.Html],
      allowQueryStringInAnalytics: scala.Boolean): HtmlFormat.Appendable =
      null

    override def webchatClickToChatScriptPartial(entryPoint: String, template: String)(implicit request: Request[_]) =
      Html("WebChat stuff goes here")
  }

  val testService = new SectionRenderingService(frontendAppConfig, lookupRegistry)

  "SectionRenderingService" should "generate first page" in {
    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber.firstSection,
        mkFormDataRecalculated(
          VariadicFormData.ones(
            FormComponentId("nameOfBusiness")  -> "",
            FormComponentId("startDate-day")   -> "",
            FormComponentId("startDate-month") -> "",
            FormComponentId("startDate-year")  -> "",
            FormComponentId("iptRegNum")       -> ""
          )),
        formTemplate.copy(webChat = None),
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        allSections,
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))

    hiddenFieldNames should be(
      List(
        "csrfToken",
        "nameOfBusiness",
        "startDate-day",
        "startDate-month",
        "startDate-year",
        "iptRegNum",
        "facePhoto",
        "save"))
    visibleFields should be(List("firstName", "surname"))
  }

  "SectionRenderingService" should "set a field to hidden if is onlyShowOnSummary is set to true" in {
    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber.firstSection,
        mkFormDataRecalculated(
          VariadicFormData.ones(
            FormComponentId("nameOfBusiness")  -> "",
            FormComponentId("startDate-day")   -> "",
            FormComponentId("startDate-month") -> "",
            FormComponentId("startDate-year")  -> "",
            FormComponentId("iptRegNum")       -> ""
          )),
        formTemplate,
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        allSections.map(sc => sc.copy(fields = sc.fields.map(f => f.copy(onlyShowOnSummary = true)))),
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))

    hiddenFieldNames should be(
      List(
        "csrfToken",
        "firstName",
        "surname",
        "nameOfBusiness",
        "startDate-day",
        "startDate-month",
        "startDate-year",
        "iptRegNum",
        "facePhoto",
        "save"))
    visibleFields should be(List())
  }
  "SectionRenderingService" should "add in progress indicator if it is defined" in {
    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber.firstSection,
        FormDataRecalculated.empty,
        formTemplate,
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        List(allSections.head.copy(progressIndicator = Some(toSmartString("Progress Indicator")))),
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc: Document = Jsoup.parse(generatedHtml.body)
    val progressIndicator = doc.getElementById("progress-indicator")
    progressIndicator.toString should be(
      "<span id=\"progress-indicator\" class=\"heading-caption\">Progress Indicator</span>")
  }

  it should "generate second page" in {
    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber(1),
        mkFormDataRecalculated(
          VariadicFormData.ones(
            FormComponentId("firstName") -> "",
            FormComponentId("surname")   -> "",
            FormComponentId("facePhoto") -> ""
          )),
        formTemplate,
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        allSections,
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))

    hiddenFieldNames should be(List("csrfToken", "firstName", "surname", "facePhoto", "save"))
    visibleFields should be(List("nameOfBusiness", "startDate-day", "startDate-month", "startDate-year", "iptRegNum"))
  }

  it should "return the HMTL representation of provided markdown" in new ExampleData {

    override def `fieldValue - firstName` = {
      val markdown =
        """
          |You are now seeing markdown.
          |
          |# This is an H1
          |## This is an H2
          |### This is an H3
          |
          |Link:
          |
          |[This is a link](https://avatars1.githubusercontent.com/u/5755339?v=3&s=200)
          |
          |An image: ![Alt Text](/template/assets/images/gov.uk_logotype_crown.png)
          |
          |Now some code:
          |```
          |This is some code
          |Second line of code
          |Third line of code
          |```
          |
          |Ordered list
          |
          |1. One
          |2. Two
          |3. Three
          |
          |Unordered list
          |
          |* element one
          |* element two
          |* element three
          |
          || Table         | Col           | name  |
          || ------------- |:-------------:| -----:|
          || col 3 is      | right-aligned | $1600 |
          || col 2 is      | centered      |   $12 |
          || aa a a a a aa | bbbbbbbbb     |    $1 |
          |
      """.stripMargin

      FormComponent(
        id = FormComponentId("testInfoField"),
        `type` = InformationMessage(StandardInfo, toSmartString(markdown)),
        label = toSmartString("This is the field label"),
        helpText = None,
        shortName = None,
        validIf = None,
        mandatory = true,
        editable = false,
        submissible = false,
        derived = true,
        errorMessage = None
      )
    }

    override def `section - about you` = super.`section - about you`.copy(fields = List(`fieldValue - firstName`))

    override def allSections = List(
      `section - about you`
    )

    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber(0),
        FormDataRecalculated.empty,
        formTemplate,
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        allSections,
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val infoFieldDiv = doc.getElementsByClass("subsection").first

    infoFieldDiv.getElementsByTag("H1").last.text shouldBe "This is an H1"
    infoFieldDiv.getElementsByTag("H2").first.text shouldBe "This is an H2"
    infoFieldDiv.getElementsByTag("H3").first.text shouldBe "This is an H3"
    infoFieldDiv.getElementsByTag("A").size() shouldBe 1
    infoFieldDiv.getElementsByTag("IMG").size() shouldBe 1
    infoFieldDiv.getElementsByTag("CODE").size() shouldBe 1
    infoFieldDiv.getElementsByTag("PRE").size() shouldBe 1
    infoFieldDiv.getElementsByTag("OL").size() shouldBe 1
    infoFieldDiv.getElementsByTag("UL").size() shouldBe 1

    val table = infoFieldDiv.getElementsByTag("TABLE")
    table.size() shouldBe 1
    table.first.getElementsByTag("TR").size shouldBe 4
    table.first.getElementsByTag("TD").size shouldBe 9
  }

  it should "return HTML with dynamic groups and an add-group button (repeating groups)" in new ExampleData {

    override def `group - type` = Group(
      fields = List(`fieldValue - firstName`),
      orientation = Horizontal,
      repeatsMax = Some(3),
      repeatsMin = Some(1),
      repeatLabel = Some(toSmartString("REPEAT_LABEL")),
      repeatAddAnotherText = Some(toSmartString("repeatAddAnotherText"))
    )

    override def allSections = List(
      `section - group`
    )

    val generatedHtml = testService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber(0),
        FormDataRecalculated.empty,
        formTemplate,
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        allSections,
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val addButtonValue = "AddGroup-" + `fieldValue - group`.id.value
    val fieldName = `fieldValue - firstName`.id.value
    doc
      .getElementsByAttributeValue("href", addButtonValue)
      .size shouldBe 1 withClue "no limit reached, add button shown"
    doc.getElementsByAttributeValue("name", fieldName).size shouldBe 1 withClue "One repeat element"
    doc.getElementsContainingOwnText("REPEAT_LABEL").size shouldBe 1
  }

  it should "hide add-group button when limit has been reached (repeating groups)" in new ExampleData {

    val thisTestService = new SectionRenderingService(frontendAppConfig, lookupRegistry)

    override def `group - type` = Group(
      fields = List(`fieldValue - firstName`),
      orientation = Horizontal,
      repeatsMax = Some(2),
      repeatsMin = Some(1),
      repeatLabel = Some(toSmartString("REPEAT_LABEL")),
      repeatAddAnotherText = Some(toSmartString("repeatAddAnotherText"))
    )

    override def allSections = List(
      `section - group`
    )

    val generatedHtml = thisTestService
      .renderSection(
        Some(accessCode),
        form,
        SectionNumber(0),
        mkFormDataRecalculated(
          VariadicFormData.ones(
            FormComponentId("firstName")   -> "",
            FormComponentId("1_firstName") -> ""
          )),
        formTemplate,
        Nil,
        Envelope.empty,
        envelopeId,
        ValidationResult.empty.valid,
        allSections,
        0,
        Nil,
        retrievals,
        NotChecked
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val addButtonValue = "AddGroup-" + `fieldValue - group`.id.value
    val fieldName = `fieldValue - firstName`.id.value
    doc.getElementsByAttributeValue("href", addButtonValue).size shouldBe 0
    doc.getElementsByAttributeValue("name", fieldName).size shouldBe 1
    doc.getElementsByAttributeValue("name", "1_" + fieldName).size shouldBe 1
    doc.getElementsContainingOwnText(messages("linkText.removeRepeatedGroup")).size shouldBe 2
  }

  it should "generate declaration page" in {
    val generatedHtml = testService
      .renderDeclarationSection(
        Some(accessCode),
        form,
        formTemplate,
        retrievals,
        ValidationResult.empty.valid,
        FormDataRecalculated.empty,
        Nil
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
    val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml())

    hiddenFieldNames should be(List("csrfToken", "save"))
    visibleFields should be(List())
    buttons should be(List(messages("button.acceptAndSubmit")))
  }

  it should "generate declaration page with submit claim button" in {
    val generatedHtml = testService
      .renderDeclarationSection(
        Some(accessCode),
        form,
        formTemplate.copy(formCategory = HMRCClaimForm),
        retrievals,
        ValidationResult.empty.valid,
        FormDataRecalculated.empty,
        Nil
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
    val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml())

    hiddenFieldNames should be(List("csrfToken", "save"))
    visibleFields should be(List())
    buttons should be(List(messages("button.acceptAndSubmitForm")))
  }

  it should "generate declaration page with submit return button" in {
    val generatedHtml = testService
      .renderDeclarationSection(
        Some(accessCode),
        form,
        formTemplate.copy(formCategory = HMRCReturnForm),
        retrievals,
        ValidationResult.empty.valid,
        FormDataRecalculated.empty,
        Nil
      )

    val doc = Jsoup.parse(generatedHtml.body)

    val hiddenFieldNames = toList(doc.getElementsByAttributeValue("type", "hidden")).map(_.attr("name"))
    val visibleFields = toList(doc.getElementsByAttributeValue("type", "text")).map(_.attr("name"))
    val buttons = toList(doc.getElementsByTag("BUTTON")).map(_.childNode(0).outerHtml())

    hiddenFieldNames should be(List("csrfToken", "save"))
    visibleFields should be(List())
    buttons should be(List(messages("button.acceptAndSubmitForm")))
  }

  private def toList(elements: Elements) = JavaConverters.asScalaIteratorConverter(elements.iterator).asScala.toList

  private def mkFormDataRecalculated(data: VariadicFormData): FormDataRecalculated =
    FormDataRecalculated.empty.copy(recData = RecData.fromData(data))
}
