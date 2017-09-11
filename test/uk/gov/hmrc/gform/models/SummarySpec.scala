/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import org.jsoup.Jsoup
import play.api.libs.json.JsValue
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.models.helpers.Extractors._
import uk.gov.hmrc.gform.models.helpers.Fields.okValues
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.Summary
import uk.gov.hmrc.gform.validation.FormFieldValidationResult
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SummarySpec extends Spec {

  trait Test extends ExampleData {
    override def dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
    def section0 = Section("Your details", None, None, None, None, None, None, List(FieldValue(`fieldId - iptRegNum`, Text(AnyText, Constant("")), "Insurance Premium Tax (IPT) number", None, None, true, true, true, false, None)))
    def section1 = Section("About you", None, None, None, None, None, None, List(FieldValue(`fieldId - firstName`, Text(AnyText, Constant("")), "First Name", None, None, true, true, true, false, None)))
    def section2 = Section("Business details", None, None, None, None, None, None, List(FieldValue(`fieldId - businessName`, Text(AnyText, Constant("")), "Name of business", None, None, true, true, true, false, None)))

    override def `formField - iptRegNum` = super.`formField - iptRegNum`.copy(value = "Test!Your details!Test")
    override def `formField - firstName` = super.`formField - firstName`.copy(value = "Test!About you!Test")
    override def `formField - businessName` = super.`formField - businessName`.copy(value = "Test!Business details!Test")

    override def formTemplate = super.formTemplate.copy(sections = List(section0, section1, section2))

    val mockRepeatService = new RepeatingComponentService(null) {
      override def getAllSections(formTemplate: FormTemplate, data: Map[FieldId, Seq[String]])(implicit hc: HeaderCarrier): Future[List[Section]] = {
        Future.successful(formTemplate.sections)
      }

      override def getAllRepeatingGroups(implicit hc: HeaderCarrier): Future[CacheMap] =
        Future.successful(CacheMap("Empty", Map.empty[String, JsValue]))

      override def getAllFieldsInGroupForSummary(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier): List[FieldValue] = {
        List[FieldValue]()
      }

      override def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier): List[List[FieldValue]] = {
        List.empty[List[FieldValue]]
      }

      override def atomicFields(section: BaseSection)(implicit hc: HeaderCarrier): List[FieldValue] = {
        section.fields
      }
    }

    def summary = Summary(formTemplate)
    def filedValues = formTemplate.sections.flatMap(_.fields)
    def f: FieldValue => Option[FormFieldValidationResult] = okValues(rawDataFromBrowser, filedValues, envelope)

    implicit val hc = HeaderCarrier()

  }

  "Summary" should "display the summary sections" in new Test {
    val render = summary.summaryForRender(f, rawDataFromBrowser, formId, mockRepeatService, envelope, None)
    render.futureValue.snippets.size should be(9)
    extractAllTestStringValues(render.futureValue.snippets) should be(List("Your details", "About you", "Business details"))
  }

  it should "display links to page sections" in new Test {
    override val formTemplate = super.formTemplate.copy(sections = List(section0, section1))

    val render = summary.summaryForRender(f, Map(), formId, mockRepeatService, envelope, None)

    val testStringValues = extractAllHrefs(render.futureValue.snippets)
    val expectedResult = List(
      uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, summary.formTemplate._id, SectionNumber(0), 2, None).url,
      uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, summary.formTemplate._id, SectionNumber(1), 2, None).url
    )

    testStringValues should be(expectedResult)
  }

  it should "display values for each field type with a submissible field, " in new Test {

    val section = Section("Personal details", None, None, None, None, None, None, List(
      FieldValue(FieldId("Surname"), Text(AnyText, Constant("")), "Surname", None, None, true, true, true, false, None),
      FieldValue(FieldId("Info"), Text(AnyText, Constant("")), "Info", None, None, true, true, submissible = false, false, None, presentationHint = Some(List(InvisibleInSummary))),
      FieldValue(FieldId("BirthDate"), Date(AnyDate, Offset(0), None), "Birth date", None, None, true, true, true, false, None),
      FieldValue(FieldId("HomeAddress"), Address(international = false), "Home address", None, None, true, true, true, false, None)
    ))

    override def formTemplate = super.formTemplate.copy(sections = List(section))

    override val rawDataFromBrowser = Map(
      FieldId("Surname") -> Seq("Test!Saxe-Coburg-Gotha!Test"),
      FieldId("Info") -> Seq("Test!Royal!Test"),
      FieldId("BirthDate-day") -> Seq("19"),
      FieldId("BirthDate-month") -> Seq("11"),
      FieldId("BirthDate-year") -> Seq("1841"),
      FieldId("HomeAddress-street1") -> Seq("Test!Street!Test"),
      FieldId("HomeAddress-street2") -> Seq("Test!Second Street!Test"),
      FieldId("HomeAddress-street3") -> Seq("Test!Third Street!Test"),
      FieldId("HomeAddress-street4") -> Seq("Test!Town!Test"),
      FieldId("HomeAddress-postcode") -> Seq("Test!PO32 6JX!Test"),
      FieldId("HomeAddress-country") -> Seq("Test!UK!Test")
    )

    override def filedValues = formTemplate.sections.flatMap(_.fields)
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(rawDataFromBrowser, filedValues, envelope)
    val render = summary.summaryForRender(f, rawDataFromBrowser, formId, mockRepeatService, envelope, None)
    val testStringValues = extractAllTestStringValues(render.futureValue.snippets)
    testStringValues should be(List("Saxe-Coburg-Gotha", "Street", "Second Street", "Third Street", "Town", "PO32 6JX", "UK"))
    extractDates(render.futureValue.snippets) should be(List(("19", "November", "1841")))
  }

  it should "display the title when shortName is not present in the section" in new Test {

    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)

    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)

    val doc = Jsoup.parse(render.futureValue.snippets.head.toString())
    doc.getElementsByTag("H2").text().equalsIgnoreCase("your details") shouldBe true
  }

  it should "display the shortName as section title if present" in new Test {
    val shortName = "THIS_IS_A_VERY_VERY_VERY_SHORT_NAME"
    val section = section0.copy(shortName = Some(shortName))
    override val formTemplate = super.formTemplate.copy(sections = List(section))

    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)

    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)

    val doc = Jsoup.parse(render.futureValue.snippets.head.toString())
    doc.getElementsByTag("H2").text().equalsIgnoreCase(shortName) shouldBe true
  }

  it should "display shortName instead of label for Address field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Address(false),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("Address section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label when shortName not provided for Address field" in new Test {
    val label = "JUST_A_VERY_LONG_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Address(false),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )
    val section = section0.copy(fields = List(addressField), shortName = Some("Address section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Text field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Text(AnyText, Constant("DA")),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Text field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Text(AnyText, Constant("DA")),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Choice field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Choice(Radio, NonEmptyList.of("u"), Vertical, List(), None),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Choice field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Choice(Radio, NonEmptyList.of("u"), Vertical, List(), None),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Date field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Date field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, filedValues, envelope)
    val render = summary.summaryForRender(f, Map.empty, formId, mockRepeatService, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "not render sections with includeIf expressions that evaluate to false" in new Test {

    override val section1 = super.section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"), Constant("Pete")))))
    override val formTemplate = super.formTemplate.copy(
      sections = List(section1)
    )

    val f1: FieldValue => Option[FormFieldValidationResult] = okValues(Map(FieldId("firstName") -> Seq("Pete")), filedValues, envelope)

    val renderWithDataMatching = summary.summaryForRender(f1, Map(FieldId("firstName") -> Seq("Pete")), formId, mockRepeatService, envelope, None)
    renderWithDataMatching.futureValue.snippets.size shouldBe 3

    val f2: FieldValue => Option[FormFieldValidationResult] = okValues(Map(FieldId("firstName") -> Seq("*Not*Pete")), filedValues, envelope)

    val renderWithDataMismatch = summary.summaryForRender(f2, Map(FieldId("firstName") -> Seq("*Not*Pete")), formId, mockRepeatService, envelope, None)
    renderWithDataMismatch.futureValue.snippets.size shouldBe 0
  }

  it should "display Group Labels (or Group Short Names if specified)" in new Test {

    val groupFieldValue = FieldValue(
      FieldId("gid"),
      Group(
        List(),
        Horizontal
      ),
      "Test!group-label!Test", None, None, true, true, true, true, None
    )
    override def section0 = Section("", None, None, None, None, None, None, List(groupFieldValue))
    override def formTemplate = super.formTemplate.copy(sections = List(section0))
    override def f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty[FieldId, Seq[String]], filedValues, envelope)
    val render0 = Summary(formTemplate).summaryForRender(f, Map.empty[FieldId, Seq[String]], formId, mockRepeatService, envelope, None)
    extractAllTestStringValues(render0.futureValue.snippets) should be(List("group-label"))
    val formTemplateWGroupWithShortname = formTemplate.copy(
      sections = List(Section("", None, None, None, None, None, None, List(groupFieldValue.copy(shortName = Some("Test!group-shortname!Test")))))
    )

    val filedValues1 = formTemplate.sections.flatMap(_.fields)

    val f1: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty[FieldId, Seq[String]], filedValues1, envelope)

    val render1 = Summary(formTemplateWGroupWithShortname).summaryForRender(f1, Map.empty[FieldId, Seq[String]], formId, mockRepeatService, envelope, None)
    extractAllTestStringValues(render1.futureValue.snippets) should be(List("group-shortname"))
  }

  "The Change hrefs" should "link to the correct page" in new Test {

    override val formTemplate = super.formTemplate.copy(
      sections = List(section0, section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"), Constant("Pete"))))), section2)
    )
    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map(FieldId("firstName") -> Seq("*Not*Pete")), filedValues, envelope)

    val summaryForRender = summary.summaryForRender(f, Map(FieldId("firstName") -> Seq("*Not*Pete")), formId, mockRepeatService, envelope, None)
    val htmls = summaryForRender.futureValue.snippets
    val htmlAheadOfSection2 = htmls(3)
    val doc = Jsoup.parse(htmlAheadOfSection2.toString)
    val urlOfHrefToSection2 = doc.select("a:contains(Change").get(0).attributes().get("href")
    urlOfHrefToSection2 shouldBe uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, summary.formTemplate._id, SectionNumber(2), 3, None).url
  }
}

