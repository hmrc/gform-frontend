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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import org.jsoup.Jsoup
import org.scalatest.mockito.MockitoSugar.mock
import play.api.libs.json.JsValue
import play.twirl.api.Html
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.gform.routes
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.helpers.Extractors._
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.cache.client.CacheMap

import scala.collection.immutable.List
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.http.HeaderCarrier

class SummarySpec extends Spec {

  trait Test extends ExampleData {
    override def dmsSubmission = DmsSubmission("DMS-ID-XX", TextExpression(AuthCtx(PayeNino)), "some-classification-type", "some-business-area")
    def section0 = Section("Your details", None, None, None, None, None, None, None, List(FormComponent(`fieldId - iptRegNum`, Text(AnyText, Constant("")), "Insurance Premium Tax (IPT) number", None, None, None, true, true, true, false, false, None)))
    def section1 = Section("About you", None, None, None, None, None, None, None, List(FormComponent(`fieldId - firstName`, Text(AnyText, Constant("")), "First Name", None, None, None, true, true, true, false, false, None)))
    def section2 = Section("Business details", None, None, None, None, None, None, None, List(FormComponent(`fieldId - businessName`, Text(AnyText, Constant("")), "Name of business", None, None, None, true, true, true, false, false, None)))

    override def `formField - iptRegNum` = super.`formField - iptRegNum`.copy(value = "Test!Your details!Test")
    override def `formField - firstName` = super.`formField - firstName`.copy(value = "Test!About you!Test")
    override def `formField - businessName` = super.`formField - businessName`.copy(value = "Test!Business details!Test")

    override def formTemplate = super.formTemplate.copy(sections = List(section0, section1, section2))

    val mockRepeatService = new RepeatingComponentService(null, null) {
      override def getCache(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Option[CacheMap]] = Future.successful(None)

      override def getAllSections(formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]], cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[List[Section]] = {
        Future.successful(formTemplate.sections)
      }

      override def getAllRepeatingGroups(cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[CacheMap] =
        Future.successful(CacheMap("Empty", Map.empty[String, JsValue]))

      override def getAllFieldsInGroupForSummary(topFieldValue: FormComponent, groupField: Group, cache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ex: ExecutionContext): Future[List[List[FormComponent]]] = {
        Future.successful(List[List[FormComponent]]())
      }

      override def getAllFieldsInGroup(topFieldValue: FormComponent, groupField: Group, repeatCache: Future[Option[CacheMap]])(implicit hc: HeaderCarrier, ec: ExecutionContext): List[List[FormComponent]] = {
        List.empty[List[FormComponent]]
      }

      override def atomicFields(repeatCache: Future[Option[CacheMap]])(section: BaseSection)(implicit hc: HeaderCarrier, ec: ExecutionContext): List[FormComponent] = {
        section.fields
      }
    }

    val retrievals: Retrievals = mock[Retrievals]

    def fieldValues = formTemplate.sections.flatMap(_.fields)
    def f: ValidatedType = Valid(()) //valuesValidate(rawDataFromBrowser, fieldValues, envelope, Map.empty[FieldId, Set[String]])
    implicit val hc = HeaderCarrier()

  }

  "Summary" should "display the summary sections" in new Test {
    val render = SummaryRenderingService.summaryForRender(f, rawDataFromBrowser, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    render.futureValue.snippets.size should be(9)
    extractAllTestStringValues(render.futureValue.snippets) should be(List("Your details", "About you", "Business details"))
  }

  it should "display links to page sections" in new Test {
    override def formTemplate = super.formTemplate.copy(sections = List(section0, section1))

    val render = SummaryRenderingService.summaryForRender(f, Map(), retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)

    val testStringValues = extractAllHrefs(render.futureValue.snippets)
    val expectedResult = List(
      routes.FormController.form(formId, formTemplate._id, SectionNumber(0), 2, None).url,
      routes.FormController.form(formId, formTemplate._id, SectionNumber(1), 2, None).url
    )

    testStringValues should be(expectedResult)
  }

  it should "display values for each field type with a submissible field, " in new Test {

    val section = Section("Personal details", None, None, None, None, None, None, None, List(
      FormComponent(FormComponentId("Surname"), Text(AnyText, Constant("")), "Surname", None, None, None, true, true, true, false, false, None),
      FormComponent(FormComponentId("Info"), Text(AnyText, Constant("")), "Info", None, None, None, true, true, submissible = false, false, false, None, presentationHint = Some(List(InvisibleInSummary))),
      FormComponent(FormComponentId("BirthDate"), Date(AnyDate, Offset(0), None), "Birth date", None, None, None, true, true, true, false, false, None),
      FormComponent(FormComponentId("HomeAddress"), Address(international = false), "Home address", None, None, None, true, true, true, false, false, None)
    ))

    override def formTemplate = super.formTemplate.copy(sections = List(section))

    override val rawDataFromBrowser = Map(
      FormComponentId("Surname") -> Seq("Test!Saxe-Coburg-Gotha!Test"),
      FormComponentId("Info") -> Seq("Test!Royal!Test"),
      FormComponentId("BirthDate-day") -> Seq("19"),
      FormComponentId("BirthDate-month") -> Seq("11"),
      FormComponentId("BirthDate-year") -> Seq("1841"),
      FormComponentId("HomeAddress-street1") -> Seq("Test!Street!Test"),
      FormComponentId("HomeAddress-street2") -> Seq("Test!Second Street!Test"),
      FormComponentId("HomeAddress-street3") -> Seq("Test!Third Street!Test"),
      FormComponentId("HomeAddress-street4") -> Seq("Test!Town!Test"),
      FormComponentId("HomeAddress-postcode") -> Seq("Test!PO32 6JX!Test"),
      FormComponentId("HomeAddress-country") -> Seq("Test!UK!Test")
    )

    override def fieldValues = formTemplate.sections.flatMap(_.fields)
    val render = SummaryRenderingService.summaryForRender(f, rawDataFromBrowser, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val testStringValues = extractAllTestStringValues(render.futureValue.snippets)
    testStringValues should be(List("Saxe-Coburg-Gotha", "Street", "Second Street", "Third Street", "Town", "PO32 6JX", "UK"))
    extractDates(render.futureValue.snippets) should be(List(("19", "November", "1841")))
  }

  it should "display the title when shortName is not present in the section" in new Test {
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)

    val doc = Jsoup.parse(render.futureValue.snippets.head.toString())
    println(doc.getElementsByTag("SPAN").text())
    doc.getElementsByTag("SPAN").text().toLowerCase should include("your details")
  }

  it should "display the shortName as section title if present" in new Test {
    val shortName = "THIS_IS_A_VERY_VERY_VERY_SHORT_NAME"
    val section = section0.copy(shortName = Some(shortName))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)

    val doc = Jsoup.parse(render.futureValue.snippets.head.toString())
    doc.getElementsByTag("SPAN").text().toUpperCase should include(shortName)
  }

  it should "display shortName instead of label for Address field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Address(false),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("Address section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label when shortName not provided for Address field" in new Test {
    val label = "JUST_A_VERY_LONG_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Address(false),
      label = label,
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )
    val section = section0.copy(fields = List(addressField), shortName = Some("Address section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Text field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Text(AnyText, Constant("DA")),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Text field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Text(AnyText, Constant("DA")),
      label = label,
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Choice field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Choice(Radio, NonEmptyList.of("u"), Vertical, List(), None),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Choice field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Choice(Radio, NonEmptyList.of("u"), Vertical, List(), None),
      label = label,
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    //    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, fieldValues, envelope)
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Date field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Date field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = label,
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render = SummaryRenderingService.summaryForRender(f, Map.empty, retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val doc = Jsoup.parse(render.futureValue.snippets.mkString)
    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "not render sections with includeIf expressions that evaluate to false" in new Test {

    override val section1 = super.section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"), Constant("Pete")))))
    override val formTemplate = super.formTemplate.copy(
      sections = List(section1)
    )
    val renderWithDataMatching = SummaryRenderingService.summaryForRender(f, Map(FormComponentId("firstName") -> Seq("Pete")), retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    renderWithDataMatching.futureValue.snippets.size shouldBe 3
    val renderWithDataMismatch = SummaryRenderingService.summaryForRender(f, Map(FormComponentId("firstName") -> Seq("*Not*Pete")), retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    renderWithDataMismatch.futureValue.snippets.size shouldBe 0
  }

  it should "display Group Labels (or Group Short Names if specified)" in new Test {

    val groupFieldValue = FormComponent(
      FormComponentId("gid"),
      Group(
        List(),
        Horizontal
      ),
      "Test!group-label!Test", None, None, None, true, true, true, true, false, None
    )
    override def section0 = Section("", None, None, None, None, None, None, None, List(groupFieldValue))
    override def formTemplate = super.formTemplate.copy(sections = List(section0))
    val render0 = SummaryRenderingService.summaryForRender(f, Map.empty[FormComponentId, Seq[String]], retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    extractAllTestStringValues(render0.futureValue.snippets) should be(List("group-label"))
    val formTemplateWGroupWithShortname = formTemplate.copy(
      sections = List(Section("", None, None, None, None, None, None, None, List(groupFieldValue.copy(shortName = Some("Test!group-shortname!Test")))))
    )

    val filedValues1 = formTemplate.sections.flatMap(_.fields)
    val render1 = SummaryRenderingService.summaryForRender(f, Map.empty[FormComponentId, Seq[String]], retrievals, formId, formTemplateWGroupWithShortname, mockRepeatService, mockRepeatService.getCache, envelope, None)
    extractAllTestStringValues(render1.futureValue.snippets) should be(List("group-shortname"))
  }

  "The Change hrefs" should "link to the correct page" in new Test {

    override val formTemplate = super.formTemplate.copy(
      sections = List(section0, section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"), Constant("Pete"))))), section2)
    )
    //    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map(FieldId("firstName") -> Seq("*Not*Pete")), fieldValues, envelope)

    val summaryForRender = SummaryRenderingService.summaryForRender(f, Map(FormComponentId("firstName") -> Seq("*Not*Pete")), retrievals, formId, formTemplate, mockRepeatService, mockRepeatService.getCache, envelope, None)
    val htmls = summaryForRender.futureValue.snippets
    val htmlAheadOfSection2 = htmls(3)
    val doc = Jsoup.parse(htmlAheadOfSection2.toString)
    val urlOfHrefToSection2 = doc.select("a:contains(Change)").get(0).attributes().get("href")
    urlOfHrefToSection2 shouldBe uk.gov.hmrc.gform.gform.routes.FormController.form(formId, formTemplate._id, SectionNumber(2), 3, None).url
  }
}

