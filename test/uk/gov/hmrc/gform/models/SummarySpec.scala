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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.data.Validated.Valid
import cats.syntax.validated._
import org.jsoup.Jsoup
import org.scalatest.mockito.MockitoSugar.mock
import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.gform.routes
import uk.gov.hmrc.gform.graph.RecData
import uk.gov.hmrc.gform.models.helpers.Extractors._
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString, NotChecked }
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DmsSubmission
import uk.gov.hmrc.gform.sharedmodel.graph.IncludeIfGN
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import scala.collection.immutable.List
import uk.gov.hmrc.http.HeaderCarrier

class SummarySpec(implicit messages: Messages, l: LangADT) extends Spec {

  trait Test extends ExampleData {
    override def dmsSubmission =
      DmsSubmission("DMS-ID-XX", TextExpression(AuthCtx(PayeNino)), "some-classification-type", "some-business-area")
    def section0 =
      Section(
        toLocalisedString("Your details"),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        List(
          FormComponent(
            `fieldId - iptRegNum`,
            Text(AnyText, Value),
            toLocalisedString("Insurance Premium Tax (IPT) number"),
            None,
            None,
            None,
            true,
            true,
            true,
            false,
            false,
            None)),
        None,
        None
      )
    def section1 =
      Section(
        toLocalisedString("About you"),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        List(
          FormComponent(
            `fieldId - firstName`,
            Text(AnyText, Value),
            toLocalisedString("First Name"),
            None,
            None,
            None,
            true,
            true,
            true,
            false,
            false,
            None)),
        None,
        None
      )
    def section2 =
      Section(
        toLocalisedString("Business details"),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
        List(
          FormComponent(
            `fieldId - businessName`,
            Text(AnyText, Value),
            toLocalisedString("Name of business"),
            None,
            None,
            None,
            true,
            true,
            true,
            false,
            false,
            None)),
        None,
        None
      )

    override def `formField - iptRegNum` = super.`formField - iptRegNum`.copy(value = "Test!Your details!Test")
    override def `formField - firstName` = super.`formField - firstName`.copy(value = "Test!About you!Test")
    override def `formField - businessName` =
      super.`formField - businessName`.copy(value = "Test!Business details!Test")

    override def formTemplate = super.formTemplate.copy(sections = List(section0, section1, section2))

    val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]

    def fieldValues = formTemplate.sections.flatMap(_.fields)
    def f: ValidatedType[ValidationResult] = ValidationResult.empty.valid
    implicit val hc = HeaderCarrier()

  }

  "Summary" should "display the summary sections" in new Test {
    val render = SummaryRenderingService
      .summaryForRender(f, formDataRecalculated, Some(accessCode), formTemplate, envelope, NotChecked)
    render.size should be(9)
    extractAllTestStringValues(render) should be(List("Your details", "About you", "Business details"))
  }

  it should "display links to page sections" in new Test {
    override def formTemplate = super.formTemplate.copy(sections = List(section0, section1))

    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)

    val testStringValues = extractAllHrefs(render)

    def callUrlEscaped(call: Call) = call.url.replaceAll("&", "&amp;")

    val expectedResult = List(
      callUrlEscaped(
        routes.FormController
          .form(formTemplate._id, Some(accessCode), SectionNumber(0), SectionTitle4Ga("Your-details"), SeYes)),
      callUrlEscaped(
        routes.FormController
          .form(formTemplate._id, Some(accessCode), SectionNumber(1), SectionTitle4Ga("About-you"), SeYes))
    )

    testStringValues(0) should startWith(expectedResult(0))
    testStringValues(1) should startWith(expectedResult(1))
  }

  it should "display values for each field type with a submissible field, " in new Test {

    val section = Section(
      toLocalisedString("Personal details"),
      None,
      None,
      None,
      None,
      None,
      None,
      None,
      List(
        FormComponent(
          FormComponentId("Surname"),
          Text(AnyText, Value),
          toLocalisedString("Surname"),
          None,
          None,
          None,
          true,
          true,
          true,
          false,
          false,
          None),
        FormComponent(
          FormComponentId("Info"),
          Text(AnyText, Value),
          toLocalisedString("Info"),
          None,
          None,
          None,
          true,
          true,
          submissible = false,
          false,
          false,
          None,
          presentationHint = Some(List(InvisibleInSummary))
        ),
        FormComponent(
          FormComponentId("BirthDate"),
          Date(AnyDate, Offset(0), None),
          toLocalisedString("Birth date"),
          None,
          None,
          None,
          true,
          true,
          true,
          false,
          false,
          None),
        FormComponent(
          FormComponentId("HomeAddress"),
          Address(international = false),
          toLocalisedString("Home address"),
          None,
          None,
          None,
          true,
          true,
          true,
          false,
          false,
          None)
      ),
      None,
      None
    )

    override def formTemplate = super.formTemplate.copy(sections = List(section))

    override val formDataRecalculated = FormDataRecalculated.empty.copy(
      recData = RecData.fromData(Map(
        FormComponentId("Surname")              -> Seq("Test!Saxe-Coburg-Gotha!Test"),
        FormComponentId("Info")                 -> Seq("Test!Royal!Test"),
        FormComponentId("BirthDate-day")        -> Seq("19"),
        FormComponentId("BirthDate-month")      -> Seq("11"),
        FormComponentId("BirthDate-year")       -> Seq("1841"),
        FormComponentId("HomeAddress-street1")  -> Seq("Test!Street!Test"),
        FormComponentId("HomeAddress-street2")  -> Seq("Test!Second Street!Test"),
        FormComponentId("HomeAddress-street3")  -> Seq("Test!Third Street!Test"),
        FormComponentId("HomeAddress-street4")  -> Seq("Test!Town!Test"),
        FormComponentId("HomeAddress-postcode") -> Seq("Test!PO32 6JX!Test"),
        FormComponentId("HomeAddress-country")  -> Seq("Test!UK!Test")
      )))

    override def fieldValues = formTemplate.sections.flatMap(_.fields)
    val render = SummaryRenderingService
      .summaryForRender(f, formDataRecalculated, Some(accessCode), formTemplate, envelope, NotChecked)
    val testStringValues = extractAllTestStringValues(render)
    testStringValues should be(
      List("Saxe-Coburg-Gotha", "Street", "Second Street", "Third Street", "Town", "PO32 6JX", "UK"))
    extractDates(render) should be(List(("19", "November", "1841")))
  }

  it should "display the title when shortName is not present in the section" in new Test {
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)

    val doc = Jsoup.parse(render.head.toString())
    doc.getElementsByTag("H2").text().toLowerCase should include("your details")
  }

  it should "display the shortName as section title if present" in new Test {
    val shortName = "THIS_IS_A_VERY_VERY_VERY_SHORT_NAME"
    val section = section0.copy(shortName = Some(toLocalisedString(shortName)))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)

    val doc = Jsoup.parse(render.head.toString())
    doc.getElementsByTag("H2").text().toUpperCase should include(shortName)
  }

  it should "display shortName instead of label for Address field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Address(false),
      label = toLocalisedString("label"),
      shortName = Some(toLocalisedString(shortName)),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("Address section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").text().equals(shortName) shouldBe true
  }

  it should "display label when shortName not provided for Address field" in new Test {
    val label = "JUST_A_VERY_LONG_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Address(false),
      label = toLocalisedString(label),
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )
    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("Address section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Text field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Text(AnyText, Constant("DA")),
      label = toLocalisedString("label"),
      shortName = Some(toLocalisedString(shortName)),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("A section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Text field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Text(AnyText, Constant("DA")),
      label = toLocalisedString(label),
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("A section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Choice field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Choice(Radio, NonEmptyList.of(toLocalisedString("u")), Vertical, List(), None),
      label = toLocalisedString("label"),
      shortName = Some(toLocalisedString(shortName)),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("A section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Choice field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Choice(Radio, NonEmptyList.of(toLocalisedString("u")), Vertical, List(), None),
      label = toLocalisedString(label),
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("A section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    //    override val f: FieldValue => Option[FormFieldValidationResult] = okValues(Map.empty, fieldValues, envelope)
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Date field" in new Test {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = toLocalisedString("label"),
      shortName = Some(toLocalisedString(shortName)),
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("A section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Date field" in new Test {
    val label = "THIS_IS_A_LABEL"
    val addressField = FormComponent(
      id = FormComponentId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = toLocalisedString(label),
      shortName = None,
      helpText = None,
      validIf = None,
      mandatory = true,
      editable = true,
      submissible = true,
      derived = true,
      errorMessage = None
    )

    val section = section0.copy(fields = List(addressField), shortName = Some(toLocalisedString("A section")))
    override val formTemplate = super.formTemplate.copy(sections = List(section))
    val render =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    val doc = Jsoup.parse(render.mkString)
    doc.getElementsByTag("DT").first().text().equals(label) shouldBe true
  }

  it should "not render sections with includeIf expressions that evaluate to false" in new Test {

    val includeIf = IncludeIf(Equals(FormCtx("firstName"), Constant("Pete")))
    override val section2 = super.section2.copy(includeIf = Some(includeIf))
    override val formTemplate = super.formTemplate.copy(
      sections = List(section2)
    )
    val renderWithDataMatching = SummaryRenderingService.summaryForRender(
      f,
      FormDataRecalculated.empty.copy(recData = RecData.fromData(Map(FormComponentId("firstName") -> Seq("Pete")))),
      Some(accessCode),
      formTemplate,
      envelope,
      NotChecked
    )
    renderWithDataMatching.size shouldBe 3
    val renderWithDataMismatch = SummaryRenderingService.summaryForRender(
      f,
      FormDataRecalculated(
        Set(IncludeIfGN(FormComponentId("includeId_X"), includeIf)),
        RecData.fromData(Map(FormComponentId("firstName") -> Seq("*Not*Pete")))),
      Some(accessCode),
      formTemplate,
      envelope,
      NotChecked
    )
    renderWithDataMismatch.size shouldBe 0
  }

  it should "display Group Labels (or Group Short Names if specified)" in new Test {

    val groupFieldValue = FormComponent(
      FormComponentId("gid"),
      Group(
        List(),
        Horizontal
      ),
      toLocalisedString("Test!group-label!Test"),
      None,
      None,
      None,
      true,
      true,
      true,
      true,
      false,
      None
    )
    override def section0 =
      Section(toLocalisedString(""), None, None, None, None, None, None, None, List(groupFieldValue), None, None)
    override def formTemplate = super.formTemplate.copy(sections = List(section0))
    val render0 =
      SummaryRenderingService
        .summaryForRender(f, FormDataRecalculated.empty, Some(accessCode), formTemplate, envelope, NotChecked)
    extractAllTestStringValues(render0) should be(List("group-label"))
    val formTemplateWGroupWithShortname = formTemplate.copy(
      sections = List(
        Section(
          toLocalisedString(""),
          None,
          None,
          None,
          None,
          None,
          None,
          None,
          List(groupFieldValue.copy(shortName = Some(toLocalisedString("Test!group-shortname!Test")))),
          None,
          None
        ))
    )

    val filedValues1 = formTemplate.sections.flatMap(_.fields)
    val render1 = SummaryRenderingService
      .summaryForRender(
        f,
        FormDataRecalculated.empty,
        Some(accessCode),
        formTemplateWGroupWithShortname,
        envelope,
        NotChecked)
    extractAllTestStringValues(render1) should be(List("group-shortname"))
  }

  "The Change hrefs" should "link to the correct page" in new Test {

    val includeIf = IncludeIf(Equals(FormCtx("firstName"), Constant("Pete")))

    override val formTemplate = super.formTemplate.copy(
      sections = List(section0, section1.copy(includeIf = Some(includeIf)), section2)
    )

    val summaryForRender = SummaryRenderingService.summaryForRender(
      f,
      FormDataRecalculated(
        Set(IncludeIfGN(FormComponentId("includeId_X"), includeIf)),
        RecData.fromData(Map(FormComponentId("firstName") -> Seq("*Not*Pete")))),
      Some(accessCode),
      formTemplate,
      envelope,
      NotChecked
    )
    val htmls = summaryForRender

    {
      val htmlAheadOfSection0 = htmls(1)
      val doc = Jsoup.parse(htmlAheadOfSection0.toString)
      val urlOfHrefToSection0 = doc.select("a:contains(Change)").get(0).attributes().get("href")
      val targetUrl = uk.gov.hmrc.gform.gform.routes.FormController
        .form(formTemplate._id, Some(accessCode), SectionNumber(0), SectionTitle4Ga("Your-details"), SeYes)
        .url
      urlOfHrefToSection0 shouldBe targetUrl
    }
    {
      val htmlAheadOfSection2 = htmls(4)
      val doc = Jsoup.parse(htmlAheadOfSection2.toString)
      val urlOfHrefToSection2 = doc.select("a:contains(Change)").get(0).attributes().get("href")
      val targetUrl = uk.gov.hmrc.gform.gform.routes.FormController
        .form(formTemplate._id, Some(accessCode), SectionNumber(2), SectionTitle4Ga("Business-details"), SeYes)
        .url
      urlOfHrefToSection2 shouldBe targetUrl
    }
  }
}
