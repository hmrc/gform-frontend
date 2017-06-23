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
import org.scalatest._
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.form.{FormId, FormTypeId}
import uk.gov.hmrc.gform.models.helpers.Extractors._

import scala.collection.immutable.List


class SummarySpec extends FlatSpec with Matchers with EitherValues {

  val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  val section0 = Section("Your details", None, None, List(FieldValue(FieldId("iptRegNum"), Text(Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, None, true, true, true)))
  val section1 = Section("About you", None, None, List(FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "First Name", None, None, true, true, true)))
  val section2 = Section("Business details", None, None, List(FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, None, true, true, true)))
  val formTemplate = FormTemplate(
    formTypeId = FormTypeId(""),
    formName = "IPT100",
    version = "1.2.3",
    description = "abc",
    characterSet = "UTF-8",
    dmsSubmission = dmsSubmission,
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section0, section1, section2)
  )

  "Summary" should "display the summary sections" in {

    val summary = Summary(formTemplate)

    val formData = Map(FieldId("iptRegNum") -> Seq("Test!Your details!Test"),
      FieldId("firstName") -> Seq("Test!About you!Test"),
      FieldId("nameOfBusiness") -> Seq("Test!Business details!Test")
    )
    val render = summary.summaryForRender(formData, FormId(""))

    render.snippets.size should be(9)

    val testStringValues = extractAllTestStringValues(render.snippets)
    testStringValues should be(List("Your details", "About you", "Business details"))
  }

  "Summary" should "display links to page sections" in {

    val summary = Summary(
      formTemplate.copy(
        formTypeId = FormTypeId("Test!Form Type!Test"),
        sections = List(section0, section1)
      )
    )

    val render = summary.summaryForRender(Map(), FormId("Test!Form Id!Test"))
//    render should be(List())

    val testStringValues = extractAllTestStringValues(render.snippets)
    testStringValues should be(List("Form Type", "Form Id", "Form Type", "Form Id"))
  }

  "Summary" should "display values for each field type with a submissible field, " in {

    val section = Section("Personal details", None, None, List(
      FieldValue(FieldId("Surname"), Text(Constant(""), total = false), "Surname", None, None, true, true, true),
      FieldValue(FieldId("Info"), Text(Constant(""), total = false), "Info", None, None, true, true, submissible = false),
      FieldValue(FieldId("BirthDate"), Date(AnyDate, Offset(0), None), "Birth date", None, None, true, true, true),
      FieldValue(FieldId("HomeAddress"), Address(international = false), "Home address", None, None, true, true, true)
    ))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val formFields = Map(FieldId("Surname") -> Seq("Test!Saxe-Coburg-Gotha!Test"),
      FieldId("Info") -> Seq("Test!Royal!Test"),
      FieldId("BirthDate.day") -> Seq("19"),
      FieldId("BirthDate.month") -> Seq("11"),
      FieldId("BirthDate.year") -> Seq("1841"),
      FieldId("HomeAddress-street1") -> Seq("Test!Street!Test"),
      FieldId("HomeAddress-street2") -> Seq("Test!Second Street!Test"),
      FieldId("HomeAddress-street3") -> Seq("Test!Third Street!Test"),
      FieldId("HomeAddress-street4") -> Seq("Test!Town!Test"),
      FieldId("HomeAddress-postcode") -> Seq("Test!PO32 6JX!Test"),
      FieldId("HomeAddress-country") -> Seq("Test!UK!Test")
    )

    val render = summary.summaryForRender(formFields, FormId(""))

    val testStringValues = extractAllTestStringValues(render.snippets)
    testStringValues should be(List("Saxe-Coburg-Gotha", "Street", "Second Street", "Third Street", "Town", "PO32 6JX", "UK"))
    extractDates(render.snippets) should be (List(("19", "November", "1841")))
  }

  it should "display the title when shortName is not present in the section" in {
    val summary = Summary(formTemplate)

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.head.toString())
    doc.getElementsByTag("H2").text().equalsIgnoreCase("your details") shouldBe true
  }

  it should "display the shortName as section title if present" in {
    val shortName = "THIS_IS_A_VERY_VERY_VERY_SHORT_NAME"
    val section = section0.copy(shortName = Some(shortName))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.head.toString())
    doc.getElementsByTag("H2").text().equalsIgnoreCase(shortName) shouldBe true
  }

  it should "display shortName instead of label for Address field" in {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Address(false),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )
    val section = section0.copy(fields = List(addressField), shortName = Some("Address section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label when shortName not provided for Address field" in {
    val label = "JUST_A_VERY_LONG_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Address(false),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )
    val section = section0.copy(fields = List(addressField), shortName = Some("Address section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Text field" in {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Text(Constant("DA"), total = false),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Text field" in {
    val label = "THIS_IS_A_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Text(Constant("DA"), total = false),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Choice field" in {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Choice(Radio, NonEmptyList.of("u"), Vertical, List(), None),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Choice field" in {
    val label = "THIS_IS_A_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Choice(Radio, NonEmptyList.of("u"), Vertical, List(), None),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "display shortName instead of label for Date field" in {
    val shortName = "JUST_A_VERY_SHORT_NAME"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = "label",
      shortName = Some(shortName),
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(shortName) shouldBe true
  }

  it should "display label if shortName not provided for Date field" in {
    val label = "THIS_IS_A_LABEL"
    val addressField = FieldValue(
      id = FieldId("anId"),
      `type` = Date(AnyDate, Offset(0), None),
      label = label,
      shortName = None,
      helpText = None,
      mandatory = true,
      editable = true,
      submissible = true
    )

    val section = section0.copy(fields = List(addressField), shortName = Some("A section"))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val render = summary.summaryForRender(Map.empty, FormId(""))

    val doc = Jsoup.parse(render.snippets.mkString)

    doc.getElementsByTag("TBODY").first().getElementsByTag("TD").first().text().equals(label) shouldBe true
  }

  it should "not render sections with includeIf expressions that evaluate to false" in {

    val summary = Summary(formTemplate.copy(
      sections = List(section1.copy(includeIf = Some(IncludeIf(Equals(FormCtx("firstName"),Constant("Pete"))))))
    ))

    val renderWithDataMatching = summary.summaryForRender(Map(FieldId("firstName") -> Seq("Pete")), FormId(""))
    renderWithDataMatching.snippets.size shouldBe 3
    val renderWithDataMismatch = summary.summaryForRender(Map(FieldId("firstName") -> Seq("*Not*Pete")), FormId(""))
    renderWithDataMismatch.snippets.size shouldBe 0

  }

}
