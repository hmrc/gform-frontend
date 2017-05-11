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

import org.scalatest._
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.form.{FormId, FormTypeId}
import uk.gov.hmrc.gform.models.helpers.Extractors._

class SummarySpec extends FlatSpec with Matchers with EitherValues {

  val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  val section0 = Section("Your details", List(FieldValue(FieldId("iptRegNum"), Text(Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, true, true, true)))
  val section1 = Section("About you", List(FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "First Name", None, true, true, true)))
  val section2 = Section("Business details", List(FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, true, true, true)))
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

    val formFields = Map(FieldId("iptRegNum") -> Seq("Test!Your details!Test"),
      FieldId("firstName") -> Seq("Test!About you!Test"),
      FieldId("nameOfBusiness") -> Seq("Test!Business details!Test")
    )
    val render = summary.summaryForRender(formFields, FormId(""))

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

    val section = Section("Personal details", List(
      FieldValue(FieldId("Surname"), Text(Constant(""), total = false), "Surname", None, true, true, true),
      FieldValue(FieldId("Info"), Text(Constant(""), total = false), "Info", None, true, true, submissible = false),
      FieldValue(FieldId("BirthDate"), Date(AnyDate, Offset(0), None), "Birth date", None, true, true, true),
      FieldValue(FieldId("HomeAddress"), Address, "Home address", None, true, true, true)
    ))
    val summary = Summary(formTemplate.copy(sections = List(section)))

    val formFields = Map(FieldId("Surname") -> Seq("Test!Saxe-Coburg-Gotha!Test"),
      FieldId("Info") -> Seq("Test!Royal!Test"),
      FieldId("BirthDate.day") -> Seq("19"),
      FieldId("BirthDate.month") -> Seq("11"),
      FieldId("BirthDate.year") -> Seq("1841"),
      FieldId("HomeAddress.street1") -> Seq("Test!Street!Test"),
      FieldId("HomeAddress.street2") -> Seq("Test!Second Street!Test"),
      FieldId("HomeAddress.street3") -> Seq("Test!Third Street!Test"),
      FieldId("HomeAddress.town") -> Seq("Test!Town!Test"),
      FieldId("HomeAddress.county") -> Seq("Test!Countyshire!Test"),
      FieldId("HomeAddress.postcode") -> Seq("Test!PO32 6JX!Test")
    )


    val render = summary.summaryForRender(formFields, FormId(""))

    val testStringValues = extractAllTestStringValues(render.snippets)
    testStringValues should be(List("Saxe-Coburg-Gotha", "Street", "Second Street", "Third Street", "Town", "Countyshire", "PO32 6JX"))
    extractDates(render.snippets) should be (List(("19", "November", "1841")))
  }


}
