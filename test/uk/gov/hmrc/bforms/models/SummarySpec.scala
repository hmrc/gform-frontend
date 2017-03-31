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

package uk.gov.hmrc.bforms.models

import org.scalatest._

class SummarySpec extends FlatSpec with Matchers with EitherValues {

  val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  val section0 = Section("Your details", List(FieldValue(FieldId("iptRegNum"), Text, "Insurance Premium Tax (IPT) number", None, None, None, None, true)))
  val section1 = Section("About you", List(FieldValue(FieldId("firstName"), Text, "First Name", None, None, None, None, true)))
  val section2 = Section("Business details", List(FieldValue(FieldId("nameOfBusiness"), Text, "Name of business", None, None, None, None, true)))
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

  "Summary" should "display the summary" in {

    val summary = Summary(formTemplate)

    val render = summary.summaryForRender(Map.empty, FormId(""))

    render.snippets.size should be(9)

//    val fieldNames = extractNames(render.snippets)
//    fieldNames should be(List("iptRegNum"))
  }

}
