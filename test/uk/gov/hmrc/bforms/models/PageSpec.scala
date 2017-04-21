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
import org.scalatest.concurrent.ScalaFutures
import uk.gov.hmrc.bforms.models.components.{Constant, FieldId, FieldValue, Text}
import uk.gov.hmrc.bforms.models.form.FormTypeId

import uk.gov.hmrc.bforms.models.helpers.Extractors.extractNames
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.domain.{ Accounts, Authority, ConfidenceLevel, CredentialStrength }
import uk.gov.hmrc.play.http.HeaderCarrier

class PageSpec extends FlatSpec with Matchers with EitherValues with ScalaFutures {

  val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  val section0 = Section("Your details", List(FieldValue(FieldId("iptRegNum"), Text(Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, None, true)))
  val section1 = Section("About you", List(FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "First Name", None, None, true)))
  val section2 = Section("Business details", List(FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, None, true)))
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

  val authority = Authority("uri", Accounts(), None, None, CredentialStrength.None, ConfidenceLevel.L0, None, None, None, "String")
  implicit val authContext = AuthContext(authority)
  implicit val headerCarrier = HeaderCarrier()

  "Page" should "display first page" in {

    val page = Page(0, formTemplate)

    page.prev should be(0)
    page.curr should be(0)
    page.next should be(1)

    page.section should be(section0)

    val render = page.pageForRender(Map.empty, None)

    render.futureValue.hiddenFieldsSnippets.size should be(2)

    val hiddenFieldNames = extractNames(render.futureValue.hiddenFieldsSnippets)
    hiddenFieldNames should be(List("firstName", "nameOfBusiness"))

    render.futureValue.snippets.size should be(1)

    val fieldNames = extractNames(render.futureValue.snippets)
    fieldNames should be(List("iptRegNum"))
  }

  it should "display second page" in {

    val page = Page(1, formTemplate)

    page.prev should be(0)
    page.curr should be(1)
    page.next should be(2)

    page.section should be(section1)

    val render = page.pageForRender(Map.empty, None)

    render.futureValue.hiddenFieldsSnippets.size should be(2)

    val hiddenFieldNames = extractNames(render.futureValue.hiddenFieldsSnippets)
    hiddenFieldNames should be(List("iptRegNum", "nameOfBusiness"))

    render.futureValue.snippets.size should be(1)

    val fieldNames = extractNames(render.futureValue.snippets)
    fieldNames should be(List("firstName"))
  }

  it should "display third page" in {

    val page = Page(2, formTemplate)

    page.prev should be(1)
    page.curr should be(2)
    page.next should be(2)

    page.section should be(section2)

    val render = page.pageForRender(Map.empty, None)

    render.futureValue.hiddenFieldsSnippets.size should be(2)

    val hiddenFieldNames = extractNames(render.futureValue.hiddenFieldsSnippets)
    hiddenFieldNames should be(List("iptRegNum", "firstName"))

    render.futureValue.snippets.size should be(1)

    val fieldNames = extractNames(render.futureValue.snippets)
    fieldNames should be(List("nameOfBusiness"))

  }

  it should "display first page when currentPage is less than 0" in {

    val page = Page(-1, formTemplate)

    page.prev should be(0)
    page.curr should be(0)
    page.next should be(1)

    page.section should be(section0)
  }

  it should "display last page when currentPage is bigger than size of sections" in {

    val page = Page(10, formTemplate)

    page.prev should be(1)
    page.curr should be(2)
    page.next should be(2)

    page.section should be(section2)
  }
}
