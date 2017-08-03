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

import org.scalatest.mockito.MockitoSugar.mock
import org.mockito.Mockito._
import org.mockito.Matchers._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.model.{ FormTemplate, FormTypeId, Version, _ }
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.helpers.Extractors.extractNames
import uk.gov.hmrc.gform.prepop.PrepopService
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.domain.{ Accounts, Authority, ConfidenceLevel, CredentialStrength }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future

class PageSpec extends Spec {

  val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  val section0 = Section("Your details", None, None, None, None, None, None, List(FieldValue(FieldId("iptRegNum"), Text(AnyText, Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, None, true, true, true, None)))
  val section1 = Section("About you", None, None, None, None, None, None, List(FieldValue(FieldId("firstName"), Text(AnyText, Constant(""), total = false), "First Name", None, None, true, true, true, None)))
  val section2 = Section("Business details", None, None, None, None, None, None, List(FieldValue(FieldId("nameOfBusiness"), Text(AnyText, Constant(""), total = false), "Name of business", None, None, true, true, true, None)))
  val formTemplate = FormTemplate(
    formTypeId = FormTypeId(""),
    formName = "IPT100",
    version = Version("1.2.3"),
    description = "abc",
    characterSet = "UTF-8",
    dmsSubmission = dmsSubmission,
    authConfig = AuthConfig(AuthConfigModule("TEST"), None, RegimeId("TEST")),
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section0, section1, section2)
  )

  val mockPrepopService = new PrepopService(null, null, null) {
    override def prepopData(expr: Expr, formTypeId: FormTypeId)(implicit authContext: AuthContext, hc: HeaderCarrier): Future[String] =
      Future.successful("")
  }
  val mockRepeatService = mock[RepeatingComponentService]
  when(mockRepeatService.getAllSections(any(), any())(any())).thenReturn(Future.successful(formTemplate.sections))
  val formId = FormId("formid-123")
  val authority = Authority("uri", Accounts(), None, None, CredentialStrength.None, ConfidenceLevel.L0, None, None, None, "String")
  implicit val authContext = AuthContext(authority)
  implicit val headerCarrier = HeaderCarrier()

  "Page" should "display first page" in {

    val page = Page(formId, SectionNumber.firstSection, formTemplate, mockRepeatService, Envelope(Nil), EnvelopeId("env-id"), mockPrepopService)

    val render = page.pageForRender(Map.empty, None, formTemplate.sections)

    render.futureValue.hiddenFieldsSnippets.size should be(2)

    val hiddenFieldNames = extractNames(render.futureValue.hiddenFieldsSnippets)
    hiddenFieldNames should be(List("firstName", "nameOfBusiness"))

    render.futureValue.snippets.size should be(1)

    val fieldNames = extractNames(render.futureValue.snippets)
    fieldNames should be(List("iptRegNum"))
  }

  it should "display second page" in {
    val sectionNumber = SectionNumber(1)
    val page = Page(formId, sectionNumber, formTemplate, mockRepeatService, Envelope(Nil), EnvelopeId("env-id"), mockPrepopService)

    val render = page.pageForRender(Map.empty, None, formTemplate.sections)

    render.futureValue.hiddenFieldsSnippets.size should be(2)

    val hiddenFieldNames = extractNames(render.futureValue.hiddenFieldsSnippets)
    hiddenFieldNames should be(List("iptRegNum", "nameOfBusiness"))

    render.futureValue.snippets.size should be(1)

    val fieldNames = extractNames(render.futureValue.snippets)
    fieldNames should be(List("firstName"))
  }

  it should "display third page" in {

    val sectionNumber = SectionNumber(2)
    val page = Page(formId, sectionNumber, formTemplate, mockRepeatService, Envelope(Nil), EnvelopeId("env-id"), mockPrepopService)

    val render = page.pageForRender(Map.empty, None, formTemplate.sections)

    render.futureValue.hiddenFieldsSnippets.size should be(2)

    val hiddenFieldNames = extractNames(render.futureValue.hiddenFieldsSnippets)
    hiddenFieldNames should be(List("iptRegNum", "firstName"))

    render.futureValue.snippets.size should be(1)

    val fieldNames = extractNames(render.futureValue.snippets)
    fieldNames should be(List("nameOfBusiness"))

  }
}
