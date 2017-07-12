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

package uk.gov.hmrc.gform.gformbackend

import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.components.{Constant, FieldId, FieldValue, Text}
import uk.gov.hmrc.gform.models.{DmsSubmission, Section, UserId}
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.play.http.{HeaderCarrier, HttpResponse}

import scala.collection.immutable.List

class GformConnectorSpec extends Spec {

  behavior of "GformConnector.formTemplate - happy path"

  it should "return form template for given type and version" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(formTemplate))
    connector
      .getFormTemplate(formTypeId, version)
      .futureValue shouldBe formTemplate
  }

  behavior of "GformConnector.formTemplate - unhappy scenario"

  it should "it fails when formTemplate doesn't exist" in new Fixture {
    val status = 404
    val responseJson = None
    connector
      .getFormTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.NotFoundException]
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    val responseJson = None
    connector
      .getFormTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.Upstream5xxResponse]
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    val responseJson = None
    connector
      .getFormTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.BadRequestException]
  }

  it should "it fails when gform returns other 4xx code" in new Fixture {
    val status = 401
    val responseJson = None
    connector
      .getFormTemplate(formTypeId, version)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.Upstream4xxResponse]
  }

  behavior of "GformConnector.form - happy path"

  it should "return form for given id" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(form))
    connector
      .getForm(formId)
      .futureValue shouldBe formData //TODO: it should return Form not FormData!
  }

  behavior of "GformConnector.form - unhappy scenarios"

  it should "it fails when form doesn't exist" in new Fixture {
    val status = 404
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.NotFoundException]
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.Upstream5xxResponse]
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.BadRequestException]
  }

  it should "it fails when gform returns other 4xx code" in new Fixture {
    val status = 401
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.play.http.Upstream4xxResponse]
  }

  behavior of "GformConnector.newForm - happy path"

  it should "return NewFormResponse" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(newFormResponse))
    connector
      .newForm(formTypeId, version, userId)
      .futureValue shouldBe newFormResponse
  }

  trait Fixture extends ExampleData {
    def status: Int
    def responseJson: Option[JsValue]
    lazy val r = HttpResponse(
      responseStatus = status,
      responseJson = responseJson
    )
    lazy val wSHttp = new StubbedWSHttp(r)
    lazy val connector = new GformConnector(wSHttp, "baseUrl")
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }

}

trait ExampleData {

  lazy val dmsSubmission = DmsSubmission("nino", "some-classification-type", "some-business-area")
  lazy val section0 = Section("Your details", None, None, List(FieldValue(FieldId("iptRegNum"), Text(Constant(""), total = false), "Insurance Premium Tax (IPT) number", None, None, true, true, true)))
  lazy val section1 = Section("About you", None, None, List(FieldValue(FieldId("firstName"), Text(Constant(""), total = false), "First Name", None, None, true, true, true)))
  lazy val section2 = Section("Business details", None, None, List(FieldValue(FieldId("nameOfBusiness"), Text(Constant(""), total = false), "Name of business", None, None, true, true, true)))

  lazy val formTypeId = FormTypeId("IPT100")
  lazy val version = Version("0.3.0")

  private val characterSet = "UTF-8"
  lazy val formTemplate = FormTemplate(
    formTypeId = formTypeId,
    formName = "IPT100",
    version = version,
    description = "abc",
    characterSet = characterSet,
    dmsSubmission = dmsSubmission,
    submitSuccessUrl = "success-url",
    submitErrorUrl = "error-url",
    sections = List(section0, section1, section2)
  )

  lazy val formId = FormId("4fdf4eb6-c41b-4cd8-b95d-8221b670d449")
  lazy val field0 = FormField(
    FieldId("facePhoto"),
    "face-photo.jpg"
  )
  lazy val field1 = FormField(
    FieldId("name"),
    "Michael"
  )
  lazy val field2 = FormField(
    FieldId("surname"),
    "Jordan"
  )

  lazy val userId = UserId("TESTID")
  lazy val fields = Seq(field0, field1, field2)
  lazy val formData = FormData(userId,
    formTypeId,
    version,
    characterSet,
    fields
  )
  lazy val form = Form(
    formId,
    formData,
    envelopeId
  )

  lazy val envelopeId = EnvelopeId("b66c5979-e885-49cd-9281-c7f42ce6b307")
  lazy val newFormResponse = NewFormResponse(
    form,
    envelopeId,
    formTemplate
  )
}
