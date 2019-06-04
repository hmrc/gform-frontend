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

package uk.gov.hmrc.gform.gformbackend

import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.SpecWithFakeApp
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DeclarationSection
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.http._

class GformConnectorSpec extends SpecWithFakeApp {

  behavior of "GformConnector.formTemplate - happy path"

  it should "return form template for given type and version" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(formTemplate))
    connector
      .getFormTemplate(formTemplateId)
      .futureValue shouldBe formTemplate
  }

  behavior of "GformConnector.formTemplate - unhappy scenario"

  it should "it fails when formTemplate doesn't exist" in new Fixture {
    val status = 404
    val responseJson = None
    connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue shouldBe an[uk.gov.hmrc.http.NotFoundException]
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    val responseJson = None
    connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue shouldBe an[Upstream5xxResponse]
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    val responseJson = None
    connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue shouldBe an[BadRequestException]
  }

  it should "it fails when gform returns other 4xx code" in new Fixture {
    val status = 401
    val responseJson = None
    connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue shouldBe an[Upstream4xxResponse]
  }

  behavior of "GformConnector.form - happy path"

  it should "return form for given id" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(form))
    connector
      .getForm(formId)
      .futureValue shouldBe form
  }

  behavior of "GformConnector.form - unhappy scenarios"

  it should "it fails when form doesn't exist" in new Fixture {
    val status = 404
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[NotFoundException]
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[Upstream5xxResponse]
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[BadRequestException]
  }

  it should "it fails when gform returns other 4xx code" in new Fixture {
    val status = 401
    val responseJson = None
    connector
      .getForm(formId)
      .failed
      .futureValue shouldBe an[Upstream4xxResponse]
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
