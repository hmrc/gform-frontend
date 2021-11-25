/*
 * Copyright 2021 HM Revenue & Customs
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

import java.time.LocalDateTime

import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.wshttp.StubbedWSHttp
import uk.gov.hmrc.http._

class GformConnectorSpec extends Spec {

  behavior of "GformConnector.formTemplate - happy path"

  it should "return form template for given type and version" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(buildFormTemplateWithRedirects))
    connector
      .getFormTemplate(formTemplateId)
      .futureValue shouldBe buildFormTemplateWithRedirects
  }

  behavior of "GformConnector.formTemplate - unhappy scenario"

  it should "it fails when formTemplate doesn't exist" in new Fixture {
    val status = 404
    val responseJson = None
    val isUpstream404Response: Boolean = connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(e) if e.statusCode == 404 => true
      case _                                                                   => false
    }
    isUpstream404Response shouldBe true
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    val responseJson = None
    val isUpstream5xxResponse: Boolean = connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream5xxResponse(_) => true
      case _                                            => false
    }
    isUpstream5xxResponse shouldBe true
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    val responseJson = None
    val isUpstream400Response: Boolean = connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(e) if e.statusCode == 400 => true
      case _                                                                   => false
    }
    isUpstream400Response shouldBe true
  }

  it should "it fails when gform returns 4xx code" in new Fixture {
    val status = 401
    val responseJson = None
    val isUpstream4xxResponse: Boolean = connector
      .getFormTemplate(formTemplateId)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(_) => true
      case _                                            => false
    }

    isUpstream4xxResponse shouldBe true
  }

  behavior of "GformConnector.form - happy path"

  it should "return form for given id" in new Fixture {
    val status = 200
    val responseJson = Some(Json.toJson(buildForm))
    connector
      .getForm(formIdData)
      .futureValue shouldBe buildForm
  }

  behavior of "GformConnector.form - unhappy scenarios"

  it should "it fails when form doesn't exist" in new Fixture {
    val status = 404
    val responseJson = None
    val isUpstream404Response = connector
      .getForm(formIdData)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(e) if e.statusCode == 404 => true
      case _                                                                   => false
    }
    isUpstream404Response shouldBe true
  }

  it should "it fails when gform returns 5xx" in new Fixture {
    val status = 500
    val responseJson = None
    val isUpstream5xxResponse: Boolean = connector
      .getForm(formIdData)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream5xxResponse(_) => true
      case _                                            => false
    }
    isUpstream5xxResponse shouldBe true
  }

  it should "it fails when gform returns BadRequest" in new Fixture {
    val status = 400
    val responseJson = None
    val isUpstream400Response: Boolean = connector
      .getForm(formIdData)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(e) if e.statusCode == 400 => true
      case _                                                                   => false
    }
    isUpstream400Response shouldBe true
  }

  it should "it fails when gform returns 4xx code" in new Fixture {
    val status = 401
    val responseJson = None
    val isUpstream4xxResponse: Boolean = connector
      .getForm(formIdData)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(_) => true
      case _                                            => false
    }
    isUpstream4xxResponse shouldBe true
  }

  behavior of "GformConnector.createSubmission - successful scenario"

  it should "create submission and return the Submission entity" in new Fixture {
    val status = 200
    implicit val localDateTime = LocalDateTime.now()
    val responseJson = Some(Json.toJson(submission))
    connector
      .createSubmission(formIdData.toFormId, formTemplateId, envelopeId, "some-customer-id", 1)
      .futureValue shouldBe submission
  }

  it should "it fails when gform returns non OK response" in new Fixture {
    val status = 400
    val responseJson = None
    val isUpstream400Response = connector
      .createSubmission(formIdData.toFormId, formTemplateId, envelopeId, "some-customer-id", 1)
      .failed
      .futureValue match {
      case UpstreamErrorResponse.Upstream4xxResponse(_) => true
      case _                                            => false
    }
    isUpstream400Response shouldBe true
  }

  trait Fixture extends ExampleData {
    def status: Int
    def responseJson: Option[JsValue]
    lazy val r: HttpResponse = HttpResponse(
      status,
      responseJson.orNull,
      Map.empty[String, Seq[String]]
    )

    lazy val wSHttp = new StubbedWSHttp(r)
    lazy val connector = new GformConnector(wSHttp, "http://localhost")
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }

}
