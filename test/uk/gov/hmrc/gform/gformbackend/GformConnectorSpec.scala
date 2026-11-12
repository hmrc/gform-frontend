/*
 * Copyright 2023 HM Revenue & Customs
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

import munit.FunSuite

import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{ RETURNS_SELF, mock, when }
import play.api.libs.json.{ JsValue, Json }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Span }
import uk.gov.hmrc.gform.sharedmodel.ExampleData
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.wshttp.StubbedHttpClientV2
import uk.gov.hmrc.http._
import uk.gov.hmrc.http.client.{ HttpClientV2, RequestBuilder }

import java.net.URL
import java.time.LocalDateTime
import scala.concurrent.{ ExecutionContext, Future }

class GformConnectorSpec extends FunSuite with Matchers with ScalaFutures {
  implicit private val ec: ExecutionContext = ExecutionContext.global
  override implicit val patienceConfig: PatienceConfig =
    PatienceConfig(timeout = scaled(Span(1000, Millis)), interval = scaled(Span(15, Millis)))

  test("GformConnector.formTemplate - happy path should return form template for given type and version") {
    new ConnectorFixture {
      val status = 200
      val responseJson = Some(Json.toJson(buildFormTemplateContext))
      connector
        .getFormTemplateContext(formTemplateId)
        .futureValue shouldBe buildFormTemplateContext
    }
  }

  test("GformConnector.formTemplate - unhappy scenario should it fails when formTemplate doesn't exist") {
    new ConnectorFixture {
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
  }

  test("GformConnector.formTemplate - unhappy scenario should it fails when gform returns 5xx") {
    new ConnectorFixture {
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
  }

  test("GformConnector.formTemplate - unhappy scenario should it fails when gform returns BadRequest") {
    new ConnectorFixture {
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
  }

  test("GformConnector.formTemplate - unhappy scenario should it fails when gform returns 4xx code") {
    new ConnectorFixture {
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
  }

  test("GformConnector.form - happy path should return form for given id") {
    new ConnectorFixture {
      val status = 200
      val responseJson = Some(Json.toJson(buildForm))
      connector
        .getForm(formIdData)
        .futureValue shouldBe buildForm
    }
  }

  test("GformConnector.form - unhappy scenarios should it fails when form doesn't exist") {
    new ConnectorFixture {
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
  }

  test("GformConnector.form - unhappy scenarios should it fails when gform returns 5xx") {
    new ConnectorFixture {
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
  }

  test("GformConnector.form - unhappy scenarios should it fails when gform returns BadRequest") {
    new ConnectorFixture {
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
  }

  test("GformConnector.form - unhappy scenarios should it fails when gform returns 4xx code") {
    new ConnectorFixture {
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
  }

  test(
    "GformConnector.createSubmission - successful scenario should create submission and return the Submission entity"
  ) {
    new ConnectorFixture {
      val status = 200
      implicit val localDateTime: LocalDateTime = LocalDateTime.now()
      val responseJson = Some(Json.toJson(submission))
      connector
        .createSubmission(
          formIdData.toFormId,
          formTemplateId,
          envelopeId,
          "some-customer-id",
          1,
          submissionRef
        )
        .futureValue shouldBe submission
    }
  }

  test(
    "GformConnector.createSubmission - successful scenario should it fails when gform returns non OK response"
  ) {
    new ConnectorFixture {
      val status = 400
      val responseJson = None
      val isUpstream400Response = connector
        .createSubmission(formIdData.toFormId, formTemplateId, envelopeId, "some-customer-id", 1, submissionRef)
        .failed
        .futureValue match {
        case UpstreamErrorResponse.Upstream4xxResponse(_) => true
        case _                                            => false
      }
      isUpstream400Response shouldBe true
    }
  }

  test(
    "GformConnector.getHipAgentDetails - happy path should return ServiceResponse with processed attributes on a 200 response"
  ) {
    new ConnectorFixture {
      val status = 200
      val responseJson = Some(
        Json.obj(
          "det" -> Json.obj(
            "success" -> Json.obj(
              "name"     -> "Test Agency",
              "addr1"    -> "1 Test Street",
              "addr2"    -> "",
              "addr3"    -> "",
              "addr4"    -> "",
              "postcode" -> "TE1 1ST",
              "country"  -> "GB",
              "email"    -> "test@example.com",
              "phone"    -> "01234567890"
            )
          )
        )
      )

      val dataRetrieve = DataRetrieve(
        tpe = DataRetrieve.Type("agentDetails"),
        id = DataRetrieveId("agencyInfo"),
        attributes = Attr.FromObject(
          List(
            AttributeInstruction(
              DataRetrieve.Attribute("agencyName"),
              ConstructAttribute.AsIs(Fetch(List("det", "success", "name")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("agencyEmail"),
              ConstructAttribute.AsIs(Fetch(List("det", "success", "email")))
            )
          )
        ),
        attrTypeMapping = Map.empty,
        params = List.empty,
        `if` = None,
        maxFailedAttempts = None,
        failureCountResetMinutes = None,
        callOnNoChange = false,
        populateATL = None,
        UrlDescriptor(
          urlPath = "/hip/agent-details/{{agentReferenceNumber}}",
          destination = UrlDestination.GForm
        ),
        Some(
          UrlDescriptor(
            urlPath = "/etmp/RESTAdapter/generic/agent/subscription/{{agentReferenceNumber}}",
            destination = UrlDestination.HIP
          )
        ),
        manualFallbackIsDesigned = None
      )

      val request = DataRetrieve.Request(
        json = Json.obj("agentReferenceNumber" -> "XARN1234567"),
        params = List("agentReferenceNumber" -> "XARN1234567"),
        previousFailureCount = None,
        failureResetTime = None,
        correlationId = Some("test-correlation-id")
      )

      connector.getHipAgentDetails(dataRetrieve, request).futureValue shouldBe ServiceResponse(
        DataRetrieve.Response.Object(
          Map(
            DataRetrieve.Attribute("agencyName")  -> "Test Agency",
            DataRetrieve.Attribute("agencyEmail") -> "test@example.com"
          )
        )
      )
    }
  }

  test(
    "GformConnector.getHipAgentDetails - error scenarios should return CannotRetrieveResponse when backend returns 500"
  ) {
    new ConnectorFixture {
      val status = 500
      val responseJson = None

      val dataRetrieve = DataRetrieve(
        tpe = DataRetrieve.Type("agentDetails"),
        id = DataRetrieveId("agencyInfo"),
        attributes = Attr.FromObject(List.empty),
        attrTypeMapping = Map.empty,
        params = List.empty,
        `if` = None,
        maxFailedAttempts = None,
        failureCountResetMinutes = None,
        callOnNoChange = false,
        populateATL = None,
        UrlDescriptor(
          urlPath = "/hip/agent-details/{{agentReferenceNumber}}",
          destination = UrlDestination.GForm
        ),
        Some(
          UrlDescriptor(
            urlPath = "/etmp/RESTAdapter/generic/agent/subscription/{{agentReferenceNumber}}",
            destination = UrlDestination.HIP
          )
        ),
        manualFallbackIsDesigned = None
      )

      val request = DataRetrieve.Request(
        json = Json.obj("agentReferenceNumber" -> "XARN1234567"),
        params = List("agentReferenceNumber" -> "XARN1234567"),
        previousFailureCount = None,
        failureResetTime = None,
        correlationId = None
      )

      connector.getHipAgentDetails(dataRetrieve, request).futureValue shouldBe CannotRetrieveResponse
    }
  }

  test(
    "GformConnector.getHipAgentDetails - error scenarios should return CannotRetrieveResponse when backend returns 401"
  ) {
    new ConnectorFixture {
      val status = 401
      val responseJson = None

      val dataRetrieve = DataRetrieve(
        tpe = DataRetrieve.Type("agentDetails"),
        id = DataRetrieveId("agencyInfo"),
        attributes = Attr.FromObject(List.empty),
        attrTypeMapping = Map.empty,
        params = List.empty,
        `if` = None,
        maxFailedAttempts = None,
        failureCountResetMinutes = None,
        callOnNoChange = false,
        populateATL = None,
        UrlDescriptor(
          urlPath = "/hip/agent-details/{{agentReferenceNumber}}",
          destination = UrlDestination.GForm
        ),
        Some(
          UrlDescriptor(
            urlPath = "/etmp/RESTAdapter/generic/agent/subscription/{{agentReferenceNumber}}",
            destination = UrlDestination.HIP
          )
        ),
        manualFallbackIsDesigned = None
      )

      val request = DataRetrieve.Request(
        json = Json.obj("agentReferenceNumber" -> "XARN1234567"),
        params = List("agentReferenceNumber" -> "XARN1234567"),
        previousFailureCount = None,
        failureResetTime = None,
        correlationId = None
      )

      connector.getHipAgentDetails(dataRetrieve, request).futureValue shouldBe CannotRetrieveResponse
    }
  }

  List(404, 503).foreach { status =>
    test(
      "Data retrieve no-record responses" + " should " + s"distinguish no records from technical failure for HIP and DES HTTP $status"
    ) {
      val httpClient = mock(classOf[HttpClientV2])
      val requestBuilder = mock(classOf[RequestBuilder], RETURNS_SELF)
      when(httpClient.get(any[URL])(any[HeaderCarrier])).thenReturn(requestBuilder)
      when(httpClient.post(any[URL])(any[HeaderCarrier])).thenReturn(requestBuilder)
      when(requestBuilder.execute[HttpResponse](any[HttpReads[HttpResponse]], any[ExecutionContext]))
        .thenReturn(Future.successful(HttpResponse(status, "{}")))
      val connector = new GformConnector(httpClient, "http://localhost")
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val dataRetrieve = DataRetrieve(
        tpe = DataRetrieve.Type("agentDetails"),
        id = DataRetrieveId("agencyInfo"),
        attributes = Attr.FromObject(
          List(AttributeInstruction(DataRetrieve.Attribute("name"), ConstructAttribute.AsIs(Fetch(List("name")))))
        ),
        attrTypeMapping = Map.empty,
        params = Nil,
        `if` = None,
        maxFailedAttempts = None,
        failureCountResetMinutes = None,
        callOnNoChange = false,
        populateATL = None,
        urlFrontend = UrlDescriptor("/retrieve", UrlDestination.GForm),
        urlBackend = None,
        manualFallbackIsDesigned = None
      )
      val request = DataRetrieve.Request(Json.obj(), Nil, None, None, None)
      val expected = if (status == 404) dataRetrieve.emptyValidResponse() else CannotRetrieveResponse

      connector.getHipAgentDetails(dataRetrieve, request).futureValue shouldBe expected
      connector
        .getDesOrganisation(dataRetrieve.copy(tpe = DataRetrieve.Type("hmrcRosmRegistrationCheck")), request)
        .futureValue shouldBe expected
    }
  }

  trait ConnectorFixture extends ExampleData {
    def status: Int
    def responseJson: Option[JsValue]
    lazy val r: HttpResponse = HttpResponse(
      status,
      responseJson.orNull,
      Map.empty[String, Seq[String]]
    )

    lazy val wSHttp = new StubbedHttpClientV2(r)
    lazy val connector = new GformConnector(wSHttp, "http://localhost")
    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }

}
