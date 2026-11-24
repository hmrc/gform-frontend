/*
 * Copyright 2026 HM Revenue & Customs
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

import izumi.reflect.Tag
import munit.FunSuite
import play.api.libs.json.Json
import play.api.libs.ws.{ BodyWritable, WSRequest }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx }
import uk.gov.hmrc.http.client.{ HttpClientV2, RequestBuilder, StreamHttpReads }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpReads, HttpResponse }

import java.net.URL
import scala.concurrent.{ ExecutionContext, Future }

class FtaPodAuthorisationDetailsConnectorSuite extends FunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val hc: HeaderCarrier = HeaderCarrier()

  private val attributeNames = List(
    "emailConsent",
    "exporterEori",
    "exporterFullName",
    "exporterAddressName",
    "exporterStreetAndNumber",
    "exporterPostCode",
    "exporterCity",
    "exporterCountryCode",
    "yourDetailsFullName",
    "yourDetailsAddressLine1",
    "yourDetailsAddressLine2",
    "yourDetailsPostCode",
    "yourDetailsCity",
    "yourDetailsCountry",
    "yourDetailsTelephoneNumber",
    "yourDetailsEmailAddress",
    "exporterActivity",
    "version",
    "lastPodAuthModifiedDate",
    "hs4Code",
    "goodsDescription",
    "businessFailure"
  )

  private val dataRetrieve = DataRetrieve(
    DataRetrieve.Type("ftaPodAuthorisationDetails"),
    DataRetrieveId("podDetails"),
    Attr.FromArray(
      attributeNames.map(name =>
        AttributeInstruction(DataRetrieve.Attribute(name), ConstructAttribute.AsIs(Fetch(List(name))))
      )
    ),
    attributeNames
      .map(name =>
        DataRetrieve.Attribute(name) -> (if (name == "version") DataRetrieve.AttrType.Number
                                         else DataRetrieve.AttrType.String)
      )
      .toMap,
    List(DataRetrieve.ParamExpr(DataRetrieve.Parameter("podReference"), FormCtx(FormComponentId("podRef")))),
    None,
    None,
    None,
    callOnNoChange = false,
    None,
    UrlDescriptor("/if/fta/maintainpodauthorisationdetails/{{podReference}}/v1", UrlDestination.GForm),
    Some(UrlDescriptor("/fta/maintainPodAuthorisationDetails/{{podReference}}/v1", UrlDestination.IF)),
    None
  )

  private def request(correlationId: Option[String] = Some("correlation-123")) = DataRetrieve.Request(
    Json.obj(),
    List("podReference" -> "POD123456789012"),
    None,
    None,
    correlationId
  )

  private val flattenedRows = Json.arr(
    Json.obj(
      "emailConsent"               -> true,
      "exporterEori"               -> "GB523456789586000",
      "exporterFullName"           -> "Andrew Flintoff",
      "exporterAddressName"        -> "Rose Cottage",
      "exporterStreetAndNumber"    -> "10 Downing Street",
      "exporterPostCode"           -> "SW1A 2AA",
      "exporterCity"               -> "London",
      "exporterCountryCode"        -> "GB",
      "yourDetailsFullName"        -> "Freddie Flintoff",
      "yourDetailsAddressLine1"    -> "111 Old Trafford Street",
      "yourDetailsAddressLine2"    -> "Old Trafford",
      "yourDetailsPostCode"        -> "MN12 7LD",
      "yourDetailsCity"            -> "Manchester",
      "yourDetailsCountry"         -> "GB",
      "yourDetailsTelephoneNumber" -> "0161 528467",
      "yourDetailsEmailAddress"    -> "andrew@gmail.com",
      "exporterActivity"           -> "Trading",
      "version"                    -> 1,
      "lastPodAuthModifiedDate"    -> "2026-10-05T22:28:58.303182443Z",
      "hs4Code"                    -> "1900",
      "goodsDescription"           -> "Soft Drinks"
    ),
    Json.obj(
      "emailConsent"            -> true,
      "exporterEori"            -> "GB523456789586000",
      "exporterFullName"        -> "Andrew Flintoff",
      "exporterStreetAndNumber" -> "10 Downing Street",
      "exporterCity"            -> "London",
      "yourDetailsFullName"     -> "Freddie Flintoff",
      "yourDetailsAddressLine1" -> "111 Old Trafford Street",
      "yourDetailsAddressLine2" -> "Old Trafford",
      "yourDetailsPostCode"     -> "MN12 7LD",
      "yourDetailsCity"         -> "Manchester",
      "yourDetailsCountry"      -> "GB",
      "yourDetailsEmailAddress" -> "andrew@gmail.com",
      "exporterActivity"        -> "Trading",
      "version"                 -> 1,
      "lastPodAuthModifiedDate" -> "2026-10-05T22:28:58.303182443Z",
      "hs4Code"                 -> "1905",
      "goodsDescription"        -> "Biscuits"
    )
  )

  private def businessFailureBody(validationMessage: String) =
    Json.stringify(
      Json.obj(
        "businessFailure" -> Json.obj(
          "podReference"      -> "POD123456789012",
          "failureCode"       -> "2",
          "validationMessage" -> validationMessage
        )
      )
    )

  private def call(response: HttpResponse): (Future[ServiceCallResponse[DataRetrieve.Response]], RawHttpClient) = {
    val httpClient = new RawHttpClient(response)
    val connector = new GformConnector(httpClient, "http://localhost:9196/gform")
    connector.ftaPodAuthorisationDetails(dataRetrieve, request()) -> httpClient
  }

  private def onlyBusinessFailure(value: String): ServiceCallResponse[DataRetrieve.Response] =
    ServiceResponse(
      DataRetrieve.Response.Array(
        List(
          attributeNames
            .map(name => DataRetrieve.Attribute(name) -> (if (name == "businessFailure") value else ""))
            .toMap
        )
      )
    )

  test("ftaPodAuthorisationDetails - call gform with the filled url and correlationId header") {
    val (result, httpClient) = call(HttpResponse(200, Json.stringify(flattenedRows)))

    result.map { _ =>
      assertEquals(
        httpClient.capturedUrl.map(_.toString),
        Some("http://localhost:9196/gform/if/fta/maintainpodauthorisationdetails/POD123456789012/v1")
      )
      assertEquals(httpClient.capturedHeaders, Seq("correlationId" -> "correlation-123"))
    }
  }

  test("ftaPodAuthorisationDetails - return one row per goods item for a 200 response") {
    val (result, _) = call(HttpResponse(200, Json.stringify(flattenedRows)))

    result.map {
      case ServiceResponse(DataRetrieve.Response.Array(rows)) =>
        assertEquals(rows.size, 2)
        val first = rows.head
        val second = rows(1)
        assertEquals(first(DataRetrieve.Attribute("emailConsent")), "true")
        assertEquals(first(DataRetrieve.Attribute("version")), "1")
        assertEquals(first(DataRetrieve.Attribute("exporterAddressName")), "Rose Cottage")
        assertEquals(first(DataRetrieve.Attribute("hs4Code")), "1900")
        assertEquals(first(DataRetrieve.Attribute("goodsDescription")), "Soft Drinks")
        assertEquals(first(DataRetrieve.Attribute("businessFailure")), "")
        assertEquals(second(DataRetrieve.Attribute("hs4Code")), "1905")
        assertEquals(second(DataRetrieve.Attribute("goodsDescription")), "Biscuits")
        assertEquals(second(DataRetrieve.Attribute("exporterAddressName")), "")
        assertEquals(second(DataRetrieve.Attribute("yourDetailsTelephoneNumber")), "")
      case other => fail(s"Unexpected response $other")
    }
  }

  test("ftaPodAuthorisationDetails - return empty array for a 404 response") {
    val (result, _) = call(HttpResponse(404, """{"errorDetail":{"errorCode":404,"errorMessage":"Not Found"}}"""))

    result.map(response => assertEquals(response, ServiceResponse(DataRetrieve.Response.Array(List.empty))))
  }

  List(
    "Invalid pod authorisation pattern provided"                       -> "INVALID_POD_REFERENCE",
    "No matching authorisation found using the supplied Pod Reference" -> "NO_MATCHING_AUTHORISATION",
    "Decision not current"                                             -> "DECISION_NOT_CURRENT",
    "Some new validation message"                                      -> "UNKNOWN_BUSINESS_FAILURE"
  ).foreach { case (validationMessage, expected) =>
    test(s"ftaPodAuthorisationDetails - map 400 business failure '$validationMessage' to $expected") {
      val (result, _) = call(HttpResponse(400, businessFailureBody(validationMessage)))

      result.map(response => assertEquals(response, onlyBusinessFailure(expected)))
    }
  }

  test("ftaPodAuthorisationDetails - return CannotRetrieveResponse for a 400 that is not a business failure") {
    val (result, _) = call(HttpResponse(400, """{"technicalFailure":{"validationMessages":["bad"]}}"""))

    result.map(response => assertEquals(response, CannotRetrieveResponse))
  }

  test("ftaPodAuthorisationDetails - return CannotRetrieveResponse for a 500 response") {
    val (result, _) =
      call(HttpResponse(500, """{"errorDetail":{"errorCode":500,"errorMessage":"An internal error has occurred"}}"""))

    result.map(response => assertEquals(response, CannotRetrieveResponse))
  }

  test("ftaPodAuthorisationDetails - generate a correlationId header when the request has none") {
    val httpClient = new RawHttpClient(HttpResponse(200, Json.stringify(flattenedRows)))
    val connector = new GformConnector(httpClient, "http://localhost:9196/gform")

    connector.ftaPodAuthorisationDetails(dataRetrieve, request(correlationId = None)).map { _ =>
      assertEquals(httpClient.capturedHeaders.map(_._1), Seq("correlationId"))
      assert(httpClient.capturedHeaders.head._2.nonEmpty)
    }
  }

  private class RawHttpClient(response: HttpResponse) extends HttpClientV2 {
    var capturedUrl: Option[URL] = None
    var capturedHeaders: Seq[(String, String)] = Seq.empty

    private def builder(url: URL): RequestBuilder = {
      capturedUrl = Some(url)
      new RawRequestBuilder(this, response)
    }

    override def get(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = builder(url)
    override def post(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = builder(url)
    override def put(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = builder(url)
    override def delete(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = builder(url)
    override def head(url: URL)(implicit hc: HeaderCarrier): RequestBuilder = builder(url)
    override protected def mkRequestBuilder(url: URL, method: String)(implicit hc: HeaderCarrier): RequestBuilder =
      builder(url)
  }

  private class RawRequestBuilder(client: RawHttpClient, response: HttpResponse) extends RequestBuilder {
    override def execute[A](implicit rds: HttpReads[A], ec: ExecutionContext): Future[A] =
      Future(rds.read("GET", "http://localhost", response))

    override def setHeader(headers: (String, String)*): RequestBuilder = {
      client.capturedHeaders = client.capturedHeaders ++ headers
      this
    }
    override def transform(transform: WSRequest => WSRequest): RequestBuilder = this
    override def stream[A: StreamHttpReads](implicit ec: ExecutionContext): Future[A] = execute[A]
    override def withProxy: RequestBuilder = this
    override def withBody[B: BodyWritable: Tag](body: B)(implicit ec: ExecutionContext): RequestBuilder = this
  }
}
