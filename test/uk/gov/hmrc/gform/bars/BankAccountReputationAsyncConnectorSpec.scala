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

package uk.gov.hmrc.gform.bars

import org.apache.pekko.actor.ActorSystem
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.WiremockSupport
import uk.gov.hmrc.gform.sharedmodel.{ Attr, AttributeInstruction, CannotRetrieveResponse, ConstructAttribute, DataRetrieve, DataRetrieveId, Fetch, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.HttpTestUtils
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, RequestId }

import scala.concurrent.ExecutionContext.Implicits.global

class BankAccountReputationAsyncConnectorSpec
    extends AnyFlatSpec with Matchers with WiremockSupport with BeforeAndAfterEach with BeforeAndAfterAll
    with ScalaFutures {

  implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))

  implicit val system: ActorSystem = ActorSystem()
  implicit val hc: HeaderCarrier = HeaderCarrier(requestId = Some(RequestId("some-tracking-id")))

  override protected def beforeAll(): Unit = {
    wireMockServer.start()
    configureFor("localhost", wiremockPort)
  }

  override protected def beforeEach(): Unit =
    wireMockServer.resetAll()

  override protected def afterAll(): Unit = {
    wireMockServer.stop()
    ()
  }

  val url = s"http://localhost:$wiremockPort"

  val wsHttp: HttpClientV2 = HttpTestUtils.getTestImpl(wiremockPort)

  val bankAccountReputationAsyncConnector = new BankAccountReputationAsyncConnector(wsHttp, url)

  trait TestFixture {
    val dataRetrieveValidateBankDetails = DataRetrieve(
      DataRetrieve.Type("validateBankDetails"),
      DataRetrieveId("someId"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("isValid"),
            ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeBankName"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeBankName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
            ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
            ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit")))
          ),
          AttributeInstruction(DataRetrieve.Attribute("iban"), ConstructAttribute.AsIs(Fetch(List("iban"))))
        )
      ),
      Map.empty[DataRetrieve.Attribute, DataRetrieve.AttrType],
      List.empty[DataRetrieve.ParamExpr],
      None,
      None,
      None
    )

    val dataRetrieveCompanyHouseProfile = DataRetrieve(
      DataRetrieve.Type("companyHouseProfile"),
      DataRetrieveId("someId"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("companyName"),
            ConstructAttribute.AsIs(Fetch(List("company_name")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("companyStatus"),
            ConstructAttribute.AsIs(Fetch(List("company_status")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("registeredOfficeAddress"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("registered_office_address", "address_line_1")),
                Fetch(List("registered_office_address", "address_line_2")),
                Fetch(List("registered_office_address", "postal_code")),
                Fetch(List("registered_office_address", "locality")),
                Fetch(List("registered_office_address", "region"))
              )
            )
          )
        )
      ),
      Map.empty[DataRetrieve.Attribute, DataRetrieve.AttrType],
      List.empty[DataRetrieve.ParamExpr],
      None,
      None,
      None
    )

    val dataRetrieveEmployments = DataRetrieve(
      DataRetrieve.Type("employments"),
      DataRetrieveId("someId"),
      Attr.FromArray(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("employerName"),
            ConstructAttribute.AsIs(Fetch(List("employerName")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sequenceNumber"),
            ConstructAttribute.AsIs(Fetch(List("sequenceNumber")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("worksNumber"),
            ConstructAttribute.AsIs(Fetch(List("worksNumber")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("taxDistrictNumber"),
            ConstructAttribute.AsIs(Fetch(List("taxDistrictNumber")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("payeNumber"),
            ConstructAttribute.AsIs(Fetch(List("payeNumber")))
          ),
          AttributeInstruction(DataRetrieve.Attribute("director"), ConstructAttribute.AsIs(Fetch(List("director"))))
        )
      ),
      Map(DataRetrieve.Attribute("sequenceNumber") -> DataRetrieve.AttrType.Number),
      List.empty[DataRetrieve.ParamExpr],
      None,
      None,
      None
    )

    val request = DataRetrieve.Request(
      Json.obj(),
      List.empty[(String, String)],
      None,
      None,
      None
    ) // Request Doesn't matter since response is mocked

  }

  "validateBankDetails" should "call the validate bank details service endpoint with the given request" in new TestFixture {
    stubFor(
      WireMock
        .post(s"/validate/bank-details")
        .withHeader("Content-Type", equalTo("application/json"))
        .willReturn(
          ok(
            Json
              .obj(
                "accountNumberIsWellFormatted"             -> "yes",
                "nonStandardAccountDetailsRequiredForBacs" -> "yes",
                "sortCodeIsPresentOnEISCD"                 -> "yes",
                "sortCodeBankName"                         -> "yes",
                "sortCodeSupportsDirectDebit"              -> "yes",
                "sortCodeSupportsDirectCredit"             -> "yes",
                "iban"                                     -> "Some Bank"
              )
              .toString()
          )
        )
    )

    val future = bankAccountReputationAsyncConnector.validateBankDetails(
      dataRetrieveValidateBankDetails,
      request
    )

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(
          Map(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs") -> "yes",
            DataRetrieve.Attribute("sortCodeBankName")                         -> "yes",
            DataRetrieve.Attribute("isValid")                                  -> "yes",
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit")              -> "yes",
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit")             -> "yes",
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")                 -> "yes",
            DataRetrieve.Attribute("iban")                                     -> "Some Bank"
          )
        )
      )
    }
  }

  it should "call the validate bank details service endpoint and override bad request with exceptional response" in new TestFixture {
    stubFor(
      WireMock
        .post(s"/validate/bank-details")
        .withHeader("Content-Type", equalTo("application/json"))
        .willReturn(
          badRequest().withBody("""
                                  |{
                                  |    "code": "SORT_CODE_ON_DENY_LIST",
                                  |    "desc": "some description"
                                  |}
                                  |""".stripMargin)
        )
    )

    val future = bankAccountReputationAsyncConnector.validateBankDetails(
      dataRetrieveValidateBankDetails,
      request
    )

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(
          Map(
            DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs") -> "indeterminate",
            DataRetrieve.Attribute("sortCodeBankName")                         -> "",
            DataRetrieve.Attribute("isValid")                                  -> "indeterminate",
            DataRetrieve.Attribute("sortCodeSupportsDirectDebit")              -> "",
            DataRetrieve.Attribute("sortCodeSupportsDirectCredit")             -> "",
            DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")                 -> "no",
            DataRetrieve.Attribute("iban")                                     -> ""
          )
        )
      )
    }
  }

  "isFailure" should "return false if account number not well formatted" in new TestFixture {
    val response = DataRetrieve.Response.Object(
      Map(
        DataRetrieve.Attribute("accountNumberIsWellFormatted") -> "no",
        DataRetrieve.Attribute("accountExists")                -> "indeterminate",
        DataRetrieve.Attribute("nameMatches")                  -> "indeterminate",
        DataRetrieve.Attribute("sortCodeSupportsDirectDebit")  -> "",
        DataRetrieve.Attribute("sortCodeSupportsDirectCredit") -> "",
        DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")     -> "",
        DataRetrieve.Attribute("iban")                         -> ""
      )
    )

    bankAccountReputationAsyncConnector.isFailure(response) shouldBe false
  }

  it should "return true if account exists is inapplicable" in new TestFixture {
    val response = DataRetrieve.Response.Object(
      Map(
        DataRetrieve.Attribute("accountNumberIsWellFormatted") -> "yes",
        DataRetrieve.Attribute("accountExists")                -> "inapplicable",
        DataRetrieve.Attribute("nameMatches")                  -> "indeterminate",
        DataRetrieve.Attribute("sortCodeSupportsDirectDebit")  -> "",
        DataRetrieve.Attribute("sortCodeSupportsDirectCredit") -> "",
        DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")     -> "",
        DataRetrieve.Attribute("iban")                         -> ""
      )
    )

    bankAccountReputationAsyncConnector.isFailure(response) shouldBe true
  }

  it should "return false if account exists and name matches" in new TestFixture {
    val response = DataRetrieve.Response.Object(
      Map(
        DataRetrieve.Attribute("accountNumberIsWellFormatted") -> "yes",
        DataRetrieve.Attribute("accountExists")                -> "yes",
        DataRetrieve.Attribute("nameMatches")                  -> "yes",
        DataRetrieve.Attribute("sortCodeSupportsDirectDebit")  -> "",
        DataRetrieve.Attribute("sortCodeSupportsDirectCredit") -> "",
        DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")     -> "",
        DataRetrieve.Attribute("iban")                         -> ""
      )
    )

    bankAccountReputationAsyncConnector.isFailure(response) shouldBe false
  }

  it should "return false if account exists and name is partial match" in new TestFixture {
    val response = DataRetrieve.Response.Object(
      Map(
        DataRetrieve.Attribute("accountNumberIsWellFormatted") -> "yes",
        DataRetrieve.Attribute("accountExists")                -> "yes",
        DataRetrieve.Attribute("nameMatches")                  -> "partial",
        DataRetrieve.Attribute("sortCodeSupportsDirectDebit")  -> "",
        DataRetrieve.Attribute("sortCodeSupportsDirectCredit") -> "",
        DataRetrieve.Attribute("sortCodeIsPresentOnEISCD")     -> "",
        DataRetrieve.Attribute("iban")                         -> ""
      )
    )

    bankAccountReputationAsyncConnector.isFailure(response) shouldBe false
  }

  "ConstructAttribute.Concat" should "should concat fields from json response" in new TestFixture {

    stubFor(
      WireMock
        .post(s"/validate/bank-details")
        .withHeader("Content-Type", equalTo("application/json"))
        .willReturn(
          ok(
            Json
              .obj(
                "company_name"   -> "Company Name",
                "company_status" -> "Active",
                "registered_office_address" -> Json.obj(
                  "address_line_1" -> "line1",
                  "address_line_2" -> "line2",
                  "locality"       -> "locality",
                  "postal_code"    -> "postcode",
                  "region"         -> "region"
                )
              )
              .toString()
          )
        )
    )

    val future = bankAccountReputationAsyncConnector.validateBankDetails(
      dataRetrieveCompanyHouseProfile,
      request
    )

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(
          Map(
            DataRetrieve.Attribute("companyName")             -> "Company Name",
            DataRetrieve.Attribute("companyStatus")           -> "Active",
            DataRetrieve.Attribute("registeredOfficeAddress") -> "line1 line2 postcode locality region"
          )
        )
      )
    }

  }

  "DataRetrieve.Response.Array" should "should be able to process array response" in new TestFixture {

    stubFor(
      WireMock
        .post(s"/validate/bank-details")
        .withHeader("Content-Type", equalTo("application/json"))
        .willReturn(
          ok(
            Json
              .arr(
                Json.obj(
                  "employerName"      -> "Acme",
                  "sequenceNumber"    -> 1234561,
                  "worksNumber"       -> "ACME01",
                  "taxDistrictNumber" -> "123",
                  "payeNumber"        -> "AA1111",
                  "director"          -> true
                ),
                Json.obj(
                  "employerName"      -> "Smith Holdings",
                  "sequenceNumber"    -> 2345678,
                  "worksNumber"       -> "SMITH01",
                  "taxDistrictNumber" -> "789",
                  "payeNumber"        -> "BB22222",
                  "director"          -> false
                ),
                Json
                  .obj(
                    "employerName"      -> "Acme",
                    "sequenceNumber"    -> 3456789,
                    "worksNumber"       -> "ACME09",
                    "taxDistrictNumber" -> "123",
                    "payeNumber"        -> "AA1111",
                    "director"          -> false
                  )
              )
              .toString()
          )
        )
    )

    val future = bankAccountReputationAsyncConnector.validateBankDetails(
      dataRetrieveEmployments,
      request
    )

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Array(
          List(
            Map(
              DataRetrieve.Attribute("director")          -> "true",
              DataRetrieve.Attribute("taxDistrictNumber") -> "123",
              DataRetrieve.Attribute("payeNumber")        -> "AA1111",
              DataRetrieve.Attribute("sequenceNumber")    -> "1234561",
              DataRetrieve.Attribute("employerName")      -> "Acme",
              DataRetrieve.Attribute("worksNumber")       -> "ACME01"
            ),
            Map(
              DataRetrieve.Attribute("director")          -> "false",
              DataRetrieve.Attribute("taxDistrictNumber") -> "789",
              DataRetrieve.Attribute("payeNumber")        -> "BB22222",
              DataRetrieve.Attribute("sequenceNumber")    -> "2345678",
              DataRetrieve.Attribute("employerName")      -> "Smith Holdings",
              DataRetrieve.Attribute("worksNumber")       -> "SMITH01"
            ),
            Map(
              DataRetrieve.Attribute("director")          -> "false",
              DataRetrieve.Attribute("taxDistrictNumber") -> "123",
              DataRetrieve.Attribute("payeNumber")        -> "AA1111",
              DataRetrieve.Attribute("sequenceNumber")    -> "3456789",
              DataRetrieve.Attribute("employerName")      -> "Acme",
              DataRetrieve.Attribute("worksNumber")       -> "ACME09"
            )
          )
        )
      )
    }
  }

  it should "return BadRequest error" in new TestFixture {
    stubFor(
      WireMock
        .post(s"/validate/bank-details")
        .willReturn(
          badRequest().withBody("""
                                  |{
                                  |    "code": "INVALID_SORTCODE",
                                  |    "desc": ": invalid sortcode"
                                  |}
                                  |""".stripMargin)
        )
    )

    val future = bankAccountReputationAsyncConnector.validateBankDetails(
      dataRetrieveValidateBankDetails,
      request
    )

    whenReady(future) { response =>
      response shouldBe CannotRetrieveResponse
    }
  }
}
