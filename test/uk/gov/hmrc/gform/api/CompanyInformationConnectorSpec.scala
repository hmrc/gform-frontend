/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.api

import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock.{ configureFor, notFound, ok, serverError, stubFor }
import org.apache.pekko.actor.ActorSystem
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Seconds, Span }
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.WiremockSupport
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.Attribute
import uk.gov.hmrc.gform.sharedmodel.{ Attr, AttributeInstruction, CannotRetrieveResponse, ConstructAttribute, DataRetrieve, DataRetrieveId, Fetch, ServiceResponse }
import uk.gov.hmrc.gform.wshttp.HttpTestUtils
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.http.{ HeaderCarrier, RequestId }

import scala.collection.immutable.List
import scala.concurrent.ExecutionContext.Implicits.global

class CompanyInformationConnectorSpec
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

  val companyInformationAsyncConnector = new CompanyInformationAsyncConnector(wsHttp, url)

  trait TestFixture {
    val dataRetrieveCompanyHouseProfile = DataRetrieve(
      DataRetrieve.Type("companyHouseProfile"),
      DataRetrieveId("someId"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            Attribute("companyName"),
            ConstructAttribute.AsIs(Fetch(List("company_name")))
          ),
          AttributeInstruction(
            Attribute("companyStatus"),
            ConstructAttribute.AsIs(Fetch(List("company_status")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("registeredOfficeAddress"),
            ConstructAttribute.Concat(
              List(
                Fetch(List("registered_office_address", "address_line_1")),
                Fetch(List("registered_office_address", "address_line_2")),
                Fetch(List("registered_office_address", "po_box")),
                Fetch(List("registered_office_address", "locality")),
                Fetch(List("registered_office_address", "region")),
                Fetch(List("registered_office_address", "postal_code")),
                Fetch(List("registered_office_address", "country"))
              )
            )
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("dateOfCreation"),
            ConstructAttribute.AsIs(Fetch(List("date_of_creation")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode1"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 0)
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode2"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 1)
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode3"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 2)
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("sicCode4"),
            ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 3)
          )
        )
      ),
      Map.empty[DataRetrieve.Attribute, DataRetrieve.AttrType],
      List.empty[DataRetrieve.ParamExpr],
      None
    )

    val dataRetrieveCompanyHouseActiveOfficers = DataRetrieve(
      DataRetrieve.Type("companyHouseActiveOfficers"),
      DataRetrieveId("directors"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("activeDirectors"),
            ConstructAttribute.AsIs(Fetch(List("active_directors")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("activeSecretaries"),
            ConstructAttribute.AsIs(Fetch(List("active_secretaries")))
          ),
          AttributeInstruction(
            DataRetrieve.Attribute("activeLlpMembers"),
            ConstructAttribute.AsIs(Fetch(List("active_llp_members")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("activeDirectors")   -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("activeSecretaries") -> DataRetrieve.AttrType.Number,
        DataRetrieve.Attribute("activeLlpMembers")  -> DataRetrieve.AttrType.Number
      ),
      List.empty[DataRetrieve.ParamExpr],
      None
    )

    val dataRetrieveCompanyHouseInsolvency =
      DataRetrieve(
        DataRetrieve.Type("companyHouseInsolvency"),
        DataRetrieveId("insolvency"),
        Attr.FromArray(
          List(
            AttributeInstruction(
              DataRetrieve.Attribute("caseType"),
              ConstructAttribute.AsIs(Fetch(List("caseType")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("caseNumber"),
              ConstructAttribute.AsIs(Fetch(List("caseNumber")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("name"),
              ConstructAttribute.AsIs(Fetch(List("name")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("role"),
              ConstructAttribute.AsIs(Fetch(List("role")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("address_line_1"),
              ConstructAttribute.AsIs(Fetch(List("address", "address_line_1")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("address_line_2"),
              ConstructAttribute.AsIs(Fetch(List("address", "address_line_2")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("locality"),
              ConstructAttribute.AsIs(Fetch(List("address", "locality")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("region"),
              ConstructAttribute.AsIs(Fetch(List("address", "region")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("postal_code"),
              ConstructAttribute.AsIs(Fetch(List("address", "postal_code")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("country"),
              ConstructAttribute.AsIs(Fetch(List("address", "country")))
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("primaryAddress"),
              ConstructAttribute.Combine(
                List(
                  DataRetrieve.Attribute("address_line_1") -> Fetch(List("address", "address_line_1")),
                  DataRetrieve.Attribute("address_line_2") -> Fetch(List("address", "address_line_2")),
                  DataRetrieve.Attribute("locality")       -> Fetch(List("address", "locality")),
                  DataRetrieve.Attribute("region")         -> Fetch(List("address", "region")),
                  DataRetrieve.Attribute("postal_code")    -> Fetch(List("address", "postal_code")),
                  DataRetrieve.Attribute("country")        -> Fetch(List("address", "country"))
                )
              )
            ),
            AttributeInstruction(
              DataRetrieve.Attribute("appointedOn"),
              ConstructAttribute.AsIs(Fetch(List("appointedOn")))
            )
          )
        ),
        Map(
          DataRetrieve.Attribute("caseType")       -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("caseNumber")     -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("name")           -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("role")           -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("address_line_1") -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("address_line_1") -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("locality")       -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("region")         -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("postal_code")    -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("country")        -> DataRetrieve.AttrType.String,
          DataRetrieve.Attribute("appointedOn")    -> DataRetrieve.AttrType.Date
        ),
        List.empty[DataRetrieve.ParamExpr],
        None
      )

    val request = DataRetrieve.Request(
      Json.obj(),
      List.empty[(String, String)]
    ) // Request Doesn't matter since response is mocked
  }

  "companyProfile" should "call the gform companies house endpoint and return valid company details when valid data is received from the API" in new TestFixture {
    val companyReturn: JsValue = Json.parse("""{
                                              |  "accounts": {
                                              |    "accounting_reference_date": {
                                              |      "day": 1,
                                              |      "month": 1
                                              |    },
                                              |    "last_accounts": {
                                              |      "made_up_to": "2024-01-01",
                                              |      "type": ""
                                              |    },
                                              |    "next_due": "2025-01-01",
                                              |    "next_made_up_to": "2025-01-01",
                                              |    "overdue": false
                                              |  },
                                              |  "annual_return": {
                                              |    "last_made_up_to": "2022-01-01",
                                              |    "next_due": "2025-01-01",
                                              |    "next_made_up_to": "2025-01-01",
                                              |    "overdue": false
                                              |  },
                                              |  "branch_company_details": {
                                              |    "business_activity": "",
                                              |    "parent_company_name": "",
                                              |    "parent_company_number": ""
                                              |  },
                                              |  "can_file": false,
                                              |  "company_name": "test company name",
                                              |  "company_number": "01234567",
                                              |  "company_status": "Active",
                                              |  "company_status_detail": "",
                                              |  "confirmation_statement": {
                                              |    "last_made_up_to": "2023-01-01",
                                              |    "next_due": "2025-01-01",
                                              |    "next_made_up_to": "2025-01-01",
                                              |    "overdue": false
                                              |  },
                                              |  "date_of_creation": "2024-01-01",
                                              |  "date_of_dissolution": "2025-01-01",
                                              |  "etag": "",
                                              |  "foreign_company_details": {
                                              |    "accounting_requirement": {
                                              |      "foreign_account_type": "",
                                              |      "terms_of_account_publication": ""
                                              |    },
                                              |    "accounts": {
                                              |      "account_period_from": {
                                              |        "day": 1,
                                              |        "month": 1
                                              |      },
                                              |      "account_period_to": {
                                              |        "day": 1,
                                              |        "month": 1
                                              |      },
                                              |      "must_file_within": {
                                              |        "months": 1
                                              |      }
                                              |    },
                                              |    "business_activity": "",
                                              |    "company_type": "",
                                              |    "governed_by": "",
                                              |    "is_a_credit_finance_institution": false,
                                              |    "originating_registry": {
                                              |      "country": "",
                                              |      "name": ""
                                              |    },
                                              |    "registration_number": ""
                                              |  },
                                              |  "has_been_liquidated": false,
                                              |  "has_charges": false,
                                              |  "has_insolvency_history": false,
                                              |  "is_community_interest_company": false,
                                              |  "jurisdiction": "",
                                              |  "last_full_members_list_date": "2024-01-01",
                                              |  "links": {
                                              |    "persons_with_significant_control_list": "",
                                              |    "persons_with_significant_control_statements_list": "",
                                              |    "self": ""
                                              |  },
                                              |  "officer_summary": {
                                              |    "active_count": 1,
                                              |    "officers": [
                                              |      {
                                              |        "appointed_on": "2024-01-01",
                                              |        "date_of_birth": {
                                              |          "day": 23,
                                              |          "month": 4,
                                              |          "year": 1948
                                              |        },
                                              |        "name": "Jim Ferguson",
                                              |        "officer_role": "director"
                                              |      }
                                              |    ],
                                              |    "resigned_count": 0
                                              |  },
                                              |  "registered_office_address": {
                                              |    "address_line_1": "test address line 1",
                                              |    "address_line_2": "test address line 2",
                                              |    "care_of": "",
                                              |    "country": "test country",
                                              |    "locality": "test locality",
                                              |    "po_box": "test po box",
                                              |    "postal_code": "test postal code",
                                              |    "premises": "",
                                              |    "region": "test region"
                                              |  },
                                              |  "registered_office_is_in_dispute": "boolean",
                                              |  "sic_codes": [
                                              |    "1234",
                                              |    "2345"
                                              |  ],
                                              |  "type": "",
                                              |  "undeliverable_registered_office_address": false
                                              |}
                                              |""".stripMargin)

    stubFor(
      WireMock
        .get(s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D")
        .willReturn(
          ok(companyReturn.toString)
        )
    )

    val future = companyInformationAsyncConnector.companyProfile(dataRetrieveCompanyHouseProfile, request)

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(
          Map(
            Attribute("companyName")   -> "test company name",
            Attribute("companyStatus") -> "Active",
            Attribute(
              "registeredOfficeAddress"
            )                           -> "test address line 1 test address line 2 test po box test locality test region test postal code test country",
            Attribute("dateOfCreation") -> "2024-01-01",
            Attribute("sicCode1")       -> "1234",
            Attribute("sicCode2")       -> "2345",
            Attribute("sicCode3")       -> "",
            Attribute("sicCode4")       -> ""
          )
        )
      )
    }
  }

  it should "call the gform companies house endpoint and return empty company details when the company is not found from the API" in new TestFixture {
    stubFor(
      WireMock
        .get(s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D")
        .willReturn(
          notFound()
        )
    )

    val future = companyInformationAsyncConnector.companyProfile(dataRetrieveCompanyHouseProfile, request)

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(Map.empty)
      )
    }
  }

  it should "call the gform companies house endpoint and return the cannot retrieve response when there is an error accessing the API" in new TestFixture {
    stubFor(
      WireMock
        .get(s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D")
        .willReturn(
          serverError()
        )
    )

    val future = companyInformationAsyncConnector.companyProfile(dataRetrieveCompanyHouseProfile, request)

    whenReady(future) { response =>
      response shouldBe CannotRetrieveResponse
    }
  }

  "companyHouseActiveOfficers" should "call the gform companies house endpoint and return valid company active officers when valid data is received from the API" in new TestFixture {
    val companyActiveOfficersReturn: JsValue = Json.parse("""{
                                                            |   "items": [
                                                            |       {
                                                            |           "officer_role": "director"
                                                            |       },
                                                            |       {
                                                            |           "officer_role": "director"
                                                            |       },
                                                            |       {
                                                            |           "officer_role": "director",
                                                            |           "resigned_on": "01-01-2000"
                                                            |       },
                                                            |       {
                                                            |           "officer_role": "secretary",
                                                            |           "resigned_on": "01-01-2000"
                                                            |       },
                                                            |       {
                                                            |           "officer_role": "secretary"
                                                            |       },
                                                            |       {
                                                            |           "officer_role": "secretary",
                                                            |           "resigned_on": "01-01-2000"
                                                            |       },
                                                            |       {
                                                            |           "officer_role": "llp-member"
                                                            |       }
                                                            |   ]
                                                            |}
                                                            |""".stripMargin)

    stubFor(
      WireMock
        .get(
          s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D/officers"
        )
        .willReturn(
          ok(companyActiveOfficersReturn.toString)
        )
    )

    val future = companyInformationAsyncConnector.companyOfficers(dataRetrieveCompanyHouseActiveOfficers, request)

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(
          Map(
            Attribute("activeDirectors")   -> "2",
            Attribute("activeSecretaries") -> "1",
            Attribute("activeLlpMembers")  -> "1"
          )
        )
      )
    }
  }

  it should "call the gform companies house endpoint and return empty company active officers details when the company active officers is not found from the API" in new TestFixture {
    stubFor(
      WireMock
        .get(
          s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D/officers"
        )
        .willReturn(
          notFound()
        )
    )

    val future = companyInformationAsyncConnector.companyOfficers(dataRetrieveCompanyHouseActiveOfficers, request)

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Object(Map.empty)
      )
    }
  }

  it should "call the companies house active officers api proxy endpoint and return the cannot retrieve response when there is an error accessing the API" in new TestFixture {
    stubFor(
      WireMock
        .get(
          s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D/officers"
        )
        .willReturn(
          serverError()
        )
    )

    val future = companyInformationAsyncConnector.companyOfficers(dataRetrieveCompanyHouseActiveOfficers, request)

    whenReady(future) { response =>
      response shouldBe CannotRetrieveResponse
    }
  }

  "companyInsolvency" should "call the gform companies house endpoint and return valid company insolvency details when valid data is received from the API" in new TestFixture {
    val companyInsolvency: JsValue = Json.parse(s"""
                                                   |{
                                                   |  "etag": "somelongstring",
                                                   |  "cases": [
                                                   |    {
                                                   |      "type": "receiver-manager",
                                                   |      "dates": [],
                                                   |      "practitioners": [
                                                   |        {
                                                   |          "name": "John Smith",
                                                   |          "address": {
                                                   |            "address_line_1": "1 Somewhere St",
                                                   |            "address_line_2": "Someburb",
                                                   |            "locality": "Somewheresville",
                                                   |            "postal_code": "SM1 2WH"
                                                   |          },
                                                   |          "appointed_on": "2022-08-05",
                                                   |          "role": "receiver-manager"
                                                   |        },
                                                   |        {
                                                   |          "name": "Jane Doe",
                                                   |          "address": {
                                                   |            "address_line_1": "1 Somewhere St",
                                                   |            "address_line_2": "Someburb",
                                                   |            "locality": "Somewheresville",
                                                   |            "postal_code": "SM1 2WH"
                                                   |          },
                                                   |          "appointed_on": "2022-08-05",
                                                   |          "role": "receiver-manager"
                                                   |        }
                                                   |      ],
                                                   |      "links": {
                                                   |        "charge": "/company/00000001/charges/somestring"
                                                   |      },
                                                   |      "number": "1"
                                                   |    },
                                                   |    {
                                                   |      "type": "receiver-manager",
                                                   |      "dates": [],
                                                   |      "practitioners": [
                                                   |        {
                                                   |          "name": "John Smith",
                                                   |          "address": {
                                                   |            "address_line_1": "1 Somewhere St",
                                                   |            "address_line_2": "Someburb",
                                                   |            "locality": "Somewheresville",
                                                   |            "postal_code": "SM1 2WH"
                                                   |          },
                                                   |          "appointed_on": "2022-08-05",
                                                   |          "ceased_to_act_on": "2023-10-01",
                                                   |          "role": "receiver-manager"
                                                   |        },
                                                   |        {
                                                   |          "name": "Jane Doe",
                                                   |          "address": {
                                                   |            "address_line_1": "1 Somewhere St",
                                                   |            "address_line_2": "Someburb",
                                                   |            "locality": "Somewheresville",
                                                   |            "postal_code": "SM1 2WH"
                                                   |          },
                                                   |          "appointed_on": "2022-08-05",
                                                   |          "role": "receiver-manager"
                                                   |        }
                                                   |      ],
                                                   |      "links": {
                                                   |        "charge": "/company/00000001/charges/somestring"
                                                   |      },
                                                   |      "number": "2"
                                                   |    },
                                                   |    {
                                                   |      "type": "in-administration",
                                                   |      "dates": [
                                                   |        {
                                                   |          "type": "administration-started-on",
                                                   |          "date": "2022-10-21"
                                                   |        }
                                                   |      ],
                                                   |      "practitioners": [
                                                   |        {
                                                   |          "name": "John McSmith",
                                                   |          "address": {
                                                   |            "address_line_1": "58 Somewhere Rd",
                                                   |            "locality": "Somewheresville",
                                                   |            "region": "Somewhereshire",
                                                   |            "postal_code": "SM2 1WH"
                                                   |          },
                                                   |          "role": "practitioner"
                                                   |        }
                                                   |      ],
                                                   |      "number": "3"
                                                   |    }
                                                   |  ],
                                                   |  "status": [
                                                   |    "in-administration",
                                                   |    "receiver-manager"
                                                   |  ]
                                                   |}
                                                   |""".stripMargin)

    stubFor(
      WireMock
        .get(s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D/insolvency")
        .willReturn(
          ok(companyInsolvency.toString)
        )
    )

    val future = companyInformationAsyncConnector.companyInsolvency(dataRetrieveCompanyHouseInsolvency, request)

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Array(
          List(
            Map(
              Attribute("locality")       -> "Somewheresville",
              Attribute("address_line_1") -> "1 Somewhere St",
              Attribute("caseNumber")     -> "1",
              Attribute("country")        -> "",
              Attribute("name")           -> "John Smith",
              Attribute("caseType")       -> "receiver-manager",
              Attribute("address_line_2") -> "Someburb",
              Attribute("region")         -> "",
              Attribute("postal_code")    -> "SM1 2WH",
              Attribute("appointedOn")    -> "2022-08-05",
              Attribute("role")           -> "receiver-manager"
            ),
            Map(
              Attribute("locality")       -> "Somewheresville",
              Attribute("address_line_1") -> "1 Somewhere St",
              Attribute("caseNumber")     -> "1",
              Attribute("country")        -> "",
              Attribute("name")           -> "Jane Doe",
              Attribute("caseType")       -> "receiver-manager",
              Attribute("address_line_2") -> "Someburb",
              Attribute("region")         -> "",
              Attribute("postal_code")    -> "SM1 2WH",
              Attribute("appointedOn")    -> "2022-08-05",
              Attribute("role")           -> "receiver-manager"
            ),
            Map(
              Attribute("locality")       -> "Somewheresville",
              Attribute("address_line_1") -> "1 Somewhere St",
              Attribute("caseNumber")     -> "2",
              Attribute("country")        -> "",
              Attribute("name")           -> "Jane Doe",
              Attribute("caseType")       -> "receiver-manager",
              Attribute("address_line_2") -> "Someburb",
              Attribute("region")         -> "",
              Attribute("postal_code")    -> "SM1 2WH",
              Attribute("appointedOn")    -> "2022-08-05",
              Attribute("role")           -> "receiver-manager"
            ),
            Map(
              Attribute("locality")       -> "Somewheresville",
              Attribute("address_line_1") -> "58 Somewhere Rd",
              Attribute("caseNumber")     -> "3",
              Attribute("country")        -> "",
              Attribute("name")           -> "John McSmith",
              Attribute("caseType")       -> "in-administration",
              Attribute("address_line_2") -> "",
              Attribute("region")         -> "Somewhereshire",
              Attribute("postal_code")    -> "SM2 1WH",
              Attribute("appointedOn")    -> "",
              Attribute("role")           -> "practitioner"
            )
          )
        )
      )
    }
  }

  it should "call the gform companies house endpoint and return empty company insolvency details when the company insolvency detais are not found from the API" in new TestFixture {
    stubFor(
      WireMock
        .get(
          s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D/insolvency"
        )
        .willReturn(
          notFound()
        )
    )

    val future = companyInformationAsyncConnector.companyInsolvency(dataRetrieveCompanyHouseInsolvency, request)

    whenReady(future) { response =>
      response shouldBe ServiceResponse(
        DataRetrieve.Response.Array(List.empty)
      )
    }
  }

  it should "call the companies house insolvency details api proxy endpoint and return the cannot retrieve response when there is an error accessing the API" in new TestFixture {
    stubFor(
      WireMock
        .get(
          s"/companieshouse/company/%7B%7BcompanyNumber%7D%7D/insolvency"
        )
        .willReturn(
          serverError()
        )
    )

    val future = companyInformationAsyncConnector.companyInsolvency(dataRetrieveCompanyHouseInsolvency, request)

    whenReady(future) { response =>
      response shouldBe CannotRetrieveResponse
    }
  }

}
