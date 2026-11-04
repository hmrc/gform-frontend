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

package uk.gov.hmrc.gform.gform

import com.github.tomakehurst.wiremock.client.WireMock._
import com.github.tomakehurst.wiremock.http.Fault
import org.apache.pekko.actor.ActorSystem
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }
import org.scalatest.time.{ Seconds, Span }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.{ Spec, WiremockSupport }
import uk.gov.hmrc.gform.api.NinoInsightsAsyncConnector
import uk.gov.hmrc.gform.bars.BankAccountReputationAsyncConnector
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.wshttp.HttpTestUtils
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

class DataRetrieveConnectorBlueprintSpec
    extends Spec with WiremockSupport with BeforeAndAfterAll with BeforeAndAfterEach {

  override implicit val patienceConfig: PatienceConfig = PatienceConfig(timeout = Span(5, Seconds))
  implicit val system: ActorSystem = ActorSystem()
  implicit val hc: HeaderCarrier = HeaderCarrier()

  override protected def beforeAll(): Unit = {
    wireMockServer.start()
    configureFor("localhost", wiremockPort)
  }

  override protected def beforeEach(): Unit = wireMockServer.resetAll()

  override protected def afterAll(): Unit = {
    wireMockServer.stop()
    system.terminate()
    ()
  }

  private val baseUrl = s"http://localhost:$wiremockPort"
  private val httpClient = HttpTestUtils.getTestImpl(wiremockPort)
  private val connector = new DataRetrieveConnectorBlueprint(httpClient, baseUrl, "test data retrieve")
  private val backend = new GformConnector(httpClient, baseUrl)
  private val bars = new BankAccountReputationAsyncConnector(httpClient, baseUrl)
  private val nino = new NinoInsightsAsyncConnector(httpClient, baseUrl, "test-token")

  private val dataRetrieve = DataRetrieve(
    tpe = DataRetrieve.Type("test"),
    id = DataRetrieveId("test"),
    attributes = Attr.FromObject(Nil),
    attrTypeMapping = Map.empty,
    params = Nil,
    `if` = None,
    maxFailedAttempts = None,
    failureCountResetMinutes = None,
    callOnNoChange = false,
    populateATL = None,
    urlFrontend = UrlDescriptor("/retrieve", UrlDestination.GForm),
    urlBackend = None
  )
  private val request = DataRetrieve.Request(Json.obj(), Nil, None, None, None)

  private type Retrieve = DataRetrieve => Future[ServiceCallResponse[DataRetrieve.Response]]
  private val methods: List[(String, Retrieve)] = List(
    "GET"                     -> ((dr: DataRetrieve) => connector.get(dr, request)),
    "POST"                    -> ((dr: DataRetrieve) => connector.post(dr, request)),
    "GET empty if not found"  -> ((dr: DataRetrieve) => connector.getEmptyIfNotFound(dr, request)),
    "POST empty if not found" -> ((dr: DataRetrieve) => connector.postEmptyIfNotFound(dr, request))
  )

  methods.foreach { case (method, retrieve) =>
    behavior of s"DataRetrieveConnectorBlueprint $method"

    List(false, true).foreach { manualFallback =>
      it should s"only allow an intentional not found response with manual fallback $manualFallback" in {
        stubFor(any(urlEqualTo("/retrieve")).willReturn(notFound()))
        val dr = dataRetrieve.copy(manualFallbackIsDesigned = manualFallback)

        retrieve(dr).futureValue shouldBe
          (if (method.contains("empty if not found")) dr.emptyValidResponse() else CannotRetrieveResponse)
      }
    }

    List(201, 204, 302, 400, 401, 403, 404, 422, 500, 503)
      .filterNot(status => status == 404 && method.contains("empty if not found"))
      .foreach { status =>
        it should s"return cannot retrieve for HTTP $status even with a valid body" in {
          stubFor(any(urlEqualTo("/retrieve")).willReturn(aResponse().withStatus(status).withBody("{}")))

          retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
        }
      }

    it should "preserve a successful empty object" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(okJson("{}")))

      retrieve(dataRetrieve).futureValue shouldBe ServiceResponse(DataRetrieve.Response.Object(Map.empty))
    }

    it should "preserve a successful empty array" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(okJson("[]")))

      retrieve(dataRetrieve.copy(attributes = Attr.FromArray(Nil))).futureValue shouldBe
        ServiceResponse(DataRetrieve.Response.Array(Nil))
    }

    it should "return cannot retrieve for malformed JSON" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(ok("not json")))

      retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
    }

    it should "return cannot retrieve for an unexpected response shape" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(okJson("{}")))

      retrieve(dataRetrieve.copy(attributes = Attr.FromArray(Nil))).futureValue shouldBe CannotRetrieveResponse
    }

    it should "return cannot retrieve when the upstream connection fails" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(aResponse().withFault(Fault.CONNECTION_RESET_BY_PEER)))

      retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
    }
  }

  behavior of "Data retrieve direct connector callsites"

  private val backendMethods: List[(String, Retrieve)] = List(
    "claim validation" -> ((dr: DataRetrieve) => backend.getNiClaimValidation(dr, request)),
    "caseflow"         -> ((dr: DataRetrieve) => backend.getCaseflowCaseDetails(dr, request)),
    "FTA emails"       -> ((dr: DataRetrieve) => backend.ftaManageEmails(dr, request)),
    "employments"      -> ((dr: DataRetrieve) => backend.getEmployments(dr, request)),
    "HIP agent"        -> ((dr: DataRetrieve) => backend.getHipAgentDetails(dr, request)),
    "DES organisation" -> ((dr: DataRetrieve) => backend.getDesOrganisation(dr, request))
  )

  backendMethods.foreach { case (name, retrieve) =>
    List(false, true).foreach { manualFallback =>
      List(
        Attr.FromObject(
          List(
            AttributeInstruction(
              DataRetrieve.Attribute("name"),
              ConstructAttribute.AsIs(Fetch(List("success", "name")))
            )
          )
        ),
        Attr.FromArray(Nil)
      ).foreach { attributes =>
        it should s"preserve $name not found as empty $attributes with manual fallback $manualFallback" in {
          stubFor(any(urlEqualTo("/retrieve")).willReturn(notFound().withBody("""{"NotFound":{}}""")))

          val dr = dataRetrieve.copy(attributes = attributes, manualFallbackIsDesigned = manualFallback)
          retrieve(dr).futureValue shouldBe dr.emptyValidResponse()
        }
      }

      List(500, 503).foreach { status =>
        it should s"return cannot retrieve for $name HTTP $status with manual fallback $manualFallback" in {
          stubFor(any(urlEqualTo("/retrieve")).willReturn(aResponse().withStatus(status).withBody("{}")))

          retrieve(dataRetrieve.copy(manualFallbackIsDesigned = manualFallback)).futureValue shouldBe
            CannotRetrieveResponse
        }
      }
    }

    it should s"preserve successful empty data for $name" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(okJson("{}")))

      retrieve(dataRetrieve).futureValue shouldBe ServiceResponse(DataRetrieve.Response.Object(Map.empty))
    }

    List("not json", "{}").foreach { body =>
      it should s"return cannot retrieve for invalid $name successful JSON: $body" in {
        stubFor(any(urlEqualTo("/retrieve")).willReturn(ok(body)))

        retrieve(dataRetrieve.copy(attributes = Attr.FromArray(Nil))).futureValue shouldBe CannotRetrieveResponse
      }
    }

    it should s"return cannot retrieve when the $name connection fails" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(aResponse().withFault(Fault.CONNECTION_RESET_BY_PEER)))

      retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
    }
  }

  List(
    "HIP agent"        -> ((dr: DataRetrieve) => backend.getHipAgentDetails(dr, request)),
    "DES organisation" -> ((dr: DataRetrieve) => backend.getDesOrganisation(dr, request))
  ).foreach { case (name, retrieve) =>
    it should s"preserve the legacy wrapped NotFound attribute shape for $name" in {
      stubFor(any(urlEqualTo("/retrieve")).willReturn(okJson("""{"NotFound":{}}""")))
      val dr = dataRetrieve.copy(attributes =
        Attr.FromObject(
          List(
            AttributeInstruction(
              DataRetrieve.Attribute("name"),
              ConstructAttribute.AsIs(Fetch(List("success", "name")))
            )
          )
        )
      )

      retrieve(dr).futureValue shouldBe dr.emptyValidResponse()
    }
  }

  it should "preserve a caseflow case ID validation error as a business result" in {
    stubFor(get(urlEqualTo("/retrieve")).willReturn(badRequest().withBody("path.caseId failed validation")))

    val dr = dataRetrieve.copy(attributes =
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("error"),
            ConstructAttribute.AsIs(Fetch(List("error")))
          )
        )
      )
    )
    backend.getCaseflowCaseDetails(dr, request).futureValue shouldBe
      ServiceResponse(DataRetrieve.Response.Object(Map(DataRetrieve.Attribute("error") -> "CASE_ID_VALIDATION_ERROR")))
  }

  List(400, 404).foreach { status =>
    it should s"preserve empty email details for an FTA emails error wrapping HTTP $status" in {
      stubFor(get(urlEqualTo("/retrieve")).willReturn(serverError().withBody(s"with statusCode: $status")))

      val dr = dataRetrieve.copy(attributes =
        Attr.FromArray(
          List(
            AttributeInstruction(
              DataRetrieve.Attribute("primaryEmail"),
              ConstructAttribute.AsIs(Fetch(List("primaryEmail")))
            )
          )
        )
      )
      backend.ftaManageEmails(dr, request).futureValue shouldBe
        ServiceResponse(DataRetrieve.Response.Array(List(Map(DataRetrieve.Attribute("primaryEmail") -> ""))))
    }
  }

  private val barsMethods: List[(String, Retrieve)] = List(
    "bank details"     -> ((dr: DataRetrieve) => bars.validateBankDetails(dr, request)),
    "business account" -> ((dr: DataRetrieve) => bars.businessBankAccountExistence(dr, request)),
    "personal account" -> ((dr: DataRetrieve) => bars.personalBankAccountExistence(dr, request))
  )

  barsMethods.foreach { case (name, retrieve) =>
    it should s"preserve the business validation result for a $name sort code deny list error" in {
      stubFor(post(urlEqualTo("/retrieve")).willReturn(badRequest().withBody("""{"code":"SORT_CODE_ON_DENY_LIST"}""")))

      val dr = dataRetrieve.copy(attributes =
        Attr.FromObject(
          List(
            AttributeInstruction(
              DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
              ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD")))
            )
          )
        )
      )
      retrieve(dr).futureValue shouldBe
        ServiceResponse(DataRetrieve.Response.Object(Map(DataRetrieve.Attribute("sortCodeIsPresentOnEISCD") -> "no")))
    }

    List(400, 404, 500, 503).foreach { status =>
      it should s"return cannot retrieve for an unrelated $name HTTP $status" in {
        stubFor(post(urlEqualTo("/retrieve")).willReturn(aResponse().withStatus(status).withBody("{}")))

        retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
      }
    }
  }

  List(false, true).foreach { manualFallback =>
    List(404, 503).foreach { status =>
      it should s"classify agentDetails HTTP $status through the service with manual fallback $manualFallback" in {
        stubFor(get(urlEqualTo("/retrieve")).willReturn(aResponse().withStatus(status).withBody("""{"NotFound":{}}""")))
        val dr = dataRetrieve.copy(
          tpe = DataRetrieve.Type("agentDetails"),
          attributes = Attr.FromObject(
            List(
              AttributeInstruction(
                DataRetrieve.Attribute("agencyName"),
                ConstructAttribute.AsIs(Fetch(List("det", "success", "name")))
              )
            )
          ),
          manualFallbackIsDesigned = manualFallback
        )
        val result = DataRetrieveService.retrieveDataResult(
          dr,
          None,
          request,
          None,
          None,
          None,
          None,
          Some(backend),
          None,
          None
        )

        if (status == 503 && !manualFallback) {
          result.failed.futureValue.getMessage shouldBe "Cannot retrieve data"
        } else {
          val retrieved = result.futureValue.value
          retrieved.data shouldBe RetrieveDataType.ObjectType(Map(DataRetrieve.Attribute("agencyName") -> ""))
          retrieved.hasFailed shouldBe (status == 503)
        }
      }
    }
  }

  it should "return cannot retrieve for NINO insights not found" in {
    stubFor(post(urlEqualTo("/retrieve")).willReturn(notFound()))

    nino.insights(dataRetrieve, request).futureValue shouldBe CannotRetrieveResponse
  }
}
