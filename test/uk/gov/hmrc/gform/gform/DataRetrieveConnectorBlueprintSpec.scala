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
    "GET"  -> ((dr: DataRetrieve) => connector.get(dr, request)),
    "POST" -> ((dr: DataRetrieve) => connector.post(dr, request))
  )

  methods.foreach { case (method, retrieve) =>
    behavior of s"DataRetrieveConnectorBlueprint $method"

    List(201, 204, 302, 400, 401, 403, 404, 422, 500, 503).foreach { status =>
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
    "employments"      -> ((dr: DataRetrieve) => backend.getEmployments(dr, request))
  )

  backendMethods.foreach { case (name, retrieve) =>
    it should s"return cannot retrieve for $name not found" in {
      stubFor(get(urlEqualTo("/retrieve")).willReturn(notFound()))

      retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
    }

    it should s"preserve successful empty data for $name" in {
      stubFor(get(urlEqualTo("/retrieve")).willReturn(okJson("{}")))

      retrieve(dataRetrieve).futureValue shouldBe ServiceResponse(DataRetrieve.Response.Object(Map.empty))
    }
  }

  it should "return cannot retrieve for a caseflow case ID validation error" in {
    stubFor(get(urlEqualTo("/retrieve")).willReturn(badRequest().withBody("path.caseId failed validation")))

    backend.getCaseflowCaseDetails(dataRetrieve, request).futureValue shouldBe CannotRetrieveResponse
  }

  List(400, 404).foreach { status =>
    it should s"return cannot retrieve for an FTA emails error wrapping HTTP $status" in {
      stubFor(get(urlEqualTo("/retrieve")).willReturn(serverError().withBody(s"with statusCode: $status")))

      backend.ftaManageEmails(dataRetrieve, request).futureValue shouldBe CannotRetrieveResponse
    }
  }

  private val barsMethods: List[(String, Retrieve)] = List(
    "bank details"     -> ((dr: DataRetrieve) => bars.validateBankDetails(dr, request)),
    "business account" -> ((dr: DataRetrieve) => bars.businessBankAccountExistence(dr, request)),
    "personal account" -> ((dr: DataRetrieve) => bars.personalBankAccountExistence(dr, request))
  )

  barsMethods.foreach { case (name, retrieve) =>
    it should s"return cannot retrieve for a $name sort code deny list error" in {
      stubFor(post(urlEqualTo("/retrieve")).willReturn(badRequest().withBody("""{"code":"SORT_CODE_ON_DENY_LIST"}""")))

      retrieve(dataRetrieve).futureValue shouldBe CannotRetrieveResponse
    }
  }

  it should "return cannot retrieve for NINO insights not found" in {
    stubFor(post(urlEqualTo("/retrieve")).willReturn(notFound()))

    nino.insights(dataRetrieve, request).futureValue shouldBe CannotRetrieveResponse
  }
}
