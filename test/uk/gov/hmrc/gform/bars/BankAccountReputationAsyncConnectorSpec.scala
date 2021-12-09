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

package uk.gov.hmrc.gform.bars

import akka.actor.ActorSystem
import com.github.tomakehurst.wiremock.client.WireMock
import com.github.tomakehurst.wiremock.client.WireMock._
import com.typesafe.config.{ Config, ConfigFactory }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.test.WsTestClient.InternalWSClient
import uk.gov.hmrc.gform.WiremockSupport
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.hooks.HttpHook
import uk.gov.hmrc.http.{ HeaderCarrier, RequestId, UpstreamErrorResponse }

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

  val wsHttp = new WSHttp {
    override protected def actorSystem: ActorSystem = system
    override protected def configuration: Config = ConfigFactory.parseString("""
                                                                               |internalServiceHostPatterns = []
                                                                               |bootstrap.http.headersAllowlist = []
                                                                               |http-verbs.retries.intervals = []
                                                                               |""".stripMargin)
    override val hooks: Seq[HttpHook] = Seq.empty
    override protected def wsClient: WSClient = new InternalWSClient("http", wiremockPort)
  }

  val bankAccountReputationAsyncConnector = new BankAccountReputationAsyncConnector(wsHttp, url)

  trait TestFixture {
    val validateResponse = ValidateBankDetails.Response(
      "yes",
      "yes",
      "yes",
      Some("yes"),
      Some("yes"),
      Some("yes"),
      Some("yes"),
      Some("111111"),
      Some("Some Bank")
    )
  }

  "validateBankDetails" should "call the validate bank details service endpoint with the given request" in new TestFixture {

    stubFor(
      WireMock
        .post(s"/v2/validateBankDetails")
        .withHeader("User-Agent", equalTo("gforms"))
        .withHeader("Content-Type", equalTo("application/json"))
        .withHeader("X-Tracking-Id", equalTo("some-tracking-id"))
        .willReturn(
          ok(
            Json
              .toJson(
                validateResponse
              )
              .toString()
          )
        )
    )

    val future = bankAccountReputationAsyncConnector.validateBankDetails(
      ValidateBankDetails.create(
        "112233",
        "12345678"
      )
    )

    whenReady(future) { response =>
      response shouldBe validateResponse
    }
  }

  it should "return BadRequest error" in new TestFixture {
    stubFor(
      WireMock
        .post(s"/v2/validateBankDetails")
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
      ValidateBankDetails.create("", "")
    )

    whenReady(future.failed) {
      case UpstreamErrorResponse(message, statusCode, _, _) =>
        message shouldBe
          s"""POST of '$url/v2/validateBankDetails' returned 400. Response body: '
             |{
             |    "code": "INVALID_SORTCODE",
             |    "desc": ": invalid sortcode"
             |}
             |'""".stripMargin
        statusCode shouldBe 400
      case other => fail(other)
    }
  }
}
