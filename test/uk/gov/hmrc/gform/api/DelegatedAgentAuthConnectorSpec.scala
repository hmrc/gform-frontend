/*
 * Copyright 2025 HM Revenue & Customs
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
import com.github.tomakehurst.wiremock.client.WireMock._
import com.typesafe.config.{ Config, ConfigFactory }
import org.apache.pekko.actor.ActorSystem
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{ Millis, Seconds, Span }
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach }
import play.api.libs.json.Json
import play.api.libs.ws.WSClient
import play.api.test.WsTestClient.InternalWSClient
import play.api.{ Configuration, Environment }
import uk.gov.hmrc.gform.WiremockSupport
import uk.gov.hmrc.gform.auth.AuthConnector
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ Attribute, Response }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, FormCtx }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.hooks.HttpHook
import uk.gov.hmrc.http.{ Authorization, HeaderCarrier }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class DelegatedAgentAuthConnectorSpec
    extends AnyFlatSpec with Matchers with WiremockSupport with BeforeAndAfterEach with BeforeAndAfterAll
    with ScalaFutures {

  implicit val defaultPatience: PatienceConfig =
    PatienceConfig(timeout = Span(5, Seconds), interval = Span(500, Millis))

  implicit val system: ActorSystem = ActorSystem()
  implicit val hc: HeaderCarrier = HeaderCarrier(authorization = Some(Authorization("Bearer some-token-id")))

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

  val environment: Environment = Environment.simple()
  val configuration: Configuration = Configuration.load(environment)

  val authConnector: AuthConnector = new AuthConnector(s"$url", wsHttp, configuration)
  val delegatadAgentAuthConnector: DelegatedAgentAuthAsyncConnector = new DelegatedAgentAuthAsyncConnector(
    authConnector
  )

  "mtdVatAuth" should "return true when authorised for HMRC MTD VAT" in new TestFixture {
    stubFor(
      WireMock
        .post("/auth/authorise")
        .willReturn(ok("{}"))
    )

    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.mtdVatAuth(authVatDataRetrieve, mtdAuthRequest)

    whenReady(future) { response =>
      response shouldBe authorisedResponse
    }
  }

  it should "return false when NOT authorised for HMRC MTD VAT" in new TestFixture {
    stubFor(
      WireMock
        .post("/auth/authorise")
        .willReturn(unauthorized())
    )

    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.mtdVatAuth(authVatDataRetrieve, mtdAuthRequest)

    whenReady(future) { response =>
      response shouldBe unauthorisedResponse
    }
  }

  it should "return false when forbidden for HMRC MTD VAT" in new TestFixture {
    stubFor(
      WireMock
        .post("/auth/authorise")
        .willReturn(forbidden())
    )

    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.mtdVatAuth(authVatDataRetrieve, mtdAuthRequest)

    whenReady(future) { response =>
      response shouldBe unauthorisedResponse
    }
  }

  "epayeAuth" should "return true when authorised for IR-PAYE" in new TestFixture {
    stubFor(
      WireMock
        .post("/auth/authorise")
        .willReturn(ok("{}"))
    )

    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.payeAuth(authPayeDataRetrieve, validPayeRequest)

    whenReady(future) { response =>
      response shouldBe authorisedResponse
    }
  }

  it should "return false when NOT authorised for IR-PAYE" in new TestFixture {
    stubFor(
      WireMock
        .post("/auth/authorise")
        .willReturn(unauthorized())
    )

    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.payeAuth(authPayeDataRetrieve, validPayeRequest)

    whenReady(future) { response =>
      response shouldBe unauthorisedResponse
    }
  }

  it should "return false when forbidden for IR-PAYE" in new TestFixture {
    stubFor(
      WireMock
        .post("/auth/authorise")
        .willReturn(forbidden())
    )

    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.payeAuth(authPayeDataRetrieve, validPayeRequest)

    whenReady(future) { response =>
      response shouldBe unauthorisedResponse
    }
  }

  it should "return false when invalid request submitted" in new TestFixture {
    val future: Future[ServiceCallResponse[DataRetrieve.Response]] =
      delegatadAgentAuthConnector.payeAuth(authPayeDataRetrieve, invalidPayeRequest)

    whenReady(future) { response =>
      response shouldBe unauthorisedResponse
    }
  }

  trait TestFixture {
    val authVatDataRetrieve: DataRetrieve = DataRetrieve(
      DataRetrieve.Type("delegatedAgentAuthVat"),
      DataRetrieveId("agentAuthorisedForVRN"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("authorised"),
            ConstructAttribute.AsIs(Fetch(List("authorised")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("authorised") -> DataRetrieve.AttrType.String
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("vatRegistrationNumber", List.empty[String], DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("vrn"))
        )
      ),
      None
    )

    val authPayeDataRetrieve: DataRetrieve = DataRetrieve(
      DataRetrieve.Type("delegatedAgentAuthPaye"),
      DataRetrieveId("agentAuthorisedForPAYE"),
      Attr.FromObject(
        List(
          AttributeInstruction(
            DataRetrieve.Attribute("authorised"),
            ConstructAttribute.AsIs(Fetch(List("authorised")))
          )
        )
      ),
      Map(
        DataRetrieve.Attribute("authorised") -> DataRetrieve.AttrType.String
      ),
      List(
        DataRetrieve.ParamExpr(
          DataRetrieve.Parameter("payeReference", List.empty[String], DataRetrieve.ParamType.String),
          FormCtx(FormComponentId("payeRef"))
        )
      ),
      None
    )

    val mtdAuthRequest: DataRetrieve.Request = DataRetrieve.Request(
      Json.obj(),
      List(("vatRegistrationNumber", "123456"))
    )

    val validPayeRequest: DataRetrieve.Request = DataRetrieve.Request(
      Json.obj(),
      List(("payeReference", "123/AB456"))
    )

    val invalidPayeRequest: DataRetrieve.Request = DataRetrieve.Request(
      Json.obj(),
      List(("payeReference", "123456"))
    )

    val authorisedResponse: ServiceResponse[Response.Object] = ServiceResponse(
      DataRetrieve.Response.Object(Map(Attribute("authorised") -> "true"))
    )

    val unauthorisedResponse: ServiceResponse[Response.Object] = ServiceResponse(
      DataRetrieve.Response.Object(Map(Attribute("authorised") -> "false"))
    )
  }

}
