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

package uk.gov.hmrc.gform.it

import org.apache.pekko.actor.ActorSystem
import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.configureFor
import com.github.tomakehurst.wiremock.core.WireMockConfiguration.options
import com.typesafe.config.ConfigFactory
import org.scalatest.{ BeforeAndAfterAll, BeforeAndAfterEach, GivenWhenThen }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.play.{ BaseOneServerPerSuite, FakeApplicationFactory }
import play.api.ApplicationLoader.Context
import play.api.libs.json.{ Json, Reads }
import play.api.libs.ws.ahc.{ AhcWSClientConfigFactory, StandaloneAhcWSClient }
import play.api.{ Application, Environment }
import uk.gov.hmrc.crypto.{ Crypted, SymmetricCryptoFactory }
import uk.gov.hmrc.gform.{ ApplicationLoader, MongoDBSupport }

import scala.jdk.CollectionConverters._
import scala.util.Random

trait ITSpec
    extends HTTPSupport with GivenWhenThen with AnyFlatSpecLike with Matchers with BaseOneServerPerSuite
    with BeforeAndAfterAll with FakeApplicationFactory with ScalaFutures with BeforeAndAfterEach
    with WiremockAdminSupport with DocumentSupport with MongoDBSupport {

  implicit val system: ActorSystem = ActorSystem()

  val wiremockPort: Int = 10000 + Random.nextInt(10000)
  implicit val wireMockServer: WireMockServer = new WireMockServer(options().port(wiremockPort))

  val settingsOverride: Map[String, String] = Map(
    "auditing.enabled"                                 -> "false",
    "metrics.enabled"                                  -> "false",
    "json.encryption.key"                              -> "fqpLDZ4sumDsekHkeEBlCA==",
    "json.encryption.previousKeys"                     -> "",
    "microservice.services.auth.port"                  -> s"$wiremockPort",
    "microservice.services.file-upload.port"           -> s"$wiremockPort",
    "microservice.services.file-upload.path-prefix"    -> "",
    "microservice.services.gform.port"                 -> s"$wiremockPort",
    "microservice.services.email.port"                 -> s"$wiremockPort",
    "microservice.services.gg.port"                    -> s"$wiremockPort",
    "microservice.services.save4later.port"            -> s"$wiremockPort",
    "microservice.services.tax-enrolments.port"        -> s"$wiremockPort",
    "microservice.services.pdf-generator.port"         -> s"$wiremockPort",
    "microservice.services.csp-partials.port"          -> s"$wiremockPort",
    "microservice.services.seiss.port"                 -> s"$wiremockPort",
    "microservice.services.enrolment-store-proxy.port" -> s"$wiremockPort"
  ) ++ mongoSettings

  override def fakeApplication(): Application = {
    val context =
      Context.create(environment = Environment.simple(), initialSettings = settingsOverride)
    new ApplicationLoader().load(context)
  }

  override protected def beforeAll(): Unit = {
    wireMockServer.start()
    configureFor("localhost", wiremockPort)
  }

  override protected def beforeEach(): Unit =
    wireMockServer.resetAll()

  override protected def afterAll(): Unit = {
    app.stop().futureValue
    wireMockServer.stop()
    mongoComponent.database.drop().toFuture().futureValue
    system.terminate()
    ()
  }

  def decryptAs[T: Reads](json: String) =
    Json.parse(jsonCrypto.decrypt(Crypted(Json.parse(json).toString())).value).as[T]

  implicit lazy val baseUrl: String = s"http://localhost:$port"

  protected def buildWSClient: StandaloneAhcWSClient =
    StandaloneAhcWSClient(config = AhcWSClientConfigFactory.forConfig().copy(useCookieStore = true))

  val jsonCrypto =
    SymmetricCryptoFactory.aesCryptoFromConfig(
      baseConfigKey = "json.encryption",
      ConfigFactory.parseMap(settingsOverride.asJava)
    )
}
