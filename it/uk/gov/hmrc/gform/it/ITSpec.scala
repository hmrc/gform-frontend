package uk.gov.hmrc.gform.it

import akka.actor.ActorSystem
import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.`extension`.responsetemplating.ResponseTemplateTransformer
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
import uk.gov.hmrc.crypto.{ Crypted, CryptoWithKeysFromConfig }
import uk.gov.hmrc.gform.ApplicationLoader

import scala.collection.JavaConverters._
import scala.util.Random

trait ITSpec
    extends HTTPSupport with GivenWhenThen with AnyFlatSpecLike with Matchers with BaseOneServerPerSuite
    with BeforeAndAfterAll with FakeApplicationFactory with ScalaFutures with BeforeAndAfterEach
    with WiremockAdminSupport with DocumentSupport {

  implicit val system: ActorSystem = ActorSystem()

  val wiremockPort: Int = 10000 + Random.nextInt(10000)
  implicit val wireMockServer: WireMockServer = new WireMockServer(
    options().port(wiremockPort).extensions(new ResponseTemplateTransformer(false))
  )

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
  )

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
    system.terminate()
    ()
  }

  def decryptAs[T: Reads](json: String) =
    Json.parse(jsonCrypto.decrypt(Crypted(Json.parse(json).toString())).value).as[T]

  implicit lazy val baseUrl: String = s"http://localhost:$port"

  protected def buildWSClient: StandaloneAhcWSClient =
    StandaloneAhcWSClient(config = AhcWSClientConfigFactory.forConfig().copy(useCookieStore = true))

  val jsonCrypto =
    new CryptoWithKeysFromConfig(baseConfigKey = "json.encryption", ConfigFactory.parseMap(settingsOverride.asJava))
}
