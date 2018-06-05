import sbt._
import play.sbt.PlayImport._
import play.core.PlayVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning

object FrontendBuild extends Build with MicroService {

  val appName = "gform-frontend"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "frontend-bootstrap" % "8.11.0",
    "uk.gov.hmrc" %% "auth-client" % "2.3.0",
    "uk.gov.hmrc" %% "domain" % "5.0.0",
    "uk.gov.hmrc" %% "play-partials" % "6.1.0",
    "org.julienrf" %% "play-json-derived-codecs" % "3.3",
    "org.typelevel" %% "cats-core" % "1.1.0",
    "com.github.pureconfig" %% "pureconfig" % "0.8.0",
    "org.jetbrains" % "markdown" % "0.1.25",
    "com.chuusai" %% "shapeless" % "2.3.2",
    "uk.gov.hmrc" %% "http-caching-client" % "7.0.0",
    "uk.gov.hmrc" %% "emailaddress" % "2.2.0",
    "org.jsoup" % "jsoup" % "1.10.3"
  )

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "3.0.0" % scope,
    "org.scalatest" %% "scalatest" % "3.0.5" % scope,
    "org.scalacheck" %% "scalacheck" % "1.14.0" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "com.ironcorelabs" %% "cats-scalatest" % "2.3.1" % scope,
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "org.mockito" % "mockito-all" % "1.10.19" % scope
  )

}
