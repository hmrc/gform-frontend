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
    "uk.gov.hmrc" %% "frontend-bootstrap" % "7.26.0",
    "uk.gov.hmrc" %% "play-auth" % "2.2.0",
    "uk.gov.hmrc" %% "domain" % "4.1.0",
    "uk.gov.hmrc" %% "play-partials" % "5.4.0",
    "uk.gov.hmrc" %% "play-config" % "4.3.0",
    "uk.gov.hmrc" %% "logback-json-logger" % "3.1.0",
    "uk.gov.hmrc" %% "govuk-template" % "5.6.0",
    "uk.gov.hmrc" %% "play-health" % "2.1.0",
    "org.julienrf" %% "play-json-derived-codecs" % "3.3",
    "uk.gov.hmrc" %% "play-ui" % "6.1.0",
    "org.typelevel" %% "cats" % "0.9.0",
    "com.github.pureconfig" %% "pureconfig" % "0.7.2",
    "org.jetbrains" % "markdown" % "0.1.25",
    "com.chuusai" %% "shapeless" % "2.3.2",
    "uk.gov.hmrc" %% "http-caching-client" % "6.3.0",
    "uk.gov.hmrc" %% "emailaddress" % "2.1.0"
  )

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc" %% "hmrctest" % "2.3.0" % scope,
    "org.scalatest" %% "scalatest" % "3.0.4" % scope,
    "org.pegdown" % "pegdown" % "1.6.0" % scope,
    "org.jsoup" % "jsoup" % "1.9.2" % scope,
    "com.ironcorelabs" %% "cats-scalatest" % "2.2.0" % scope,
//    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope
    "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1" % scope,
    "org.mockito" % "mockito-all" % "1.10.19" % scope
  )

}
