import sbt._
import play.sbt.PlayImport._
import play.core.PlayVersion
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning

object FrontendBuild extends Build with MicroService {

  val appName = "gform-frontend"

  override lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "frontend-bootstrap" % "12.4.0",
    "uk.gov.hmrc" %% "auth-client" % "2.19.0-play-25",
    "uk.gov.hmrc" %% "domain" % "5.3.0",
    "uk.gov.hmrc" %% "play-partials" % "6.5.0",
    "com.codecommit" %% "parseback-core" % parsebackVersion,
    "com.codecommit" %% "parseback-cats" % parsebackVersion,
    "org.julienrf" %% "play-json-derived-codecs" % "5.0.0",
    "org.typelevel" %% "cats-core" % "1.6.0",
    "org.typelevel" %% "cats-mtl-core" % "0.5.0" exclude("org.scalacheck", "scalacheck_2.11"),
    "com.github.pureconfig" %% "pureconfig" % "0.10.2",
    "org.jetbrains" % "markdown" % "0.1.31",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "uk.gov.hmrc" %% "http-caching-client" % "8.1.0",
    "uk.gov.hmrc" %% "emailaddress" % "3.2.0",
    "org.scala-graph" %% "graph-core" % "1.12.5"
  )

    def test(scope: String = "test") = Seq(
      "uk.gov.hmrc" %% "hmrctest" % "3.5.0-play-25" % scope,
      "org.scalatest" %% "scalatest" % "3.0.5" % scope,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % scope,
      "org.pegdown" % "pegdown" % "1.6.0" % scope,
      "com.ironcorelabs" %% "cats-scalatest" % "2.4.0" % scope,
      "com.typesafe.play" %% "play-test" % PlayVersion.current % scope,
      "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.1" % scope,
      "org.mockito" % "mockito-all" % "1.10.19" % scope,
      "org.jsoup" % "jsoup" % "1.11.3"
    )
}
