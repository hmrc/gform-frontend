import Resolvers._
import Dependencies._
import TestPhases.oneForkedJvmPerTest2
import uk.gov.hmrc.DefaultBuildSettings.{ addTestReportOption, defaultSettings, scalaSettings }
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)

lazy val playSettings: Seq[Setting[_]] = Seq.empty

majorVersion := 0
PlayKeys.playDefaultPort := 9195
playSettings
scalaSettings
publishingSettings
defaultSettings()

scalafmtOnCompile := true
scalaVersion := "2.11.12"
libraryDependencies ++= appDependencies
evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false)
routesImport ++= Seq(
  "uk.gov.hmrc.gform.binders.ValueClassBinder._",
  "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
  "uk.gov.hmrc.gform.sharedmodel.form._",
  "uk.gov.hmrc.gform.sharedmodel._",
  "uk.gov.hmrc.gform.auth._",
  "uk.gov.hmrc.gform.models._"
)

//TODO enable it and fix errors in code!!!
//      scalacOptions ++= Seq(
//        "-Xfatal-warnings",
//        "-Xlint:-missing-interpolator,_",
//        "-Yno-adapted-args",
//        "-Ywarn-numeric-widen",
//        "-Ywarn-value-discard",
//        "-Ywarn-dead-code",
//        "-deprecation",
//        "-feature",
//        "-unchecked"
//      )

configs(IntegrationTest)
inConfig(IntegrationTest)(Defaults.itSettings)

Keys.fork in IntegrationTest := false
unmanagedSourceDirectories in IntegrationTest <<= (baseDirectory in IntegrationTest)(base => Seq(base / "it"))
addTestReportOption(IntegrationTest, "int-test-reports")
testGrouping in IntegrationTest := oneForkedJvmPerTest2((definedTests in IntegrationTest).value)
parallelExecution in IntegrationTest := false

resolvers ++= Seq(
  bintrayRepo,
  jcenterRepo
)
