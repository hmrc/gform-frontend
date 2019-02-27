import Dependencies.appDependencies
import play.sbt.routes.RoutesKeys.routesImport
import sbt.Tests.{ Group, SubProcess }
import sbt.inConfig
import uk.gov.hmrc.DefaultBuildSettings._
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
import uk.gov.hmrc.versioning.SbtGitVersioning
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion

coverageEnabled := true

lazy val scoverageSettings = {
  import scoverage.ScoverageKeys
  Seq(
    // Semicolon-separated list of regexs matching classes to exclude
    ScoverageKeys.coverageExcludedPackages := """uk.gov.hmrc.BuildInfo;._.Routes;._.RoutesPrefix;._Filters?;MicroserviceAuditConnector;Module;GraphiteStartUp;._.Reverse[^.]*""",
    ScoverageKeys.coverageMinimum := 80.00,
    ScoverageKeys.coverageFailOnMinimum := false,
    ScoverageKeys.coverageHighlighting := true,
    parallelExecution in Test := false
  )
}

lazy val microservice = (project in file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .settings(
    organization := "uk.gov.hmrc",
    name := "gform-frontend",
    majorVersion := 0,
    PlayKeys.playDefaultPort := 9195,
    scoverageSettings,
    scalaSettings,
    publishingSettings,
    defaultSettings(),
    scalafmtOnCompile := true,
    scalaVersion := "2.11.12",
    libraryDependencies ++= appDependencies,
    evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false),
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
  )
  .configs(IntegrationTest)
  .settings(
    inConfig(IntegrationTest)(Defaults.itSettings),
    Keys.fork in IntegrationTest := false,
    unmanagedSourceDirectories in IntegrationTest := { (baseDirectory in IntegrationTest)(base => Seq(base / "it")) }.value,
    addTestReportOption(IntegrationTest, "int-test-reports"),
    testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
    parallelExecution in IntegrationTest := false
  )
  .settings(
    resolvers ++= Seq(
      Resolver.bintrayRepo("jetbrains", "markdown"),
      Resolver.jcenterRepo,
      "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven"
    ))

def oneForkedJvmPerTest(tests: Seq[TestDefinition]) =
  tests map { test =>
    Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
  }
