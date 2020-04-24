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
import org.irundaia.sbt.sass._

lazy val microservice = (project in file("."))
  .enablePlugins(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory)
  .settings(
    SassKeys.cssStyle := Maxified,
    SassKeys.generateSourceMaps := true,
    organization := "uk.gov.hmrc",
    name := "gform-frontend",
    majorVersion := 0,
    PlayKeys.playDefaultPort := 9195,
    scalaSettings,
    publishingSettings,
    defaultSettings(),
    scalafmtOnCompile := true,
    scalaVersion := "2.11.12",
    libraryDependencies ++= appDependencies,
    evictionWarningOptions in update := EvictionWarningOptions.default.withWarnScalaVersionEviction(false),
    routesImport ++= Seq(
      "uk.gov.hmrc.gform.binders.ValueClassBinder._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
      "uk.gov.hmrc.gform.sharedmodel.form._",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.auth._",
      "uk.gov.hmrc.gform.models._"
    ),
    TwirlKeys.templateImports ++= Seq(
      "play.twirl.api.Html",
      "play.twirl.api.HtmlFormat",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.eval.smartstring._",
      "uk.gov.hmrc.csp.WebchatClient",
      "uk.gov.hmrc.gform.views.ViewHelpersAlgebra",
      "uk.gov.hmrc.govukfrontend.views.html.components._",
      "uk.gov.hmrc.govukfrontend.views.html.helpers._",
      "uk.gov.hmrc.hmrcfrontend.views.html.components._"
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
    inConfig(IntegrationTest)(scalafmtCoreSettings),
    Keys.fork in IntegrationTest := false,
    unmanagedSourceDirectories in IntegrationTest := { (baseDirectory in IntegrationTest)(base => Seq(base / "it")) }.value,
    addTestReportOption(IntegrationTest, "int-test-reports"),
    testGrouping in IntegrationTest := oneForkedJvmPerTest((definedTests in IntegrationTest).value),
    parallelExecution in IntegrationTest := false
  )
  .settings(resolvers ++= Seq(
    Resolver.bintrayRepo("jetbrains", "markdown"),
    Resolver.jcenterRepo,
    "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
    "hmrc-releases" at "https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases/",
    "bintray" at "https://dl.bintray.com/webjars/maven"
  ))

def oneForkedJvmPerTest(tests: Seq[TestDefinition]) =
  tests map { test =>
    Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
  }
