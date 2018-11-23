import sbt.Keys._
import sbt.Tests.{Group, SubProcess}
import sbt._
import play.routes.compiler.StaticRoutesGenerator
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin._
import play.sbt.PlayImport.PlayKeys
import com.lucidchart.sbt.scalafmt.ScalafmtCorePlugin.autoImport._
import uk.gov.hmrc.SbtArtifactory

trait MicroService {

  import uk.gov.hmrc._
  import DefaultBuildSettings._
  import uk.gov.hmrc.{SbtBuildInfo, ShellPrompt, SbtAutoBuildPlugin}
  import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
  import uk.gov.hmrc.versioning.SbtGitVersioning
  import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
  import play.sbt.routes.RoutesKeys.{routesGenerator, routesImport}


  import TestPhases._

  val appName: String

  lazy val appDependencies : Seq[ModuleID] = ???
  lazy val plugins : Seq[Plugins] = Seq.empty
  lazy val playSettings : Seq[Setting[_]] = Seq.empty


  lazy val microservice = Project(appName, file("."))
    .enablePlugins(Seq(play.sbt.PlayScala, SbtAutoBuildPlugin, SbtGitVersioning, SbtDistributablesPlugin, SbtArtifactory) ++ plugins : _*)
    .settings(majorVersion := 0)
    .settings(PlayKeys.playDefaultPort := 9195)
    .settings(playSettings : _*)
    .settings(scalaSettings: _*)
    .settings(publishingSettings: _*)
    .settings(defaultSettings(): _*)
    .settings(
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
    .settings(inConfig(IntegrationTest)(Defaults.itSettings): _*)
    .settings(
      Keys.fork in IntegrationTest := false,
      unmanagedSourceDirectories in IntegrationTest <<= (baseDirectory in IntegrationTest)(base => Seq(base / "it")),
      addTestReportOption(IntegrationTest, "int-test-reports"),
      testGrouping in IntegrationTest := oneForkedJvmPerTest2((definedTests in IntegrationTest).value),
      parallelExecution in IntegrationTest := false)
      .settings(resolvers ++= Seq(
        Resolver.bintrayRepo("jetbrains","markdown"),
        Resolver.jcenterRepo
      ))
}

private object TestPhases {

  def oneForkedJvmPerTest2(tests: Seq[TestDefinition]) =
    tests map {
      test => new Group(test.name, Seq(test), SubProcess(ForkOptions(runJVMOptions = Seq("-Dtest.name=" + test.name))))
    }
}
