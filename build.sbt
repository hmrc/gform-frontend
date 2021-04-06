import Dependencies.appDependencies
import play.sbt.routes.RoutesKeys.routesImport
import sbt.inConfig
import uk.gov.hmrc.DefaultBuildSettings._
import uk.gov.hmrc.SbtAutoBuildPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin.publishingSettings
import uk.gov.hmrc.versioning.SbtGitVersioning
import uk.gov.hmrc.versioning.SbtGitVersioning.autoImport.majorVersion
import org.irundaia.sbt.sass._

val silencerVersion = "1.7.0"

lazy val IntegrationTest = config("it") extend Test

lazy val microservice = (project in file("."))
  .enablePlugins(
    play.sbt.PlayScala,
    SbtAutoBuildPlugin,
    SbtGitVersioning,
    SbtDistributablesPlugin,
    SbtArtifactory,
    SbtWeb
  )
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
    // Although Playframework sets Test / fork to true: https://github.com/playframework/playframework/blob/d09e2f0b1c7c211e8d00b926919338a72563bcd9/dev-mode/sbt-plugin/src/main/scala/play/sbt/PlaySettings.scala#L77
    // Hmrc sbt-settings overrides Test / fork to false: https://github.com/hmrc/sbt-settings/blob/80fa3dcb6d6fedd2917d7d492646dd0f380ad421/src/main/scala/uk/gov/hmrc/DefaultBuildSettings.scala#L65
    // But when it set to false, some tests start to fail while on sbt 1.4.x
    // Let's set it back to play framework default.
    Test / fork := true,
    scalafmtOnCompile := true,
    scalaVersion := "2.12.12",
    Test / testOptions := (Test / testOptions).value
      .map {
        // Default Argument added by https://github.com/hmrc/sbt-settings
        // are clashing with munit arguments, so we scope them to ScalaTest instead.
        case sbt.Tests.Argument(None, args) => sbt.Tests.Argument(Some(TestFrameworks.ScalaTest), args)
        case otherwise                      => otherwise
      }
      .toSet
      .toSeq, // get rid of duplicates
    libraryDependencies ++= appDependencies,
    libraryDependencies ++= Seq(
      compilerPlugin("com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full)
    ),
    routesImport ++= Seq(
      "uk.gov.hmrc.gform.binders.ValueClassBinder._",
      "uk.gov.hmrc.gform.controllers.Direction",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
      "uk.gov.hmrc.gform.models.ids._",
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
      //"uk.gov.hmrc.csp.WebchatClient",
      "uk.gov.hmrc.govukfrontend.views.html.components._",
      "uk.gov.hmrc.govukfrontend.views.html.helpers._"
    ),
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Xlint:-missing-interpolator,_",
      "-Yno-adapted-args",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-dead-code",
      "-deprecation",
      "-feature",
      "-unchecked",
      // silence all warnings on autogenerated files
      "-P:silencer:pathFilters=target/.*",
      // Make sure you only exclude warnings for the project directories, i.e. make builds reproducible
      s"-P:silencer:sourceRoots=${baseDirectory.value.getCanonicalPath}"
    ),
    pipelineStages := Seq(digest),
    Assets / pipelineStages := Seq(concat, uglify),
    uglifyCompressOptions := Seq("warnings=false")
  )
  .configs(IntegrationTest)
  .settings(
    inConfig(IntegrationTest)(Defaults.itSettings),
    inConfig(IntegrationTest)(ScalafmtPlugin.scalafmtConfigSettings),
    IntegrationTest / Keys.fork := false,
    IntegrationTest / unmanagedSourceDirectories := (IntegrationTest / baseDirectory)(base => Seq(base / "it")).value,
    addTestReportOption(IntegrationTest, "int-test-reports"),
    IntegrationTest / parallelExecution := false,
    scalafmtOnCompile := true
  )
  .settings(
    resolvers ++= Seq(
      Resolver.bintrayRepo("jetbrains", "markdown"),
      Resolver.jcenterRepo,
      "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
      "hmrc-releases" at "https://artefacts.tax.service.gov.uk/artifactory/hmrc-releases/",
      "bintray" at "https://dl.bintray.com/webjars/maven"
    )
  )
