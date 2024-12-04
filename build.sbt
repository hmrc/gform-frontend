import Dependencies.appDependencies
import play.sbt.routes.RoutesKeys.routesImport
import uk.gov.hmrc.{ DefaultBuildSettings, SbtAutoBuildPlugin }
import uk.gov.hmrc.sbtdistributables.SbtDistributablesPlugin
import uk.gov.hmrc.versioning.SbtGitVersioning
import org.irundaia.sbt.sass.*

Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val IntegrationTest = config("it") extend Test

ThisBuild / majorVersion := 0
ThisBuild / scalaVersion := "2.13.12"

lazy val microservice = (project in file("."))
  .enablePlugins(
    play.sbt.PlayScala,
    SbtAutoBuildPlugin,
    SbtGitVersioning,
    SbtDistributablesPlugin,
    SbtWeb,
    BuildInfoPlugin
  )
  .settings(
    SassKeys.cssStyle := Maxified,
    SassKeys.generateSourceMaps := true,
    organization := "uk.gov.hmrc",
    name := "gform-frontend",
    PlayKeys.playDefaultPort := 9195,
    PlayKeys.playRunHooks += Parcel(baseDirectory.value),
    DefaultBuildSettings.scalaSettings,
    ParcelBuild.parcelBundleSetting,
    DefaultBuildSettings.defaultSettings(),
    scalafmtOnCompile := true,
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
    routesImport ++= Seq(
      "uk.gov.hmrc.gform.binders.ValueClassBinder._",
      "uk.gov.hmrc.gform.controllers.Direction",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations._",
      "uk.gov.hmrc.gform.sharedmodel.formtemplate._",
      "uk.gov.hmrc.gform.models.ids._",
      "uk.gov.hmrc.gform.sharedmodel.form._",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.auth._",
      "uk.gov.hmrc.gform.models._",
      "uk.gov.hmrc.gform.upscan.UpscanReference",
      "uk.gov.hmrc.gform.testonly.snapshot.SnapshotId",
      "uk.gov.hmrc.gform.testonly.snapshot.UserInputs",
      "uk.gov.hmrc.play.bootstrap.binders.RedirectUrl",
      "uk.gov.hmrc.gform.payment.PaymentReference"
    ),
    TwirlKeys.templateImports ++= Seq(
      "play.twirl.api.Html",
      "play.twirl.api.HtmlFormat",
      "uk.gov.hmrc.gform.sharedmodel._",
      "uk.gov.hmrc.gform.eval.smartstring._",
      //"uk.gov.hmrc.csp.WebchatClient",
      "uk.gov.hmrc.govukfrontend.views.html.components._",
      "views.html.helper.CSPNonce"
    ),
    scalacOptions ++= Seq(
      "-Xfatal-warnings",
      "-Xlint:-missing-interpolator,_",
      "-Xlint:-byname-implicit",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Ywarn-dead-code",
      "-deprecation",
      "-feature",
      "-unchecked",
      // silence all warnings on autogenerated files and
      // twirl templates. (Not all twirl templates use all TwirlKeys.templateImports)
      "-Wconf:src=twirl/.*:silent,src=routes/.*:silent",
      "-Wconf:cat=lint-multiarg-infix:silent"
    ),
    uglifyOps := UglifyOps.singleFile,
    uglify / excludeFilter ~= { _ || "builder.js" },
    pipelineStages := Seq(digest),
    Assets / pipelineStages := Seq(concat, uglify),
    Assets / unmanagedResourceDirectories += baseDirectory.value / "builder" / "dist",
    uglifyCompressOptions := Seq("warnings=false")
  )
  .settings(
    resolvers ++= Seq(
      Resolver.bintrayRepo("jetbrains", "markdown"),
      Resolver.jcenterRepo,
      "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",
      "bintray" at "https://dl.bintray.com/webjars/maven"
    )
  )
  .settings(
    buildInfoKeys := Seq[BuildInfoKey](version),
    buildInfoPackage := "uk.gov.hmrc.gform"
  )

lazy val it = project
  .enablePlugins(PlayScala)
  .dependsOn(microservice % "test->test") // the "test->test" allows reusing test code and test dependencies
  .settings(
    DefaultBuildSettings.itSettings(),
    Keys.fork := false,
    DefaultBuildSettings.addTestReportOption(IntegrationTest, "int-test-reports"),
    scalafmtOnCompile := true
  )
  .settings(libraryDependencies ++= Dependencies.test)
