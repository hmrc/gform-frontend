import play.core.PlayVersion
import sbt._

object Dependencies {

  val jacksonVersion = "2.16.1"
  val jacksonDatabindVersion = "2.16.1"
  val circeVersion = "0.14.5"
  val bootstrapVersion = "8.2.0"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val compile = Seq(
    "uk.gov.hmrc"                 %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"                 %% "play-frontend-hmrc-play-30" % "9.1.0",
    "uk.gov.hmrc"                 %% "play-partials-play-30"      % "9.1.0",
    "uk.gov.hmrc"                 %% "domain-play-30"             % "9.0.0",
    "org.playframework"           %% "play-json-joda"             % "3.0.2",
    "org.julienrf"                %% "play-json-derived-codecs"   % "10.1.0",
    "org.typelevel"               %% "cats-core"                  % "2.9.0",
    "org.typelevel"               %% "cats-free"                  % "2.9.0",
    "org.typelevel"               %% "cats-mtl"                   % "1.3.0",
    "org.typelevel"               %% "case-insensitive"           % "1.3.0",
    "com.github.pureconfig"       %% "pureconfig"                 % "0.17.4",
    "org.jetbrains"                % "markdown"                   % "0.1.46",
    "com.chuusai"                 %% "shapeless"                  % "2.3.10",
    "uk.gov.hmrc"                 %% "emailaddress"               % "3.7.0",
    "org.scala-graph"             %% "graph-core"                 % "1.13.5",
    "com.softwaremill.quicklens"  %% "quicklens"                  % "1.9.0",
    "com.nrinaudo"                %% "kantan.csv"                 % "0.7.0",
    "com.miguelfonseca.completely" % "completely-core"            % "0.9.0",
    "org.jsoup"                    % "jsoup"                      % "1.15.4",
    "org.webjars.npm"              % "govuk-frontend"             % "5.2.0",
    "com.openhtmltopdf"            % "openhtmltopdf-pdfbox"       % "1.0.10",
    "ai.x"                        %% "play-json-extensions"       % "0.42.0",
    "org.apache.commons"           % "commons-text"               % "1.10.0",
    "com.dripower"                %% "play-circe"                 % "3014.1",
    "io.circe"                    %% "circe-core"                 % circeVersion,
    "io.circe"                    %% "circe-generic"              % circeVersion,
    "io.circe"                    %% "circe-parser"               % circeVersion,
    // Taken from: https://github.com/orgs/playframework/discussions/11222
    "com.fasterxml.jackson.core"       % "jackson-core"                   % jacksonVersion,
    "com.fasterxml.jackson.core"       % "jackson-annotations"            % jacksonVersion,
    "com.fasterxml.jackson.datatype"   % "jackson-datatype-jdk8"          % jacksonVersion,
    "com.fasterxml.jackson.datatype"   % "jackson-datatype-jsr310"        % jacksonVersion,
    "com.fasterxml.jackson.core"       % "jackson-databind"               % jacksonDatabindVersion,
    "com.fasterxml.jackson.dataformat" % "jackson-dataformat-cbor"        % jacksonVersion,
    "com.fasterxml.jackson.module"     % "jackson-module-parameter-names" % jacksonVersion,
    "com.fasterxml.jackson.module"    %% "jackson-module-scala"           % jacksonVersion
  )

  val munitVersion = "0.7.29"

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-30"  % bootstrapVersion    % scope,
    "org.scalatestplus"      %% "scalacheck-1-14"         % "3.2.2.0"           % scope,
    "org.pegdown"             % "pegdown"                 % "1.6.0"             % scope,
    "com.ironcorelabs"       %% "cats-scalatest"          % "3.1.1"             % scope,
    "org.playframework"      %% "play-test"               % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "7.0.1"             % scope,
    "org.mockito"            %% "mockito-scala-scalatest" % "1.17.30"           % scope,
    "com.github.tomakehurst"  % "wiremock-jre8"           % "3.0.1"             % scope,
    "org.scalameta"          %% "munit"                   % munitVersion        % scope,
    "org.scalameta"          %% "munit-scalacheck"        % munitVersion        % scope,
    "org.playframework"      %% "play-pekko-http-server"  % "3.0.1"             % scope,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.6"            % scope
  )
}
