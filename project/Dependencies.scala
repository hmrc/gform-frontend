import play.core.PlayVersion
import sbt.*

object Dependencies {

  val jacksonVersion = "2.18.3"
  val jacksonDatabindVersion = "2.18.3"
  val circeVersion = "0.14.12"
  val bootstrapVersion = "8.6.0"
  val hmrcMongoVersion = "2.6.0"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test

  val compile = Seq(
    "uk.gov.hmrc"                 %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"                 %% "play-frontend-hmrc-play-30" % "11.13.0",
    "uk.gov.hmrc.mongo"           %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "uk.gov.hmrc"                 %% "play-partials-play-30"      % "9.1.0",
    "uk.gov.hmrc"                 %% "domain-play-30"             % "9.0.0",
    "org.playframework"           %% "play-json-joda"             % "3.0.4",
    "org.julienrf"                %% "play-json-derived-codecs"   % "11.0.0",
    "org.typelevel"               %% "cats-core"                  % "2.13.0",
    "org.typelevel"               %% "cats-free"                  % "2.13.0",
    "org.typelevel"               %% "cats-mtl"                   % "1.5.0",
    "org.typelevel"               %% "case-insensitive"           % "1.4.2",
    "commons-codec"                % "commons-codec"              % "1.18.0",
    "com.github.pureconfig"       %% "pureconfig"                 % "0.17.8",
    "org.jetbrains"                % "markdown"                   % "0.1.46",
    "com.chuusai"                 %% "shapeless"                  % "2.3.13",
    "uk.gov.hmrc"                 %% "emailaddress"               % "3.8.0",
    "org.scala-graph"             %% "graph-core"                 % "2.0.3" exclude("org.scalacheck", "scalacheck_2.13"),
    "com.softwaremill.quicklens"  %% "quicklens"                  % "1.9.12",
    "com.nrinaudo"                %% "kantan.csv"                 % "0.8.0",
    "com.miguelfonseca.completely" % "completely-core"            % "0.9.0",
    "org.jsoup"                    % "jsoup"                      % "1.19.1",
    "org.webjars.npm"              % "govuk-frontend"             % "5.9.0",
    "com.openhtmltopdf"            % "openhtmltopdf-pdfbox"       % "1.0.10",
    "org.apache.commons"           % "commons-text"               % "1.13.0",
    "com.dripower"                %% "play-circe"                 % "3014.1",
    "io.circe"                    %% "circe-core"                 % circeVersion,
    "io.circe"                    %% "circe-generic"              % circeVersion,
    "io.circe"                    %% "circe-parser"               % circeVersion,
    "org.apache.xmlgraphics"       % "fop"                        % "2.9",
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

  val munitVersion = "1.1.0"

  val test = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-30"  % bootstrapVersion    % Test,
    "org.scalatestplus"      %% "scalacheck-1-14"         % "3.2.2.0"           % Test,
    "org.pegdown"             % "pegdown"                 % "1.6.0"             % Test,
    "com.ironcorelabs"       %% "cats-scalatest"          % "3.1.1"             % Test,
    "org.playframework"      %% "play-test"               % PlayVersion.current % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "7.0.1"             % Test,
    "org.mockito"            %% "mockito-scala-scalatest" % "1.17.37"           % Test,
    "org.scalameta"          %% "munit"                   % munitVersion        % Test,
    "org.scalameta"          %% "munit-scalacheck"        % munitVersion        % Test,
    "org.playframework"      %% "play-pekko-http-server"  % "3.0.6"             % Test,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.8"            % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-30" % hmrcMongoVersion    % Test
  )
}
