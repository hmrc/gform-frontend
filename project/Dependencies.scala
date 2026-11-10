import play.core.PlayVersion
import sbt.*

object Dependencies {

  val jacksonVersion = "2.18.3"
  val jacksonDatabindVersion = "2.18.3"
  val circeVersion = "0.15.0-M1"
  val bootstrapVersion = "10.8.0"
  val hmrcMongoVersion = "2.14.0"
  val luceneVersion = "10.6.0"

  lazy val appDependencies: Seq[ModuleID] = compile ++ test

  val compile = Seq(
    "uk.gov.hmrc"                  %% "bootstrap-frontend-play-30" % bootstrapVersion,
    "uk.gov.hmrc"                  %% "play-frontend-hmrc-play-30" % "13.16.0",
    "uk.gov.hmrc.mongo"            %% "hmrc-mongo-play-30"         % hmrcMongoVersion,
    "uk.gov.hmrc"                  %% "play-partials-play-30"      % "10.2.0",
    "uk.gov.hmrc"                  %% "domain-play-30"             % "11.0.0",
    "org.playframework"            %% "play-json-joda"             % "3.0.6",
    "org.julienrf"                 %% "play-json-derived-codecs"   % "11.0.0",
    "org.typelevel"                %% "cats-core"                  % "2.13.0",
    "org.typelevel"                %% "cats-free"                  % "2.13.0",
    "org.typelevel"                %% "cats-mtl"                   % "1.7.0",
    "org.typelevel"                %% "case-insensitive"           % "1.5.0",
    "commons-codec"                 % "commons-codec"              % "1.23.0",
    "com.github.pureconfig"        %% "pureconfig"                 % "0.17.11",
    "org.jetbrains"                 % "markdown-jvm"               % "0.7.18",
    "com.chuusai"                  %% "shapeless"                  % "2.3.13",
    "org.scala-graph"              %% "graph-core"                 % "2.0.3" exclude ("org.scalacheck", "scalacheck_2.13"),
    "com.softwaremill.quicklens"   %% "quicklens"                  % "1.9.15",
    "com.nrinaudo"                 %% "kantan.csv"                 % "0.8.0",
    "org.apache.lucene"             % "lucene-core"                % luceneVersion,
    "org.apache.lucene"             % "lucene-analysis-common"     % luceneVersion,
    "org.apache.lucene"             % "lucene-backward-codecs"     % luceneVersion,
    "org.jsoup"                     % "jsoup"                      % "1.24.1",
    "org.webjars.npm"               % "govuk-frontend"             % "6.6.0",
    "io.github.openhtmltopdf"       % "openhtmltopdf-pdfbox"       % "1.1.110",
    "org.apache.commons"            % "commons-text"               % "1.15.0",
    "com.dripower"                 %% "play-circe"                 % "3014.1",
    "io.circe"                     %% "circe-core"                 % circeVersion,
    "io.circe"                     %% "circe-generic"              % circeVersion,
    "io.circe"                     %% "circe-parser"               % circeVersion,
    "org.apache.xmlgraphics"        % "fop"                        % "2.12",
    "com.github.ben-manes.caffeine" % "caffeine"                   % "3.3.1",
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

  val test: Seq[ModuleID] = Seq(
    "uk.gov.hmrc"            %% "bootstrap-test-play-30"  % bootstrapVersion    % Test,
    "org.scalatestplus"      %% "scalacheck-1-14"         % "3.2.2.0"           % Test,
    "com.ironcorelabs"       %% "cats-scalatest"          % "3.1.1"             % Test,
    "org.playframework"      %% "play-test"               % PlayVersion.current % Test,
    "org.scalatestplus.play" %% "scalatestplus-play"      % "7.0.2"             % Test,
    "org.mockito"            %% "mockito-scala-scalatest" % "1.17.45"           % Test,
    "org.scalameta"          %% "munit"                   % munitVersion        % Test,
    "org.scalameta"          %% "munit-scalacheck"        % munitVersion        % Test,
    "org.playframework"      %% "play-pekko-http-server"  % "3.0.12"            % Test,
    "com.vladsch.flexmark"    % "flexmark-all"            % "0.64.8"            % Test,
    "uk.gov.hmrc.mongo"      %% "hmrc-mongo-test-play-30" % hmrcMongoVersion    % Test
  )
}
