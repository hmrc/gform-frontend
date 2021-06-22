import play.core.PlayVersion
import play.sbt.PlayImport.ws
import sbt._

object Dependencies {

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val compile = Seq(
    ws,
    "uk.gov.hmrc"                 %% "bootstrap-frontend-play-28" % "4.3.0",
    "uk.gov.hmrc"                 %% "play-language"              % "5.1.0-play-28",
    "uk.gov.hmrc"                 %% "play-frontend-govuk"        % "0.77.0-play-28",
    "uk.gov.hmrc"                 %% "play-frontend-hmrc"         % "0.72.0-play-28",
    "uk.gov.hmrc"                 %% "auth-client"                % "5.6.0-play-28",
    "uk.gov.hmrc"                 %% "domain"                     % "5.11.0-play-27",
    "uk.gov.hmrc"                 %% "play-partials"              % "8.1.0-play-28",
    "com.typesafe.play"           %% "play-json-joda"             % "2.9.2",
    "org.julienrf"                %% "play-json-derived-codecs"   % "10.0.2",
    "org.typelevel"               %% "cats-core"                  % "2.2.0",
    "org.typelevel"               %% "cats-mtl"                   % "1.0.0",
    "org.typelevel"               %% "case-insensitive"           % "1.1.0",
    "com.github.pureconfig"       %% "pureconfig"                 % "0.14.0",
    "org.jetbrains"                % "markdown"                   % "0.1.41",
    "com.chuusai"                 %% "shapeless"                  % "2.3.3",
    "uk.gov.hmrc"                 %% "emailaddress"               % "3.3.0",
    "org.scala-graph"             %% "graph-core"                 % "1.12.5",
    "com.softwaremill.quicklens"  %% "quicklens"                  % "1.6.1",
    "com.nrinaudo"                %% "kantan.csv"                 % "0.5.1",
    "com.miguelfonseca.completely" % "completely-core"            % "0.8.0",
    "org.jsoup"                    % "jsoup"                      % "1.13.1",
    "org.webjars.npm"              % "govuk-frontend"             % "3.11.0"
  )

  val munitVersion = "0.7.22"

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc"            %% "service-integration-test" % "1.1.0-play-28"     % scope,
    "org.scalatestplus"      %% "scalacheck-1-14"          % "3.1.1.0"           % scope,
    "org.pegdown"             % "pegdown"                  % "1.6.0"             % scope,
    "com.ironcorelabs"       %% "cats-scalatest"           % "2.4.0"             % scope,
    "com.typesafe.play"      %% "play-test"                % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play"       % "5.1.0"             % scope,
    "org.mockito"            %% "mockito-scala-scalatest"  % "1.7.0"             % scope,
    "com.github.tomakehurst"  % "wiremock-jre8"            % "2.26.3"            % scope,
    "org.scalameta"          %% "munit"                    % munitVersion        % scope,
    "org.scalameta"          %% "munit-scalacheck"         % munitVersion        % scope,
    "com.typesafe.play"      %% "play-akka-http-server"    % "2.8.7"             % scope,
    "com.vladsch.flexmark" % "flexmark-all" % "0.35.10"  % scope
  )
}
