import play.core.PlayVersion
import play.sbt.PlayImport.ws
import sbt._

object Dependencies {

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val compile = Seq(
    ws,
    "uk.gov.hmrc" %% "bootstrap-frontend-play-27" % "3.2.0",
    "uk.gov.hmrc"                  %% "play-language"            % "4.5.0-play-27",
    "uk.gov.hmrc"                  %% "play-frontend-govuk"      % "0.60.0-play-27",
    "uk.gov.hmrc"                  %% "play-frontend-hmrc"       % "0.40.0-play-27",
    "uk.gov.hmrc"                  %% "auth-client"              % "3.2.0-play-27",
    "uk.gov.hmrc"                  %% "domain"                   % "5.10.0-play-27",
    "uk.gov.hmrc"                  %% "play-partials"            % "7.1.0-play-27",
    "com.typesafe.play"            %% "play-json-joda"           % "2.7.4",
    "org.julienrf"                 %% "play-json-derived-codecs" % "4.0.1",
    "org.typelevel"                %% "cats-core"                % "2.2.0",
    "org.typelevel"                %% "cats-mtl"                 % "1.0.0",
    "com.github.pureconfig"        %% "pureconfig"               % "0.14.0",
    "org.jetbrains"                % "markdown"                  % "0.1.41",
    "com.chuusai"                  %% "shapeless"                % "2.3.3",
    "uk.gov.hmrc"                  %% "emailaddress"             % "3.3.0",
    "org.scala-graph"              %% "graph-core"               % "1.12.5",
    "com.softwaremill.quicklens"   %% "quicklens"                % "1.4.11",
    "com.nrinaudo"                 %% "kantan.csv"               % "0.5.1",
    "com.miguelfonseca.completely" % "completely-core"           % "0.8.0",
    "org.jsoup"                    % "jsoup"                     % "1.13.1",
    "org.webjars.npm"              % "govuk-frontend"            % "3.11.0"
  )

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc" %% "service-integration-test" % "0.13.0-play-27" % scope,
    "org.scalacheck"         %% "scalacheck"         % "1.14.3"            % scope,
    "org.pegdown"            % "pegdown"             % "1.6.0"             % scope,
    "com.ironcorelabs"       %% "cats-scalatest"     % "2.4.0"             % scope,
    "com.typesafe.play"      %% "play-test"          % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "4.0.3"             % scope,
    "org.mockito" %% "mockito-scala-scalatest" % "1.7.0" % scope
  )
}
