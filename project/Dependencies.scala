import play.core.PlayVersion
import play.sbt.PlayImport.ws
import sbt._

object Dependencies {

  lazy val appDependencies: Seq[ModuleID] = compile ++ test()

  val parsebackVersion = "0.3"

  val compile = Seq(
    ws,
    "uk.gov.hmrc"                  %% "bootstrap-play-26"        % "1.3.0",
    "uk.gov.hmrc"                  %% "play-ui"                  % "8.3.0-play-26",
    "uk.gov.hmrc"                  %% "play-language"            % "4.2.0-play-26",
    "uk.gov.hmrc"                  %% "govuk-template"           % "5.44.0-play-26",
    "uk.gov.hmrc"                  %% "auth-client"              % "2.31.0-play-26",
    "uk.gov.hmrc"                  %% "domain"                   % "5.3.0",
    "uk.gov.hmrc"                  %% "play-partials"            % "6.9.0-play-26",
    "uk.gov.hmrc"                  %% "csp-client"               % "4.2.0-play-26",
    "com.typesafe.play"            %% "play-json-joda"           % "2.6.14",
    "com.codecommit"               %% "parseback-core"           % parsebackVersion,
    "com.codecommit"               %% "parseback-cats"           % parsebackVersion,
    "org.julienrf"                 %% "play-json-derived-codecs" % "3.3",
    "org.typelevel"                %% "cats-core"                % "1.6.0",
    "org.typelevel"                %% "cats-mtl-core"            % "0.5.0" exclude ("org.scalacheck", "scalacheck_2.11"),
    "com.github.pureconfig"        %% "pureconfig"               % "0.10.2",
    "org.jetbrains"                % "markdown"                  % "0.1.41",
    "com.chuusai"                  %% "shapeless"                % "2.3.3",
    "uk.gov.hmrc"                  %% "emailaddress"             % "3.3.0",
    "org.scala-graph"              %% "graph-core"               % "1.12.5",
    "com.softwaremill.quicklens"   %% "quicklens"                % "1.4.11",
    "com.nrinaudo"                 %% "kantan.csv"               % "0.5.1",
    "com.miguelfonseca.completely" % "completely-core"           % "0.8.0",
    "org.jsoup"                    % "jsoup"                     % "1.13.1"
  )

  def test(scope: String = "test") = Seq(
    "uk.gov.hmrc"            %% "hmrctest"           % "3.5.0-play-26"     % scope,
    "org.scalatest"          %% "scalatest"          % "3.0.5"             % scope,
    "org.scalacheck"         %% "scalacheck"         % "1.14.0"            % scope,
    "org.pegdown"            % "pegdown"             % "1.6.0"             % scope,
    "com.ironcorelabs"       %% "cats-scalatest"     % "2.4.0"             % scope,
    "com.typesafe.play"      %% "play-test"          % PlayVersion.current % scope,
    "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.1"             % scope,
    "org.mockito"            % "mockito-all"         % "1.10.19"           % scope
  )
}
