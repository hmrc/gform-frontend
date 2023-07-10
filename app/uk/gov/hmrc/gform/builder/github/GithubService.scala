/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.gform.builder.github

import io.circe.{ Decoder, Error, Json }
import io.circe.syntax._
import io.circe.generic.auto._
import cats.syntax.all._
import java.util.Base64
import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsValue, Json => PlayJson }
import scala.concurrent.{ ExecutionContext, Future }
import sttp.client4._
import sttp.client4.circe._
import sttp.client4.httpclient.HttpClientFutureBackend
import sttp.model.StatusCode
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import io.circe.generic.semiauto.deriveDecoder
import uk.gov.hmrc.http.HeaderCarrier

case class ShaBlob(value: String) extends AnyVal

case class GithubStateApi(
  sha: String,
  content: String
)

sealed trait GithubErrorModel
case class NotFound(message: String) extends GithubErrorModel
case class GenericError(message: String) extends GithubErrorModel

case class GithubState(
  sha: ShaBlob,
  content: String
)

object GithubStateApi {
  implicit val decoder: Decoder[GithubStateApi] = deriveDecoder[GithubStateApi]
}

class GithubService(
  gformConnector: GformConnector,
  githubConfig: GithubConfig
)(implicit
  ex: ExecutionContext
) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val GithubConfig(repoOwner, repoName, accessToken) = githubConfig

  private val request0 = basicRequest
    .header("User-Agent", "HRMC Gform")
    .header("Accept", "application/vnd.github+json")
    .header("Authorization", s"Bearer $accessToken")
    .header("X-GitHub-Api-Version", "2022-11-28")

  private def toCirceJsonUnsafe(json: JsValue) = {
    val jsonString: String = PlayJson.stringify(json)

    io.circe.parser.parse(jsonString) match {
      case Left(error) => throw new Exception("Cannot convert to circe json: " + jsonString)
      case Right(json) => json
    }
  }

  def storeJson(formTemplateId: FormTemplateId)(implicit hc: HeaderCarrier): Future[Unit] =
    for {
      json        <- gformConnector.getFormTemplateRaw(formTemplateId)
      githubState <- getCurrentState(formTemplateId)
      _           <- saveJsonToGithub(formTemplateId, toCirceJsonUnsafe(json), githubState)
    } yield ()

  def getCurrentState(formTemplateId: FormTemplateId): Future[Option[GithubState]] = {

    val request: Request[Either[ResponseException[String, Error], GithubStateApi]] =
      request0
        .get(uri"https://api.github.com/repos/$repoOwner/$repoName/contents/${formTemplateId.value}.json?ref=temp")
        .response(asJson[GithubStateApi])

    val backend = HttpClientFutureBackend()
    val response = request.send(backend)

    response
      .map(_.body)
      .map {
        case Right(githubState) =>
          val githubStateJson: Array[Byte] = Base64.getDecoder.decode(githubState.content.replace("\n", ""))
          val json: String = new String(githubStateJson, "UTF-8")
          Some(GithubState(ShaBlob(githubState.sha), json))
        case Left(responseException) =>
          responseException match {
            case HttpError(body, statusCode) =>
              statusCode match {
                case StatusCode.NotFound =>
                  // This branch is executed when form template json doesn't exist yet.
                  None
                case _ =>
                  val message = s"Unknown error when loading json from Github. StatusCode $statusCode, error: $body"
                  logger.error(message)
                  throw new Exception(message)
              }
            case DeserializationException(body, error) =>
              val message = s"Deserialization error when loading json from Github. Body $body, error: $body"
              logger.error(message)
              throw new Exception(message)
          }
      }
  }

  def saveJsonToGithub(
    formTemplateId: FormTemplateId,
    json: Json,
    maybeGithubState: Option[GithubState]
  ): Future[Unit] = {

    val newContent = json.spaces2

    val oldContent = maybeGithubState.map(_.content)

    if (oldContent.contains(newContent)) {
      // No change in json found, do not create new commit
      ().pure[Future]
    } else {
      val fileName = formTemplateId.value + ".json"

      val content = Base64.getEncoder.encodeToString(newContent.getBytes("UTF-8"))
      val payload =
        maybeGithubState
          .fold(Json.Null) { githubState =>
            Json.obj(
              "sha" := githubState.sha.value
            )
          }
          .deepMerge(
            Json
              .obj(
                "message" := s"Updating $fileName",
                "branch" := "temp", // So far we can't create content in main, so using branch as temporal workaround
                "committer" := Json.obj(
                  "name" := "gform",
                  "email" := "gform@users.noreply.github.com"
                ),
                "content" := content
              )
          )

      val request: Request[Either[ResponseException[String, Error], Json]] =
        request0
          .put(uri"https://api.github.com/repos/$repoOwner/$repoName/contents/$fileName")
          .body(payload)
          .response(asJson[Json])

      val backend = HttpClientFutureBackend()
      request
        .send(backend)
        .map(_.body)
        .map {
          case Right(_) => ()
          case Left(responseException) =>
            responseException match {
              case HttpError(body, statusCode) =>
                val message = s"Unknown error when storing json in Github. StatusCode $statusCode, error: $body"
                logger.error(message)
                throw new Exception(message)
              case DeserializationException(body, error) =>
                val message = s"Deserialization error when storing json in Github. Body $body, error: $body"
                logger.error(message)
                throw new Exception(message)
            }
        }
    }
  }
}
