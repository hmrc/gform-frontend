/*
 * Copyright 2026 HM Revenue & Customs
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

package uk.gov.hmrc.gform

import io.circe.Printer
import java.nio.file.{ Files, Path, Paths }
import java.time.Instant
import org.apache.pekko.actor.ActorSystem
import play.api.libs.ws.ahc.{ AhcWSClient, AhcWSClientConfigFactory }
import play.api.{ Configuration, Environment }
import play.api.i18n.Messages
import play.api.libs.json.{ JsError, JsSuccess, JsValue, Json }
import scala.concurrent.{ Await, Future }
import scala.io.Source
import scala.jdk.StreamConverters._
import scala.util.{ Failure, Success, Try }
import uk.gov.hmrc.gform.auth.models.Role
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.models.SectionSelectorType
import uk.gov.hmrc.gform.recalculation.{ EvaluationContext, MongoUserData }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeExpiryDate, FormData, FormField, InProgress, TaskIdTaskStatusMapping }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplateContext, FormTemplateVersion, TaskId }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, UserId }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, Form, FormId, FormModelOptics, VisitIndex }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplate
import uk.gov.hmrc.gform.tasklist.TaskStatus
import uk.gov.hmrc.http.client.{ HttpClientV2, HttpClientV2Impl }
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse, StringContextOps }
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.http.HttpReads.Implicits._
import scala.concurrent.duration._

object RealJsonTemplateSupport {

  def mkFormModelOptics(
    mongoUserData: MongoUserData,
    visitIndex: VisitIndex,
    evaluationContext0: EvaluationContext,
    filePath: String
  )(implicit l: LangADT, messages: Messages): FormModelOptics = {

    if (!Files.exists(Paths.get(filePath))) {
      throw new Exception(s"File $filePath doesn't exist. Use 'refreshTemplates' to create it.")
    }
    val file: String = Source.fromFile(filePath).mkString

    val json: JsValue = Json.parse(file)

    val formTemplate = FormTemplate.format.reads(json) match {
      case JsSuccess(ft, _) => ft
      case JsError(error)   => throw new Exception(s"Failed to read internal json representation: $error\n\nFile: $file")
    }

    val evaluationContext = evaluationContext0.copy(authConfig = formTemplate.authConfig)

    val form = Form(
      _id = FormId(evaluationContext.retrievals, formTemplate, Option.empty[AccessCode]),
      envelopeId = EnvelopeId("dummy"),
      userId = UserId("dummy-user-id"),
      formTemplateId = formTemplate._id,
      formTemplateVersion = Option.empty[FormTemplateVersion],
      formData = FormData(List.empty[FormField]),
      status = InProgress,
      visitsIndex = visitIndex,
      thirdPartyData = evaluationContext.thirdPartyData,
      envelopeExpiryDate = Option.empty[EnvelopeExpiryDate],
      componentIdToFileId = evaluationContext.componentIdToFileId,
      taskIdTaskStatus = TaskIdTaskStatusMapping(Map.empty[TaskId, TaskStatus]),
      startDate = Instant.now()
    )

    val cache = AuthCacheWithForm(
      retrievals = evaluationContext.retrievals,
      form = form,
      formTemplateContext = FormTemplateContext.basicContext(formTemplate, Option.empty[FormTemplate]),
      role = Role.Customer,
      accessCode = Option.empty[AccessCode],
      lookupRegistry = evaluationContext.lookupRegistry
    )

    FormModelOptics.mkFormModelOptics[SectionSelectorType.Normal](
      data = mongoUserData.lookup,
      cache = cache,
      phase = evaluationContext.formPhase
    )

  }

  def refreshTemplates(folderName: String): Unit = {

    val sourceDir = s"test-templates/$folderName/source"
    val internalDir = s"test-templates/$folderName/internal"

    val path = Path.of(sourceDir)

    val paths: List[Path] = Files.list(path).filter(p => Files.isRegularFile(p)).toScala(List)

    implicit val actorSystem: ActorSystem = ActorSystem()
    val environment = Environment.simple()
    val configuration: play.api.Configuration = Configuration.load(environment)

    val httpClientV2: HttpClientV2 =
      new HttpClientV2Impl(
        wsClient = AhcWSClient(AhcWSClientConfigFactory.forConfig(configuration.underlying)),
        actorSystem,
        configuration,
        hooks = Seq.empty
      )

    val uploadFormUrl = url"http://localhost:9196/gform/formtemplates"
    def internalJson(formTemplateId: String) = url"http://localhost:9196/gform/formtemplates/$formTemplateId/internal"

    implicit val hc = HeaderCarrier()

    val storeInternalJson: Future[List[Path]] = Future.traverse(paths) { path =>
      val uri = path.toUri
      val file: String = Source.fromFile(uri).mkString
      val json: JsValue = Try(Json.parse(file)) match {
        case Success(v) => v
        case Failure(e) => throw new Exception(s"Failed to parse json file: $uri", e)
      }
      httpClientV2
        .post(uploadFormUrl)
        .setHeader("Content-type" -> "text/plain; charset=utf-8")
        .withBody(json)
        .execute[HttpResponse]
        .flatMap { response =>
          if (response.status == 204) {
            val internalJsonUrl = internalJson(path.getFileName().toString.replace(".json", ""))
            httpClientV2
              .get(internalJsonUrl)
              .execute[JsValue]
              .map { response =>
                val saveToPath = Paths.get(internalDir + "/" + path.getFileName())
                val json: io.circe.Json = io.circe.parser
                  .parse(response.toString())
                  .toOption
                  .getOrElse(throw new Exception("Failed to parse json"))
                val printer = Printer.spaces2
                  .copy(
                    colonLeft = "",
                    lrbracketsEmpty = ""
                  )
                Files.writeString(
                  saveToPath,
                  printer
                    .print(json)
                    .replaceAll("\\{\\s*\\}", "{}") // render empty object as '{}'
                    + "\n" // new line at the end of file
                )
              }

          } else {
            Future.failed(
              new Exception("Failed to upload json. Response status: " + response.status + ", file path: " + path)
            )
          }
        }
    }
    val internalJsonPaths: List[Path] = Await.result(storeInternalJson, 10.seconds)
    internalJsonPaths.sorted.foreach(path => println(s"Writing $path"))
    println(internalJsonPaths.size.toString + " internal json templates saved")
  }

}
