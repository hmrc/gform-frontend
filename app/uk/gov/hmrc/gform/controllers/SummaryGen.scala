/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.controllers

import javax.inject.{ Inject, Singleton }

import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model.FormId
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components.FieldId
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SaveService }
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class SummaryGen @Inject() (
  controllersModule: ControllersModule,
  gformBackendModule: GformBackendModule,
  configModule: ConfigModule,
  repeatService: RepeatingComponentService,
  fileUploadModule: FileUploadModule,
  authModule: AuthModule,
  val messagesApi: MessagesApi,
  val sec: SecuredActions
)(implicit ec: ExecutionContext)
    extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def summaryById(formId: FormId) = auth.async { implicit c =>
    val formF = gformConnector.getForm(formId)
    for {// format: OFF
      form           <- formF
      envelopeF      = fileUploadService.getEnvelope(form.envelopeId)
      formTemplateF  = gformConnector.getFormTemplate(form.formData.formTypeId)
      envelope       <- envelopeF
      formTemplate   <- formTemplateF
      // format: ON
    } yield {
      val map = formDataMap(form.formData)
      Summary(formTemplate).renderSummary(map, formId, repeatService, envelope)
    }
  }

  def submit(formId: FormId) = auth.async { implicit c =>

    processResponseDataFromBody(c.request) { data =>
      get(data, FieldId("save")) match {
        case "Exit" :: Nil =>
          Future.successful(Ok)
        case "Continue" :: Nil =>
          anyFormId(data) match {
            case Some(formId) =>
              val submissionF = SaveService.sendSubmission(formId)
              for {
                response <- submissionF
                _ <- repeatService.clearSession
              } yield Ok(Json.obj("envelope" -> response.body, "formId" -> Json.toJson(formId)))
            case None =>
              Future.successful(BadRequest("No formId"))
          }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val authConnector = authModule.authConnector
  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector

}
