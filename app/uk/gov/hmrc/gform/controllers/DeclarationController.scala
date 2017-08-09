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
import play.api.mvc.{ AnyContent, Request }
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ anyFormId, formDataMap, get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models.Summary
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FormTemplateId }
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class DeclarationController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    configModule: ConfigModule,
    repeatService: RepeatingComponentService,
    fileUploadModule: FileUploadModule,
    authModule: AuthModule,
    val messagesApi: MessagesApi,
    auditingModule: AuditingModule
)(implicit ec: ExecutionContext) extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def showDeclaration(formId: FormId) = auth.async { implicit authRequest =>

    val formF = gformConnector.getForm(formId)
    for {
      // format: OFF
      form           <- formF
//      envelopeF      = fileUploadService.getEnvelope(form.envelopeId)
      formTemplate  <- gformConnector.getFormTemplate(form.formTemplateId)
      //envelope       <- envelopeF
      //map = formDataMap(form.formData)
      // format: ON
    } yield Ok(uk.gov.hmrc.gform.views.html.declaration(formTemplate, form._id, None))
  }

  def submitDeclaration(formId: FormId) = auth.async { implicit c =>

    processResponseDataFromBody(c.request) { (data: Map[FieldId, Seq[String]]) =>
      get(data, FieldId("save")) match {
        case "Continue" :: Nil =>
          anyFormId(data) match {
            case Some(formId) =>
              val submissionF = gformConnector.submitForm(formId)
              val formF = gformConnector.getForm(formId)
              for {
                response <- submissionF
                form <- formF
                _ <- repeatService.clearSession
              } yield {
                auditService.sendSubmissionEvent(form)
                Ok(Json.obj("envelope" -> response.body, "formId" -> Json.toJson(formId)))
              }
            case None =>
              Future.successful(BadRequest("No formId"))
          }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val auditService = auditingModule.auditService
}
