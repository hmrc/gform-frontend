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

import javax.inject.Inject

import play.api.mvc.{ Action, AnyContent, Request }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.{ Page, UserId }
import uk.gov.hmrc.gform.models.Choice._
import uk.gov.hmrc.gform.models.components.FieldId
import uk.gov.hmrc.gform.service.{ DeleteService, RepeatingComponentService, RetrieveService }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FormController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    configModule: ConfigModule,
    repeatService: RepeatingComponentService,
    fileUploadModule: FileUploadModule,
    authModule: AuthModule
) extends FrontendController {

  import AuthenticatedRequest._
  import GformSession._
  import controllersModule.i18nSupport._

  def newForm(formTypeId: FormTypeId) = auth.async { implicit c =>

    def updateSession(envelopeId: EnvelopeId, formTypeId: FormTypeId, formId: FormId, userId: UserId) = Future.successful(c.request.session
      .putFormId(formId)
      .putFormTypeId(formTypeId)
      .putEnvelopeId(envelopeId)
      .putUserId(userId))

    for {
      userId <- authConnector.getUserDetails[UserId](authContext)
      formId <- create(userId, formTypeId)
      (optForm, envelopeId) <- start(formTypeId, userId, formId)
      session <- updateSession(envelopeId, formTypeId, formId, userId)
      result <- result(formTypeId, formId, optForm)
    } yield result.withSession(session)
  }

  private def start(formTypeId: FormTypeId, userId: UserId, formId: FormId)(implicit hc: HeaderCarrier): Future[(Option[FormId], EnvelopeId)] =
    gformConnector.isStarted(formId).flatMap[(Option[FormId], EnvelopeId)](_.fold(gformConnector.newForm(formTypeId, userId, formId).map(x => (Option.empty[FormId], x.envelopeId)))(x => Future.successful((Some(formId), x))))

  private def result(formTypeId: FormTypeId, formId: FormId, formFound: Option[FormId])(implicit hc: HeaderCarrier, request: Request[_]) = {
    formFound match {
      case Some(_) => Future.successful(Ok(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, formId)))
      case None => Future.successful(Redirect(routes.FormController.form()))
    }
  }

  private def create(userId: UserId, formTypeId: FormTypeId): Future[FormId] = {
    Future.successful(FormId(userId, formTypeId))
  }

  def form() = auth.async { implicit c =>
    val formId = c.request.session.getFormId.get
    val formTypeId = c.request.session.getFormTypeId.get
    val envelopeId = c.request.session.getEnvelopeId.get
    val envelope = fileUploadService.getEnvelope(envelopeId)
    for {
      formTemplate <- gformConnector.getFormTemplate(formTypeId)
      formData <- getFormData(formId)
      envelope <- envelope
      response <- Page(0, formTemplate, repeatService, envelope, envelopeId).renderPage(formData, None, None)
    } yield response
  }

  def fileUploadPage(fId: String) = auth.async { implicit c =>
    val fileId = FileId(fId)
    val formTemplateF = gformConnector.getFormTemplate(
      c.request.session.getFormTypeId.get
    )
    val envelopeId = c.request.session.getEnvelopeId.get
    val actionUrl = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${routes.FormController.form()}"
    for {
      formTemplate <- formTemplateF
    } yield Ok(
      uk.gov.hmrc.gform.views.html.file_upload_page(fileId, formTemplate, actionUrl)
    )
  }

  private def getFormData(formId: FormId)(implicit hc: HeaderCarrier): Future[Map[FieldId, List[String]]] =
    gformConnector.getForm(formId).map(_.fields.map(fd => fd.id -> List(fd.value)).toMap)

  def decision(formTypeId: FormTypeId, formId: FormId): Action[AnyContent] = auth.async { implicit c =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, formId))),
      success =>
        success.decision match {
          case "continue" => Future.successful(Redirect(routes.FormController.form()))
          case "delete" =>
            val userId = request.session.getUserId.get
            val blankSession = request.session.removeEnvelopId
              .removeFormId
            gformConnector.deleteForm(formId).map { x =>
              Redirect(routes.FormController.newForm(formTypeId)).withSession(blankSession)
            }
          case _ =>
            val blankSession = request.session.removeEnvelopId
            Future.successful(Redirect(routes.FormController.newForm(formTypeId)).withSession(blankSession))
        }
    )
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val authConnector = authModule.authConnector
}
