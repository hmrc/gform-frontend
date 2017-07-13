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

import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.{ Page, UserId }
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, RetrieveService }
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

  def newForm(formTypeId: FormTypeId, version: Version) = auth.async { implicit c =>

    def updateSession(formId: FormId, envelopeId: EnvelopeId, userId: UserId) = Future.successful(c.request.session
      .putFormId(formId)
      .putEnvelopeId(envelopeId)
      .putVersion(version)
      .putFormTypeId(formTypeId)
      .putSectionNumber(firstSection)
      .putUserId(userId))

    for {
      userId <- authConnector.getUserDetails[UserId](authContext)
      optForm <- RetrieveService.getStartedForm(userId, formTypeId, version)
      (formId, envelopeId) <- formIdAndEnvelopeId(formTypeId, version, userId, optForm)
      session <- updateSession(formId, envelopeId, userId)
      result <- result(formTypeId, version, userId, optForm)
    } yield result.withSession(session)
  }

  private def formIdAndEnvelopeId(formTypeId: FormTypeId, version: Version, userId: UserId, formFound: Option[Index])(implicit hc: HeaderCarrier): Future[(FormId, EnvelopeId)] = {
    formFound match {
      case Some(Index(formId, envelopeId)) => Future.successful((formId, envelopeId))
      case _ =>
        gformConnector.newForm(formTypeId, version, userId).map {
          case (NewFormResponse(Form(formId, _, _), envelopeId, _)) => (formId, envelopeId)
        }
    }
  }

  private def result(formTypeId: FormTypeId, version: Version, userId: UserId, formFound: Option[Index])(implicit hc: HeaderCarrier, request: Request[_]) = {

    Future.successful(formFound match {
      case Some(Index(formId, _)) => Ok(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, version, formId))
      case None => Redirect(routes.FormController.form())
    })
  }

  def form() = auth.async { implicit c =>
    val formTypeId = c.request.session.getFormTypeId.get
    val version = c.request.session.getVersion.get
    val envelopeId = c.request.session.getEnvelopeId.get
    val envelope = fileUploadService.getEnvelope(envelopeId)
    for {
      formTemplate <- gformConnector.getFormTemplate(formTypeId, version)
      envelope <- envelope
      response <- Page(0, formTemplate, repeatService, envelope).renderPage(Map(), None, None)
    } yield response
  }

  def fileUploadPage(fId: String) = auth.async { implicit c =>
    val fileId = FileId(fId)
    val formTemplateF = gformConnector.getFormTemplate(
      c.request.session.getFormTypeId.get,
      c.request.session.getVersion.get
    )
    val envelopeId = c.request.session.getEnvelopeId.get
    val actionUrl = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${routes.FormController.form()}"
    for {
      formTemplate <- formTemplateF
    } yield Ok(
      uk.gov.hmrc.gform.views.html.file_upload_page(fileId, formTemplate, actionUrl)
    )
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val authConnector = authModule.authConnector
}
