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

import cats.Applicative
import play.api.Logger
import play.api.mvc.{ Action, AnyContent, Request, Session }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.GformSession.{ envelopeId, formTypeId, userId, version }
import uk.gov.hmrc.gform.fileupload.{ FileUploadModule, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.{ Page, UserId }
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, RetrieveService }
import uk.gov.hmrc.play.frontend.auth.AuthContext
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

  def check(formTypeId: FormTypeId, version: Version, userId: UserId, optIndx: Option[Index])(fail: Future[NewFormResponse], sf: (Index) => Session)(implicit hc: HeaderCarrier, request: Request[_]) = {
    optIndx match {
      case None =>
        fail.map { x =>
          val session: Session = sf(Index(x.form._id, x.envelopeId)).putUserId(userId)
          redirectToForm.withSession(session)
        }
      case Some(x) =>
        val session: Session = sf(x).putUserId(userId)
        Future.successful(Ok(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, version, x.formId)).withSession(session))
    }
  }

  def newForm(formTypeId: FormTypeId, version: Version) = auth.async { implicit c =>

    val updatedSession: Index => Session = inx => c.request.session
      .putFormId(inx.formId)
      .putVersion(version)
      .putFormTypeId(formTypeId)
      .putSectionNumber(firstSection)
      .putEnvelopeId(inx.envelopeId)

    for {
      userId <- authConnector.getUserDetails[UserId](authContext)
      maybeIndex <- RetrieveService.getStartedForm(userId, formTypeId, version)
      index <- check(formTypeId, version, userId, maybeIndex)(gformConnector.newForm(formTypeId, version, userId), updatedSession)
    } yield index
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
  private lazy val redirectToForm = Redirect(routes.FormController.form())
  private lazy val redirectToFormF = Future.successful(redirectToForm)
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val authConnector = authModule.authConnector
}
