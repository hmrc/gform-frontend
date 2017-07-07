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

import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.GformSession.envelopeId
import uk.gov.hmrc.gform.fileupload.{ FileUploadModule, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.Page
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

class FormController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    configModule: ConfigModule,
    repeatService: RepeatingComponentService,
    fileUploadModule: FileUploadModule
) extends FrontendController {

  import AuthenticatedRequest._
  import GformSession._
  import controllersModule.i18nSupport._

  def newForm(formTypeId: FormTypeId, version: Version) = auth.async { implicit c =>

    if (c.request.session.getFormId.isDefined)
      redirectToFormF
    else
      gformConnector.newForm(formTypeId, version).map { (x: NewFormResponse) =>
        val updatedSession = c.request.session
          .putFormId(x.form._id)
          .putVersion(x.form.formData.version)
          .putFormTypeId(x.form.formData.formTypeId)
          .putSectionNumber(firstSection)
          .putEnvelopeId(x.envelopeId)

        redirectToForm.withSession(updatedSession)
      }
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
}
