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
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.Page
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

class FormController @Inject() (controllersModule: ControllersModule, gformBackendModule: GformBackendModule) extends FrontendController {

  import controllersModule.i18nSupport._ //this brings implicit messages
  import AuthenticatedRequest._ //this explicitly brings implicits cause compiler can't see them even if they are in companion ...

  def newForm(formTypeId: FormTypeId, version: Version) = auth.async { implicit c =>

    if (ses.formId.isDefined)
      redirectToFormF
    else
      gformConnector.newForm(formTypeId, version).map { (x: NewFormResponse) =>
        val newSession = request.session +
          (SessionKeys.formId -> x.form._id.value) +
          (SessionKeys.formTypeId -> x.form.formData.formTypeId.value) +
          (SessionKeys.version -> x.form.formData.version.value)

        redirectToForm.withSession(newSession)
      }
  }

  private def isFormIdInSession()(implicit r: Request[_]): Boolean = ses.formId.isDefined

  def form() = auth.async { implicit c =>

    for {
      formTemplate <- gformConnector.getFormTemplate(ses.formTypeId.get, ses.version.get)
      response <- Page(0, formTemplate).renderPage(Map(), None, None)
    } yield response
  }

  private lazy val ses = new GformSession
  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val redirectToForm = Redirect(routes.FormController.form())
  private lazy val redirectToFormF = Future.successful(redirectToForm)
}

class GformSession {
  def formId(implicit r: Request[_]) = r.session.data.get(SessionKeys.formId).map(FormId.apply)
  def version(implicit r: Request[_]) = r.session.data.get(SessionKeys.version).map(Version.apply)
  def formTypeId(implicit r: Request[_]) = r.session.data.get(SessionKeys.formTypeId).map(FormTypeId.apply)
  def envelopeId(implicit r: Request[_]) = r.session.data.get(SessionKeys.envelopeId).map(EnvelopeId.apply)
}