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
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class SummaryGen @Inject() (
  controllersModule: ControllersModule,
  repeatService: RepeatingComponentService,
  fileUploadModule: FileUploadModule
)(implicit ec: ExecutionContext)
    extends FrontendController {

  import controllersModule.i18nSupport._

  def summaryById(formId: FormId) = auth.async(formId) { implicit request => cache =>
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)

    for {// format: OFF
      envelope       <- envelopeF
      map = formDataMap(cache.form.formData)
      result <- Summary(cache.formTemplate).renderSummary(map, formId, repeatService, envelope)
      // format: ON
    } yield result
  }

  def submit(formId: FormId, formTypeId: FormTemplateId) = auth.async(formId) { implicit request => cache =>

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>
      get(data, FieldId("save")) match {
        case "Exit" :: Nil =>
          Future.successful(Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement(formId, formTypeId)))
        case "Declaration" :: Nil =>
          Future.successful(Redirect(routes.DeclarationController.showDeclaration(formId)))
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val auth = controllersModule.authenticatedRequestActions
}
