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

import play.api.Logger
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.frontend.controller.FrontendController
import cats._
import cats.implicits._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class SummaryGen @Inject() (
  controllersModule: ControllersModule,
  repeatService: RepeatingComponentService,
  fileUploadModule: FileUploadModule,
  validationModule: ValidationModule
)(implicit ec: ExecutionContext)
    extends FrontendController {

  import controllersModule.i18nSupport._

  def summaryById(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

    for {// format: OFF
      envelope          <- envelopeF
      sections          <- sectionsF
      allFields         =  sections.flatMap(repeatService.atomicFields)
      componentsErrors  = validationService.validateComponents(allFields, data, cache.form.envelopeId)
      sectionErrors     = sections.map(validationService.validateUsingValidators(_, data)).sequenceU.map(Monoid[ValidatedType].combineAll)
      v                 <- validationService.sequenceValidations(componentsErrors, sectionErrors)
      errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      result            <- Summary(cache.formTemplate).renderSummary(errors.get, data, formId, repeatService, envelope, lang)
      // format: ON
    } yield result
  }

  def submit(formId: FormId, formTemplateId4Ga: FormTemplateId, totalPage: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>
      get(data, FieldId("save")) match {
        case "Exit" :: Nil => Future.successful(Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement(formId, formTemplateId4Ga, totalPage, lang)))
        case "Declaration" :: Nil => Future.successful(Redirect(routes.DeclarationController.showDeclaration(formId, formTemplateId4Ga, lang)))
        case _ => Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val validationService = validationModule.validationService
}
