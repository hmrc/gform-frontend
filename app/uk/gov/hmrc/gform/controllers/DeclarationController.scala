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

import cats.data.Validated.{ Invalid, Valid }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.validation.DeclarationFieldValidationService
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class DeclarationController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    repeatService: RepeatingComponentService,
    fieldValidator: DeclarationFieldValidationService,
    auditingModule: AuditingModule
) /*(implicit ec: ExecutionContext)*/ extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def showDeclaration(formId: FormId) = auth.async(formId = Some(formId)) { implicit authRequest =>
    val formF = gformConnector.getForm(formId)
    for {
      form <- formF
      formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
    } yield Ok(uk.gov.hmrc.gform.views.html.declaration(formTemplate, form._id, Map.empty, Map.empty))
  }

  def submitDeclaration(formId: FormId) = auth.async(formId = Some(formId)) { implicit c =>
    processResponseDataFromBody(c.request) { (data: Map[FieldId, Seq[String]]) =>
      get(data, FieldId("save")) match {
        case "Continue" :: Nil =>
          val formF = gformConnector.getForm(formId)
          val templateF: FormTemplateId => Future[FormTemplate] = gformConnector.getFormTemplate
          fieldValidator.validateDeclarationFields(data) match {
            case Valid(()) =>
              for {
                form <- formF
                updatedForm = updateFormWithDeclaration(form, data)
                _ <- gformConnector.updateUserData(form._id, UserData(updatedForm.formData, None))
                response <- gformConnector.submitForm(formId)
                template <- templateF(form.formTemplateId) //TODO move this outside this single case.
                _ <- repeatService.clearSession
              } yield {
                auditService.sendSubmissionEvent(form, template.sections)
                Ok(Json.obj("envelope" -> response.body, "formId" -> Json.toJson(formId)))
              }
            case Invalid(validationMap) =>
              for {
                form <- formF
                formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
              } yield Ok(uk.gov.hmrc.gform.views.html.declaration(formTemplate, form._id, validationMap, data))
          }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val auditService = auditingModule.auditService

  private def updateFormWithDeclaration(form: Form, data: Map[FieldId, Seq[String]]) = {
    val updatedFields = data.foldLeft(form.formData.fields) {
      case (result, element) => element match {
        case a @ (FieldId("declaration-firstname"), value) => if (value.mkString.isEmpty) result else result :+ FormField(a._1, value.mkString)
        case a @ (FieldId("declaration-lastname"), value) => if (value.mkString.isEmpty) result else result :+ FormField(a._1, value.mkString)
        case a @ (FieldId("declaration-status"), value) => if (value.mkString.isEmpty) result else result :+ FormField(a._1, value.mkString)
        case a @ (FieldId("declaration-email2"), value) => if (value.mkString.isEmpty) result else result :+ FormField(a._1, value.mkString)
        case _ => result
      }
    }

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }
}
