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

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{ Inject, Singleton }

import cats.data.Validated.{ Invalid, Valid }
import org.joda.time.format
import play.api.libs.json.Json
import play.api.mvc.{ Request, WebSocket }
import play.twirl.api.Html
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AckSection, FieldId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldId, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId, UserData }
import uk.gov.hmrc.gform.validation.DeclarationFieldValidationService
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.Future

@Singleton
class DeclarationController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    repeatService: RepeatingComponentService,
    fieldValidator: DeclarationFieldValidationService,
    auditingModule: AuditingModule
) extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def showDeclaration(formId: FormId) = auth.async(formIdOpt = Some(formId)) { implicit authRequest =>
    val form = maybeForm.get
    Future.successful(
      Ok(uk.gov.hmrc.gform.views.html.declaration(formTemplate, form._id, Map.empty, Map.empty))
    )
  }

  def submitDeclaration(formId: FormId) = auth.async(formIdOpt = Some(formId)) { implicit c =>
    processResponseDataFromBody(c.request) { (data: Map[FieldId, Seq[String]]) =>
      get(data, FieldId("save")) match {
        case "Continue" :: Nil =>
          val form = maybeForm.get

          fieldValidator.validateDeclarationFields(data) match {
            case Valid(()) =>
              val updatedForm = updateFormWithDeclaration(form, data)
              for {
                _ <- gformConnector.updateUserData(form._id, UserData(updatedForm.formData, None))
                response <- gformConnector.submitForm(formId)
                template <- gformConnector.getFormTemplate(form.formTemplateId)
                _ <- repeatService.clearSession
              } yield {
                auditService.sendSubmissionEvent(form, formTemplate.sections)
                Ok(Json.obj("envelope" -> response.body, "formId" -> Json.toJson(formId)))
                ackPage(template)
              }

            case Invalid(validationMap) =>
              for {
                formTemplate <- gformConnector.getFormTemplate(form.formTemplateId)
              } yield Ok(uk.gov.hmrc.gform.views.html.declaration(formTemplate, form._id, validationMap, data))
          }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  def ackPage(template: FormTemplate)(implicit request: Request[_]) = {
    val content = template.acknowledgementSection
      .map((ackSection: AckSection) =>
        uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement_content_partial(ackSection))
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = LocalDateTime.now()

    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement(timeMessage, content))
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
