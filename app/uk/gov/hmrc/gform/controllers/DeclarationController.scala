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
import cats.Monoid
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.all._
import play.api.libs.json.Json
import play.api.mvc.Request
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class DeclarationController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    repeatService: RepeatingComponentService,
    auditingModule: AuditingModule,
    renderer: SectionRenderingService,
    validationModule: ValidationModule
) extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def showDeclaration(formId: FormId) = auth.async(formId) { implicit authRequest =>
    renderer.renderDeclarationSection(formId, formTemplate, None).map(Ok(_))
  }

  def submitDeclaration(formId: FormId) = auth.async(formId) { implicit authRequest =>
    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val validationResultF = Future.sequence(
        formTemplate.declarationSection.fields
          .map(fieldValue => validationService.validateComponents(fieldValue, data, theForm.envelopeId))
      ).map(Monoid[ValidatedType].combineAll)

      get(data, FieldId("save")) match {
        case "Continue" :: Nil => validationResultF.flatMap {
          case Valid(()) =>
            val updatedForm = updateFormWithDeclaration(theForm, formTemplate, data)
            for {
              _ <- gformConnector.updateUserData(theForm._id, UserData(updatedForm.formData, None))
              response <- gformConnector.submitForm(formId)
              template <- gformConnector.getFormTemplate(theForm.formTemplateId)
              _ <- repeatService.clearSession
            } yield {
              auditService.sendSubmissionEvent(theForm, formTemplate.sections)
              Ok(Json.obj("envelope" -> response.body, "formId" -> Json.toJson(formId)))
              acknowledgementPage(template)
            }

          case validationResult @ Invalid(_) =>
            val errorMap = getErrorMap(validationResult, data, formTemplate)
            for {
              html <- renderer.renderDeclarationSection(formId, formTemplate, Some(errorMap.get))
            } yield Ok(html)
        }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val auditService = auditingModule.auditService
  private lazy val validationService = validationModule.validationService

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: Map[FieldId, Seq[String]]) = {
    val fieldNames = data.keySet.map(_.value)
    val declarationFields = formTemplate.declarationSection.fields.filter(_.submissible).flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormField(FieldId(name), data(FieldId(name)).head))
    }
    val updatedFields = form.formData.fields ++ declarationFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def acknowledgementPage(template: FormTemplate)(implicit request: Request[_]) = {
    val content = template.acknowledgementSection
      .map((ackSection: AckSection) =>
        uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement_content_partial(ackSection))
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = LocalDateTime.now()

    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement(timeMessage, content))
  }

  private def getErrorMap(validationResult: ValidatedType, data: Map[FieldId, Seq[String]], formTemplate: FormTemplate) = {
    ValidationUtil.evaluateValidationResult(formTemplate.declarationSection.fields, validationResult, data, Envelope(Nil)) match {
      case Left(validationResults) =>
        validationResults.map(result => ValidationUtil.extractedFieldValue(result) -> result).toMap
      case Right(_) => Map.empty[FieldValue, FormFieldValidationResult]
    }
  }
}
