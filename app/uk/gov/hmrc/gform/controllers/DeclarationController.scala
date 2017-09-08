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
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SectionRenderingService }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationModule }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
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

  import controllersModule.i18nSupport._

  def showDeclaration(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    renderer.renderDeclarationSection(formId, cache.formTemplate, None, cache.retrievals, lang).map(Ok(_))
  }

  def submitDeclaration(formTemplateId4Ga: FormTemplateId, formId: FormId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val validationResultF = validationService.validateComponents(getAllDeclarationFields(cache.formTemplate.declarationSection.fields), data, cache.form.envelopeId)

      get(data, FieldId("save")) match {
        case "Continue" :: Nil => validationResultF.flatMap {
          case Valid(()) =>
            val updatedForm = updateFormWithDeclaration(cache.form, cache.formTemplate, data)
            for {
              _ <- gformConnector.updateUserData(cache.form._id, UserData(updatedForm.formData, None))
              _ <- gformConnector.submitForm(formId)
              _ <- repeatService.clearSession
            } yield {
              auditService.sendSubmissionEvent(cache.form, cache.formTemplate.sections :+ cache.formTemplate.declarationSection, cache.retrievals)
              Redirect(uk.gov.hmrc.gform.controllers.routes.AcknowledgementController.showAcknowledgement(formId, formTemplateId4Ga, lang))
            }
          case validationResult @ Invalid(_) =>
            val errorMap = getErrorMap(validationResult, data, cache.formTemplate)
            for {
              html <- renderer.renderDeclarationSection(formId, cache.formTemplate, Some(errorMap.get), cache.retrievals, lang)
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
    val allDeclarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    val submissibleFormFields = allDeclarationFields.flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormField(FieldId(name), data(FieldId(name)).head))
    }
    val updatedFields = form.formData.fields ++ submissibleFormFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def getErrorMap(validationResult: ValidatedType, data: Map[FieldId, Seq[String]], formTemplate: FormTemplate): Map[FieldValue, FormFieldValidationResult] = {
    val declarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope(Nil))
  }

  private def getAllDeclarationFields(fields: List[FieldValue]): List[FieldValue] = {
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllDeclarationFields(grp.fields)
        case _ => List(fieldValue)
      }
    }
  }
}
