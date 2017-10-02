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

package uk.gov.hmrc.gform.gform

import cats.data.Validated.{ Invalid, Valid }
import play.api.i18n.I18nSupport
import uk.gov.hmrc.gform.auditing.{ AuditService, AuditingModule }
import uk.gov.hmrc.gform.auth.{ AuthModule, AuthService }
import uk.gov.hmrc.gform.controllers.{ AuthenticatedRequestActions, ControllersModule }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.{ GformBackendModule, GformConnector }
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationModule, ValidationService }
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

class DeclarationController(
    i18nSupport: I18nSupport,
    auth: AuthenticatedRequestActions,
    gformConnector: GformConnector,
    auditService: AuditService,
    repeatService: RepeatingComponentService,
    renderer: SectionRenderingService,
    validationService: ValidationService,
    authService: AuthService
) extends FrontendController {

  import i18nSupport._

  def showDeclaration(formId: FormId, formTemplateId4Ga: FormTemplateId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    cache.form.status match {
      case Validated => renderer.renderDeclarationSection(cache.form, cache.formTemplate, cache.retrievals, None, Map.empty, None, lang).map(Ok(_))
      case _ => Future.successful(BadRequest)
    }
  }

  def submitDeclaration(formTemplateId4Ga: FormTemplateId, formId: FormId, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>

      val validationResultF = validationService.validateComponents(getAllDeclarationFields(cache.formTemplate.declarationSection.fields), data, cache.form.envelopeId)

      val customerId = authService.evaluateSubmissionReference(cache.formTemplate.dmsSubmission.customerId, cache.retrievals)

      get(data, FormComponentId("save")) match {
        case "Continue" :: Nil => validationResultF.flatMap {
          case Valid(()) =>
            val updatedForm = updateFormWithDeclaration(cache.form, cache.formTemplate, data)
            for {
              _ <- gformConnector.updateUserData(cache.form._id, UserData(updatedForm.formData, None, Signed))
              _ <- gformConnector.submitForm(formId, customerId)
              _ <- repeatService.clearSession
            } yield {
              val submissionEventId = auditService.sendSubmissionEvent(cache.form, cache.formTemplate.sections :+ cache.formTemplate.declarationSection, cache.retrievals)
              Redirect(uk.gov.hmrc.gform.gform.routes.AcknowledgementController.showAcknowledgement(formId, formTemplateId4Ga, lang, submissionEventId))
            }
          case validationResult @ Invalid(_) =>
            val errorMap: List[(FormComponent, FormFieldValidationResult)] = getErrorMap(validationResult, data, cache.formTemplate)
            for {
              html <- renderer.renderDeclarationSection(cache.form, cache.formTemplate, cache.retrievals, Some(validationResult), data, Some(errorMap), lang)
            } yield Ok(html)
        }
        case _ =>
          Future.successful(BadRequest("Cannot determine action"))
      }
    }
  }

  private def updateFormWithDeclaration(form: Form, formTemplate: FormTemplate, data: Map[FormComponentId, Seq[String]]) = {
    val fieldNames = data.keySet.map(_.value)
    val allDeclarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    val submissibleFormFields = allDeclarationFields.flatMap { fieldValue =>
      fieldNames
        .filter(_.startsWith(fieldValue.id.value))
        .map(name => FormField(FormComponentId(name), data(FormComponentId(name)).head))
    }
    val updatedFields = form.formData.fields ++ submissibleFormFields

    form.copy(formData = form.formData.copy(fields = updatedFields))
  }

  private def getErrorMap(validationResult: ValidatedType, data: Map[FormComponentId, Seq[String]], formTemplate: FormTemplate): List[(FormComponent, FormFieldValidationResult)] = {
    val declarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    validationService.evaluateValidation(validationResult, declarationFields, data, Envelope(Nil))
  }

  private def getAllDeclarationFields(fields: List[FormComponent]): List[FormComponent] = {
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllDeclarationFields(grp.fields)
        case _ => List(fieldValue)
      }
    }
  }

}
