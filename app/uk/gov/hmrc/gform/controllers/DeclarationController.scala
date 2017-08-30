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
import cats.Monoid
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.all._
import play.api.libs.json.Json
import play.api.mvc.Request
import play.twirl.api.Html
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SectionRenderingService }
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

  import controllersModule.i18nSupport._

  def showDeclaration(formId: FormId) = auth.async(formId) { implicit request => cache =>
    renderer.renderDeclarationSection(formId, cache.formTemplate, None, cache.retrievals).map(Ok(_))
  }

  def submitDeclaration(formId: FormId) = auth.async(formId) { implicit request => cache =>
    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val validationResultF = Future.sequence(
        getAllDeclarationFields(cache.formTemplate.declarationSection.fields)
          .map(fieldValue => validationService.validateComponents(fieldValue, data, cache.form.envelopeId))
      ).map(Monoid[ValidatedType].combineAll)

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
              Redirect(uk.gov.hmrc.gform.controllers.routes.AcknowledgementController.showAcknowledgement(formId))
            }
          case validationResult @ Invalid(_) =>
            val errorMap = getErrorMap(validationResult, data, cache.formTemplate)
            for {
              html <- renderer.renderDeclarationSection(formId, cache.formTemplate, Some(errorMap.get), cache.retrievals)
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

  private def getErrorMap(validationResult: ValidatedType, data: Map[FieldId, Seq[String]], formTemplate: FormTemplate) = {
    val declarationFields = getAllDeclarationFields(formTemplate.declarationSection.fields)
    ValidationUtil.evaluateValidationResult(declarationFields, validationResult, data, Envelope(Nil)) match {
      case Left(validationResults) =>
        validationResults.map(result => ValidationUtil.extractedFieldValue(result) -> result).toMap
      case Right(_) => Map.empty[FieldValue, FormFieldValidationResult]
    }
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
