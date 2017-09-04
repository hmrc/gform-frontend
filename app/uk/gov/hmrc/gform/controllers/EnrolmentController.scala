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
import play.api.mvc.Action
import uk.gov.hmrc.gform.auth.{ EnrolmentService, Identifier, Verifier }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models.{ FormFieldValidationResult, ValidationUtil }
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.service.SectionRenderingService
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormId, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

@Singleton
class EnrolmentController @Inject() (
    controllersModule: ControllersModule,
    renderer: SectionRenderingService,
    validationModule: ValidationModule,
    gformBackendModule: GformBackendModule,
    enrolmentService: EnrolmentService
) extends FrontendController {

  import controllersModule.i18nSupport._

  def showEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = Action.async { implicit request =>
    for {
      formTemplate <- gformConnector.getFormTemplate(formTemplateId)
      result <- renderer.renderEnrolmentSection(formTemplate, None, lang).map(Ok(_))
    } yield result
  }

  def submitEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = Action.async { implicit request =>
    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>
      gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>

        val validationResultF = Future.sequence(
          getAllEnrolmentFields(formTemplate.authConfig.enrolmentSection.get.fields)
            .map(fieldValue => validationService.validateComponents(fieldValue, data, EnvelopeId("")))
        ).map(Monoid[ValidatedType].combineAll)

        get(data, FieldId("save")) match {
          case "Continue" :: Nil => validationResultF.flatMap {
            case Valid(()) =>

              val (identifiers, verifiers) = extractIdentifiersAndVerifiers(formTemplate, data)

              enrolmentService.enrolUser(formTemplate.authConfig.serviceId.get, identifiers, verifiers).map { _ =>
                Redirect(uk.gov.hmrc.gform.controllers.routes.FormController.newForm(formTemplateId, lang))
              }

            case validationResult @ Invalid(_) =>

              val errorMap = getErrorMap(validationResult, data, formTemplate)
              for {
                html <- renderer.renderEnrolmentSection(formTemplate, Some(errorMap.get), lang)
              } yield Ok(html)

          }
          case _ =>
            Future.successful(BadRequest("Cannot determine action"))
        }

      }
    }
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val validationService = validationModule.validationService
  private lazy val gformConnector = gformBackendModule.gformConnector

  private def getErrorMap(validationResult: ValidatedType, data: Map[FieldId, Seq[String]], formTemplate: FormTemplate) = {
    val enrolmentFields = getAllEnrolmentFields(formTemplate.authConfig.enrolmentSection.get.fields)
    ValidationUtil.evaluateValidationResult(enrolmentFields, validationResult, data, Envelope(Nil)) match {
      case Left(validationResults) =>
        validationResults.map(result => ValidationUtil.extractedFieldValue(result) -> result).toMap
      case Right(_) => Map.empty[FieldValue, FormFieldValidationResult]
    }
  }

  private def getAllEnrolmentFields(fields: List[FieldValue]): List[FieldValue] = {
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllEnrolmentFields(grp.fields)
        case _ => List(fieldValue)
      }
    }
  }

  private def extractIdentifiersAndVerifiers(template: FormTemplate, data: Map[FieldId, Seq[String]]) = {

    def getValue(fieldValue: FieldValue) = {
      data.getOrElse(fieldValue.id, Seq("")).head
    }

    val identifierPattern = "identifier_(.*)".r
    val verifierPattern = "verifier_(.*)".r

    getAllEnrolmentFields(template.authConfig.enrolmentSection.get.fields)
      .foldLeft((List.empty[Identifier], List.empty[Verifier])) { (result, fieldValue) =>
        fieldValue.id.value match {
          case identifierPattern(identifier) => (result._1 :+ Identifier(identifier, getValue(fieldValue)), result._2)
          case verifierPattern(verifier) => (result._1, result._2 :+ Verifier(verifier, getValue(fieldValue)))
          case _ => result
        }
      }
  }
}
