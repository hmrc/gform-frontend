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
import play.api.mvc.{ Action, Request, Result }
import uk.gov.hmrc.gform.auth.{ AuthModule, Identifier, Verifier }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationModule, ValidationUtil }
import uk.gov.hmrc.gform.service.SectionRenderingService
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.{ BadRequestException, HeaderCarrier, Upstream4xxResponse, Upstream5xxResponse }

import scala.concurrent.Future

@Singleton
class EnrolmentController @Inject() (
    controllersModule: ControllersModule,
    renderer: SectionRenderingService,
    validationModule: ValidationModule,
    gformBackendModule: GformBackendModule,
    authModule: AuthModule
) extends FrontendController {

  import controllersModule.i18nSupport._

  def showEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = Action.async { implicit request =>

    gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>
      formTemplate.authConfig match {
        case authConfig: AuthConfigWithEnrolment =>
          renderer.renderEnrolmentSection(formTemplate, authConfig.enrolmentSection, None, lang).map(Ok(_))
        case _ => Future.successful(
          Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
            .flashing("formTitle" -> formTemplate.formName)
        )
      }
    }
  }

  def submitEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = Action.async { implicit request =>

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>
      gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>

        formTemplate.authConfig match {
          case authConfig: AuthConfigWithEnrolment =>

            val allFields = getAllEnrolmentFields(authConfig.enrolmentSection.fields)
            val validationResultF = validationService.validateComponents(allFields, data, EnvelopeId(""))

            get(data, FieldId("save")) match {
              case "Continue" :: Nil =>
                validationResultF.flatMap(processValidation(formTemplate, authConfig, data, lang))
              case _ =>
                Future.successful(BadRequest("Cannot determine action"))
            }
          case _ =>
            Future.successful(
              Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
                .flashing("formTitle" -> formTemplate.formName)
            )
        }
      }
    }
  }

  private lazy val validationService = validationModule.validationService
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val enrolmentService = authModule.enrolmentService

  private def processValidation(
    formTemplate: FormTemplate,
    authConfig: AuthConfigWithEnrolment,
    data: Map[FieldId, Seq[String]],
    lang: Option[String]
  )(validationResult: ValidatedType)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] = {

    validationResult match {
      case Valid(()) =>
        val (identifiers, verifiers) = extractIdentifiersAndVerifiers(authConfig, data)

        enrolmentService.enrolUser(authConfig.serviceId, identifiers, verifiers).map { _ =>
          Redirect(routes.FormController.newForm(formTemplate._id, lang))
        }.recoverWith(handleEnrolmentException(authConfig, data, formTemplate, lang))

      case validationResult @ Invalid(_) =>
        displayEnrolmentSectionWithErrors(validationResult, data, authConfig, formTemplate, lang)
    }
  }

  private def getErrorMap(
    validationResult: ValidatedType,
    data: Map[FieldId, Seq[String]],
    authConfig: AuthConfigWithEnrolment
  ): Map[FieldValue, FormFieldValidationResult] = {

    val enrolmentFields = getAllEnrolmentFields(authConfig.enrolmentSection.fields)
    ValidationUtil.evaluateValidationResult(enrolmentFields, validationResult, data, Envelope(Nil)) match {
      case Left(validationResults) =>
        validationResults.map(result => result.fieldValue -> result).toMap
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

  private def extractIdentifiersAndVerifiers(
    authConfig: AuthConfigWithEnrolment,
    data: Map[FieldId, Seq[String]]
  ): (List[Identifier], List[Verifier]) = {

    def getValue(fieldValue: FieldValue) = {
      data.getOrElse(fieldValue.id, Seq("")).head.replaceAll("""\s""", "") // spaces need to be deleted to send to GG
    }

    val identifierPattern = "identifier_(.*)".r
    val verifierPattern = "verifier_(.*)".r

    getAllEnrolmentFields(authConfig.enrolmentSection.fields)
      .foldLeft((List.empty[Identifier], List.empty[Verifier])) { (result, fieldValue) =>
        fieldValue.id.value match {
          case identifierPattern(identifier) => (result._1 :+ Identifier(identifier, getValue(fieldValue)), result._2)
          case verifierPattern(verifier) => (result._1, result._2 :+ Verifier(verifier, getValue(fieldValue)))
          case _ => result
        }
      }
  }

  private def displayEnrolmentSectionWithErrors(
    validationResult: ValidatedType,
    data: Map[FieldId, Seq[String]],
    authConfig: AuthConfigWithEnrolment,
    formTemplate: FormTemplate,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] = {

    val errorMap = getErrorMap(validationResult, data, authConfig)
    for {
      html <- renderer.renderEnrolmentSection(formTemplate, authConfig.enrolmentSection, Some(errorMap.get), lang)
    } yield Ok(html)
  }

  private def buildFailedValidationResult(
    authConfig: AuthConfigWithEnrolment,
    data: Map[FieldId, Seq[String]]
  ): Invalid[GformError] = {

    val map = getAllEnrolmentFields(authConfig.enrolmentSection.fields).foldLeft(Map.empty[FieldId, Set[String]]) {
      (result, current) =>
        data.get(current.id) match {
          case Some(value) if !value.head.isEmpty => result + (current.id -> Set("Please check the values entered"))
          case _ => result
        }
    }
    Invalid(map)
  }

  private def handleEnrolmentException(
    authConfig: AuthConfigWithEnrolment,
    data: Map[FieldId, Seq[String]],
    formTemplate: FormTemplate,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): PartialFunction[Throwable, Future[Result]] = {

    case _: Upstream4xxResponse | _: BadRequestException =>
      // As per architect's advice: Enrolment failed, the user entered wrong data
      //                            sleep 10 secs to try to avoid a brute force attack
      Thread.sleep(10000)
      val validationResult = buildFailedValidationResult(authConfig, data)
      displayEnrolmentSectionWithErrors(validationResult, data, authConfig, formTemplate, lang)

    case Upstream5xxResponse(message, _, _) if message.contains("missing required bearer token") | message.contains("Invalid security token") =>
      // Typically due to a session timeout, redirecting to newform to force going to GG login page again
      Future.successful(
        Redirect(routes.FormController.newForm(formTemplate._id, lang))
          .withNewSession
          .withHeaders(Seq.empty: _*)
      )

    case otherException => throw otherException
  }
}
