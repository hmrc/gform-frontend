/*
 * Copyright 2018 HM Revenue & Customs
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
import play.api.mvc.{ Action, Request, Result }
import uk.gov.hmrc.gform.auth.{ Identifier, Verifier, _ }
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class EnrolmentController(
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  renderer: SectionRenderingService,
  validationService: ValidationService,
  gformConnector: GformConnector,
  enrolmentService: EnrolmentService,
  appConfig: AppConfig
) extends FrontendController {

  import i18nSupport._

  def showEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = Action.async { implicit request =>
    gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>
      formTemplate.authConfig match {
        case authConfig: AuthConfigWithEnrolment =>
          renderer
            .renderEnrolmentSection(formTemplate, authConfig.enrolmentSection, Map.empty, Nil, None, lang)
            .map(Ok(_))
        case _ =>
          Future.successful(
            Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
              .flashing("formTitle" -> formTemplate.formName)
          )
      }
    }
  }

  def submitEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>
        gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>
          formTemplate.authConfig match {
            case authConfig: AuthConfigWithEnrolment =>
              val allFields = getAllEnrolmentFields(authConfig.enrolmentSection.fields)
              val validationResultF =
                validationService.validateComponents(allFields, data, EnvelopeId(""), cache.retrievals)

              get(data, FormComponentId("save")) match {
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

  private def processValidation(
    formTemplate: FormTemplate,
    authConfig: AuthConfigWithEnrolment,
    data: Map[FormComponentId, Seq[String]],
    lang: Option[String]
  )(validationResult: ValidatedType)(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] =
    validationResult match {
      case Valid(()) =>
        val (identifiers, verifiers) = extractIdentifiersAndVerifiers(authConfig, data)

        enrolmentService
          .enrolUser(authConfig.serviceId, identifiers, verifiers)
          .map { _ =>
            val newPageUrl = routes.FormController.newForm(formTemplate._id, lang).url
            val continueUrl = java.net.URLEncoder.encode(appConfig.`gform-frontend-base-url` + newPageUrl, "UTF-8")
            val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
            val redirectUrl = s"$ggLoginUrl?continue=$continueUrl"
            Redirect(redirectUrl)
          }
          .recoverWith(handleEnrolmentException(authConfig, data, formTemplate, lang))

      case validationResult @ Invalid(_) =>
        displayEnrolmentSectionWithErrors(validationResult, data, authConfig, formTemplate, lang)
    }

  private def getErrorMap(
    validationResult: ValidatedType,
    data: Map[FormComponentId, Seq[String]],
    authConfig: AuthConfigWithEnrolment
  ): List[(FormComponent, FormFieldValidationResult)] = {
    val enrolmentFields = getAllEnrolmentFields(authConfig.enrolmentSection.fields)
    validationService.evaluateValidation(validationResult, enrolmentFields, data, Envelope(Nil))
  }

  private def getAllEnrolmentFields(fields: List[FormComponent]): List[FormComponent] =
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => getAllEnrolmentFields(grp.fields)
        case _          => List(fieldValue)
      }
    }

  private def extractIdentifiersAndVerifiers(
    authConfig: AuthConfigWithEnrolment,
    data: Map[FormComponentId, Seq[String]]
  ): (List[Identifier], List[Verifier]) = {

    def getValue(fieldValue: FormComponent) =
      data.getOrElse(fieldValue.id, Seq("")).head.replaceAll("""\s""", "") // spaces need to be deleted to send to GG

    val identifierPattern = "identifier_(.*)".r
    val verifierPattern = "verifier_(.*)".r

    val (allIdentifiers, allVerifiers) = getAllEnrolmentFields(authConfig.enrolmentSection.fields)
      .foldLeft((List.empty[Identifier], List.empty[Verifier])) { (result, fieldValue) =>
        fieldValue.id.value match {
          case identifierPattern(identifier) => (result._1 :+ Identifier(identifier, getValue(fieldValue)), result._2)
          case verifierPattern(verifier)     => (result._1, result._2 :+ Verifier(verifier, getValue(fieldValue)))
          case _                             => result
        }
      }
    (allIdentifiers.filterNot(_.value.equals("")), allVerifiers.filterNot(_.value.equals("")))
  }

  private def displayEnrolmentSectionWithErrors(
    validationResult: ValidatedType,
    data: Map[FormComponentId, Seq[String]],
    authConfig: AuthConfigWithEnrolment,
    formTemplate: FormTemplate,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): Future[Result] = {

    val errorMap = getErrorMap(validationResult, data, authConfig)
    for {
      html <- renderer.renderEnrolmentSection(
               formTemplate,
               authConfig.enrolmentSection,
               data,
               errorMap,
               Some(validationResult),
               lang)
    } yield Ok(html)
  }

  private def handleEnrolmentException(
    authConfig: AuthConfigWithEnrolment,
    data: Map[FormComponentId, Seq[String]],
    formTemplate: FormTemplate,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): PartialFunction[Throwable, Future[Result]] = {
    case _ =>
      Future.successful(
        Redirect(routes.FormController.newForm(formTemplate._id, lang)).withNewSession
      )
  }
}
