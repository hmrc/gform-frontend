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

import cats.instances.future._
import cats.data.Validated.{ Invalid, Valid }
import play.api.i18n.I18nSupport
import play.api.mvc.{ AnyContent, Request, Result }
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.auth._
import uk.gov.hmrc.gform.config.AppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.{ get, processResponseDataFromBody }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormDataRecalculated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.play.HeaderCarrierConverter
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
  appConfig: AppConfig,
  recalculation: Recalculation[Future, Throwable]
) extends FrontendController {

  import i18nSupport._

  def showEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = auth.asyncGGAuth(formTemplateId) {
    implicit request => cache =>
      gformConnector.getFormTemplate(formTemplateId).map { formTemplate =>
        formTemplate.authConfig match {
          case HasEnrolmentSection((_, enrolmentSection)) =>
            Ok {
              renderer
                .renderEnrolmentSection(
                  formTemplate,
                  cache.retrievals,
                  enrolmentSection,
                  FormDataRecalculated.empty,
                  Nil,
                  Valid(()),
                  lang)
            }
          case _ =>
            Redirect(uk.gov.hmrc.gform.auth.routes.ErrorController.insufficientEnrolments())
              .flashing("formTitle" -> formTemplate.formName)
        }
      }
  }

  def submitEnrolment(formTemplateId: FormTemplateId, lang: Option[String]) = auth.asyncGGAuth(formTemplateId) {
    implicit request => cache =>
      processResponseDataFromBody(request) { (dataRaw: Map[FormComponentId, Seq[String]]) =>
        gformConnector.getFormTemplate(formTemplateId).flatMap { formTemplate =>
          formTemplate.authConfig match {
            case HasEnrolmentSection((serviceId, enrolmentSection)) =>
              val allFields = getAllEnrolmentFields(enrolmentSection.fields)
              get(dataRaw, FormComponentId("save")) match {
                case "Continue" :: Nil =>
                  for {
                    data <- recalculation.recalculateFormData(dataRaw, cache.formTemplate, cache.retrievals)
                    validationResult <- validationService.validateComponents(
                                         allFields,
                                         data,
                                         EnvelopeId(""),
                                         cache.retrievals,
                                         cache.formTemplate)
                    res <- processValidation(formTemplate, cache.retrievals, serviceId, enrolmentSection, data, lang)(
                            validationResult)
                  } yield res

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
    retrievals: MaterialisedRetrievals,
    serviceId: ServiceId,
    enrolmentSection: EnrolmentSection,
    data: FormDataRecalculated,
    lang: Option[String]
  )(validationResult: ValidatedType)(implicit hc: HeaderCarrier, request: Request[AnyContent]): Future[Result] =
    validationResult match {
      case Valid(()) =>
        val (identifiers, verifiers) = extractIdentifiersAndVerifiers(enrolmentSection, data)
        implicit val hc: HeaderCarrier =
          HeaderCarrierConverter.fromHeadersAndSession(request.headers, Some(request.session))
        (for {
          _       <- enrolmentService.enrolUser(serviceId, identifiers, verifiers)
          authRes <- auth.checkEnrolment(serviceId, identifiers)
        } yield {
          authRes match {
            case EnrolmentSuccessful => Redirect(routes.FormController.dashboard(formTemplate._id, lang).url)
            case EnrolmentFailed =>
              val newPageUrl = routes.FormController.dashboard(formTemplate._id, lang).url
              val continueUrl = java.net.URLEncoder.encode(appConfig.`gform-frontend-base-url` + newPageUrl, "UTF-8")
              val ggLoginUrl = appConfig.`government-gateway-sign-in-url`
              val redirectUrl = s"$ggLoginUrl?continue=$continueUrl"
              Redirect(redirectUrl)

          }

        }).recoverWith(handleEnrolmentException(formTemplate, lang))

      case validationResult @ Invalid(_) =>
        Future.successful(
          displayEnrolmentSectionWithErrors(validationResult, data, enrolmentSection, formTemplate, retrievals, lang))
    }

  private def getErrorMap(
    validationResult: ValidatedType,
    data: FormDataRecalculated,
    enrolmentSection: EnrolmentSection
  ): List[(FormComponent, FormFieldValidationResult)] = {
    val enrolmentFields = getAllEnrolmentFields(enrolmentSection.fields)
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
    enrolmentSection: EnrolmentSection,
    data: FormDataRecalculated
  ): (List[Identifier], List[Verifier]) = {

    def getValue(fieldValue: FormComponent) =
      data.data
        .getOrElse(fieldValue.id, Seq(""))
        .head
        .replaceAll("""\s""", "") // spaces need to be deleted to send to GG

    val identifierPattern = "identifier_(.*)".r
    val verifierPattern = "verifier_(.*)".r

    val (allIdentifiers, allVerifiers) = getAllEnrolmentFields(enrolmentSection.fields)
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
    data: FormDataRecalculated,
    enrolmentSection: EnrolmentSection,
    formTemplate: FormTemplate,
    retrievals: MaterialisedRetrievals,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): Result = {

    val errorMap = getErrorMap(validationResult, data, enrolmentSection)

    val html =
      renderer
        .renderEnrolmentSection(formTemplate, retrievals, enrolmentSection, data, errorMap, validationResult, lang)
    Ok(html)
  }

  private def handleEnrolmentException(
    formTemplate: FormTemplate,
    lang: Option[String]
  )(implicit hc: HeaderCarrier, request: Request[_]): PartialFunction[Throwable, Future[Result]] = {
    case _ =>
      Future.successful(
        Redirect(routes.FormController.dashboard(formTemplate._id, lang)).withNewSession
      )
  }
}
