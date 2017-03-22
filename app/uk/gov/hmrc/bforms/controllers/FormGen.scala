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

package uk.gov.hmrc.bforms.controllers

import javax.inject.{Inject, Singleton}

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, Request, Result }
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.bforms.FrontendAuthConnector
import uk.gov.hmrc.bforms.core.{ Add, FormCtx, Expr }
import uk.gov.hmrc.bforms.models._
import uk.gov.hmrc.play.frontend.auth._
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.bforms.service.{SaveService, RetrieveService}
import uk.gov.hmrc.play.http.HeaderCarrier

@Singleton
class FormGen @Inject()(val messagesApi: MessagesApi, val sec: SecuredActions)(implicit ec: ExecutionContext)
    extends FrontendController with I18nSupport {

  def form(formTypeId: FormTypeId, version: String) =
    sec.SecureWithTemplate(formTypeId, version) { authContext => implicit request =>

      val formTemplate = request.formTemplate

      Page(0, formTemplate).renderPage(Map.empty[FieldId, Seq[String]], None, None)

    }

  def formById(formTypeId: FormTypeId, version: String, formId: FormId) = sec.SecureWithTemplateAsync(formTypeId, version) { authContext => implicit request =>

    SaveService.getFormById(formTypeId, version, formId).map { formData =>

      val lookup: Map[FieldId, Seq[String]] = formData.fields.map(fd => fd.id -> List(fd.value)).toMap

      val formTemplate = request.formTemplate

      Page(0, formTemplate).renderPage(lookup, Some(formId), None)

    }
  }

  private def alwaysOk(fieldValue: FieldValue)(xs: Seq[String]): FormFieldValidationResult = {
    xs match {
      case Nil => FieldOk(fieldValue, "")
      case value :: rest => FieldOk(fieldValue, value) // we don't support multiple values yet
    }
  }

  private def validateRequired(fieldValue: FieldValue)(xs: Seq[String]): FormFieldValidationResult = {
    xs.filterNot(_.isEmpty()) match {
      case Nil => RequiredField(fieldValue)
      case value :: Nil => FieldOk(fieldValue, value)
      case value :: rest => FieldOk(fieldValue, value) // we don't support multiple values yet
    }
  }

  def validateFieldValue(fieldValue: FieldValue, formValue: ComponentData): FormFieldValidationResult = {
    formValue match {
      case AddressComponentData(street1, street2, street3, town, county, postcode) =>
        val validateRF = validateRequired(fieldValue) _
        AddressField(
          fieldValue,
          Map(
            "street1" -> validateRF(street1),
            "street2" -> alwaysOk(fieldValue)(street2),
            "street3" -> alwaysOk(fieldValue)(street3),
            "town" -> validateRF(town),
            "county" -> validateRF(county),
            "postcode" -> validateRF(postcode)
          )
        )
      case TextData(formValue) => fieldValue.mandatory match {
        case Some("true") => validateRequired(fieldValue)(formValue)
        case _ => alwaysOk(fieldValue)(formValue)
      }
    }
  }

  val FormIdExtractor = "bforms/forms/.*/.*/([\\w\\d-]+)$".r.unanchored

  def save(formTypeId: FormTypeId, version: String, currentPage: Int) = sec.SecureWithTemplateAsync(formTypeId, version) { authContext => implicit request =>
    request.body.asFormUrlEncoded.map(_.map{ case (a, b) => (FieldId(a), b)}) match {
      case None => Future.successful(BadRequest("Cannot parse body as FormUrlEncoded")) // Thank you play-authorised-frontend for forcing me to do this check
      case Some(data) =>
        val formTemplate = request.formTemplate

        val page = Page(currentPage, formTemplate)
        val nextPage = Page(page.next, formTemplate)

        val actions: List[String] = data.get(FieldId("save")).toList.flatten
        val formIdOpt: Option[FormId] = data.get(FieldId("formId")).flatMap(_.filterNot(_.isEmpty()).headOption).map(FormId.apply)

        val actionE: Either[String, FormAction] = FormAction.fromAction(actions, page)

        val dataGetter: FieldValue => String => Seq[String] = fv => suffix => data.get(fv.id.withSuffix(suffix)).toList.flatten

        val validate: Map[FieldValue, ComponentData] = page.section.fields.map { fv =>
          fv.`type` match {
            case Some(Address) =>
              val getData = dataGetter(fv)
              val acd = AddressComponentData(
                getData("street1"),
                getData("street2"),
                getData("street3"),
                getData("town") ,
                getData("county") ,
                getData("postcode")
              )
              fv -> acd
            case None => fv -> TextData(data.get(fv.id).toList.flatten)
          }
        }.toMap

        val validationResults: Map[FieldValue, FormFieldValidationResult] = validate.map {
          case (fieldValue, values) =>
            fieldValue -> validateFieldValue(fieldValue, values)
        }

        def saveAndProcessResponse(continuation: SaveResult => Future[Result])(implicit hc: HeaderCarrier): Future[Result] = {
          val canSave: Either[Unit, List[FormField]] = validationResults.map {
            case (_, validationResult) => validationResult.toFormField
          }.toList.sequenceU.map(_.flatten)

          canSave match {
            case Right(formFields) =>
              val formData = FormData(formTypeId, version, "UTF-8", formFields)

              submitOrUpdate(formIdOpt, formData, false).flatMap(continuation)

            case Left(_) =>
              Future.successful(page.renderPage(data, formIdOpt, Some(validationResults.get)))
          }
        }

        actionE match {
          case Right(action) =>
            action match {
              case SaveAndContinue =>
                saveAndProcessResponse { saveResult =>

                  val result =
                    getFormId(formIdOpt, saveResult) match {
                      case Right(formId) => nextPage.renderPage(data, Some(formId), None)
                      case Left(error) => BadRequest(error)
                    }

                  Future.successful(result)
                }
              case SaveAndExit =>
                val formFields: List[FormField] = validationResults.values.map(_.toFormFieldTolerant).toList

                val formData = FormData(formTypeId, version, "UTF-8", formFields)
                submitOrUpdate(formIdOpt, formData, true).map(response => Ok(Json.toJson(response)))

              case SaveAndSubmit =>
                saveAndProcessResponse { saveResult =>

                  getFormId(formIdOpt, saveResult) match {

                    case Right(formId) =>
                      SaveService.sendSubmission(formTypeId, formId).map { r =>
                        Ok(
                          Json.obj(
                            "envelope" -> r.body,
                            "formId" -> Json.toJson(saveResult)
                          )
                        )
                      }
                    case Left(error) =>
                      Future.successful(BadRequest(error))
                  }
                }
            }

          case Left(error) =>
            Future.successful(BadRequest(error))
        }
    }
  }

  private def submitOrUpdate(formIdOpt: Option[FormId], formData: FormData, tolerant: Boolean)(implicit hc: HeaderCarrier): Future[SaveResult] = {
    formIdOpt match {
      case Some(formId) =>
        SaveService.updateFormData(formId, formData, tolerant)
      case None =>
        SaveService.saveFormData(formData, tolerant)
    }
  }

  private def getFormId(formIdOpt: Option[FormId], saveResult: SaveResult): Either[String, FormId] = {
    formIdOpt match {
      case Some(formId) => Right(formId)
      case None => saveResult.success match {
        case Some(FormIdExtractor(formId)) => Right(FormId(formId))
        case Some(otherwise) => Left(s"Cannot determine formId from $otherwise")
        case None => Left(s"Cannot determine formId from ${Json.toJson(saveResult)}")
      }
    }
  }
}
