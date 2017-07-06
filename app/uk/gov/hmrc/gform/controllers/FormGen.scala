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

import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.traverse._
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.json.Json
import play.api.mvc.Result
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.ValidationUtil._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SaveService }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable
import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class FormGen @Inject() (val messagesApi: MessagesApi, val sec: SecuredActions, repeatService: RepeatingComponentService, validationModule: ValidationModule, fileUploadModule: FileUploadModule)(implicit ec: ExecutionContext)
    extends FrontendController with I18nSupport {

  def form(formTypeId: FormTypeId, version: Version) =
    sec.SecureWithTemplateAsync(formTypeId, version) { implicit authContext => implicit request =>

      val formTemplate = request.formTemplate

      Page(0, formTemplate, repeatService).renderPage(Map(), None, None)

    }

  def formById(formTypeId: FormTypeId, version: Version, formId: FormId) = formByIdPage(formTypeId, version, formId, 0)

  def formByIdPage(formTypeId: FormTypeId, version: Version, formId: FormId, currPage: Int) = sec.SecureWithTemplateAsync(formTypeId, version) { implicit authContext => implicit request =>

    SaveService.getFormById(formTypeId, version, formId).flatMap { (formData: FormData) =>

      val fieldIdToStrings: Map[FieldId, Seq[String]] = formData.fields.map(fd => fd.id -> List(fd.value)).toMap

      val formTemplate = request.formTemplate

      Page(currPage, formTemplate, repeatService).renderPage(fieldIdToStrings, Some(formId), None)

    }
  }

  val FormIdExtractor = "gform/forms/.*/.*/([\\w\\d-]+)$".r.unanchored

  def save(formTypeId: FormTypeId, version: Version, pageIdx: Int) = sec.SecureWithTemplateAsync(formTypeId, version) { implicit authContext => implicit request =>
    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>
      val formTemplate = request.formTemplate

      val page = Page(pageIdx, formTemplate, repeatService)

      val formIdOpt: Option[FormId] = anyFormId(data)

      val atomicFields = page.section.atomicFields(repeatService)

      import GformSession._

      val envelopeId = request.session.getEnvelopeId.get

      val validatedData: Seq[ValidatedType] = atomicFields.map(fv =>
        validationService.validateComponents(fv, data, envelopeId))

      val validatedDataResult: ValidatedType = Monoid[ValidatedType].combineAll(validatedData)

      val envelope = fileUploadService.getEnvelope(envelopeId)
      val finalResult: Either[List[FormFieldValidationResult], List[FormFieldValidationResult]] =
        ValidationUtil.evaluateValidationResult(atomicFields, validatedDataResult, data, envelope)

      def saveAndProcessResponse(continuation: SaveResult => Future[Result])(implicit hc: HeaderCarrier): Future[Result] = {

        finalResult match {
          case Left(listFormValidation) =>
            val map: Map[FieldValue, FormFieldValidationResult] = listFormValidation.map { validResult =>

              val extractedFieldValue = validResult match {
                case FieldOk(fv, _) => fv
                case FieldError(fv, _, _) => fv
                case ComponentField(fv, _) => fv
                case FieldGlobalOk(fv, _) => fv
                case FieldGlobalError(fv, _, _) => fv
              }

              extractedFieldValue -> validResult
            }.toMap

            page.renderPage(data, formIdOpt, Some(map.get))

          case Right(listFormValidation) =>
            val formFieldIds = listFormValidation.map(_.toFormField)
            val formFields = formFieldIds.sequenceU.map(_.flatten).toList.flatten

            val formData = FormData(formTypeId, version, "UTF-8", formFields)
            submitOrUpdate(formIdOpt, formData, false).flatMap {
              case SaveResult(_, Some(error)) => Future.successful(BadRequest(error))
              case result => continuation(result)
            }
        }

      }

      val booleanExprs = formTemplate.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)
      val optSectionIdx = BooleanExpr.nextTrueIdxOpt(pageIdx, booleanExprs, data)
      val optNextPage = optSectionIdx.map(i => Page(i, formTemplate, repeatService))
      val actionE = FormAction.determineAction(data, optNextPage)
      actionE match {
        case Right(action) =>
          action match {
            case SaveAndContinue(nextPageToRender) =>
              saveAndProcessResponse { saveResult =>
                getFormId(formIdOpt, saveResult) match {
                  case Right(formId) => nextPageToRender.renderPage(data, Some(formId), None)
                  case Left(error) => Future.successful(BadRequest(error))
                }
              }
            case SaveAndExit =>

              val formFieldsList = finalResult match {
                case Left(formFieldResultList) => formFieldResultList
                case Right(formFieldResultList) => formFieldResultList
              }

              val formFieldIds: List[List[FormField]] = formFieldsList.map(_.toFormFieldTolerant)
              val formFields = formFieldIds.flatten

              val formData = FormData(formTypeId, version, "UTF-8", formFields)

              submitOrUpdate(formIdOpt, formData, true).map(response => Ok(Json.toJson(response)))

            case SaveAndSummary =>
              saveAndProcessResponse { saveResult =>

                getFormId(formIdOpt, saveResult) match {

                  case Right(formId) =>
                    Future.successful(Redirect(routes.SummaryGen.summaryById(formTypeId, version, formId)))
                  case Left(error) =>
                    Future.successful(BadRequest(error))
                }
              }
            case AddGroup(groupId) =>
              repeatService.increaseGroupCount(groupId).flatMap { _ =>
                page.renderPage(data, formIdOpt, None)
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

  private lazy val validationService = validationModule.validationService
  private lazy val fileUploadService = fileUploadModule.fileUploadService
}
