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
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.Logger
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.{ I18nSupport, MessagesApi }
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, Result }
import uk.gov.hmrc.gform.connectors.IsEncrypt
import uk.gov.hmrc.gform.controllers.GformSession.userId

import play.api.mvc.{ Action, AnyContent, Result }
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers._
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.ValidationUtil._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ DeleteService, RepeatingComponentService, RetrieveService, SaveService }

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RetrieveService, SaveService }
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RetrieveService, SaveService }
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SaveService }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable
import scala.concurrent.{ ExecutionContext, Future }

@Singleton
class FormGen @Inject() (val messagesApi: MessagesApi, val sec: SecuredActions, repeatService: RepeatingComponentService, validationModule: ValidationModule, fileUploadModule: FileUploadModule)(implicit ec: ExecutionContext, authConnector: AuthConnector)
    extends FrontendController with I18nSupport {
  import GformSession._

  case class Choice(decision: String)

  val choice = Form(mapping(
    "decision" -> nonEmptyText
  )(Choice.apply)(Choice.unapply))

  def decision(formTypeId: FormTypeId, version: Version, formId: FormId): Action[AnyContent] = sec.SecureWithTemplateAsync(formTypeId, version) { implicit authContext => implicit request =>

    choice.bindFromRequest.fold(
      errors => {
        Logger.error("GOT HERE")
        Future.successful(BadRequest(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, version, formId)))
      },
      success =>
        success.decision match {
          case "continue" =>
            formById(formTypeId, version, formId)(request)
          case "delete" =>
            val blankSession = request.session.removeEnvelopId
              .removeFormId
            Logger.info("DELETE")
            DeleteService.deleteForm(formId)
            Logger.info("HERE")
            Future.successful(Redirect(routes.FormController.newForm(formTypeId, version)).withSession(blankSession))
          case _ =>
            val blankSession = request.session.removeEnvelopId
            Future.successful(Redirect(routes.FormController.newForm(formTypeId, version)).withSession(blankSession))
        }
    )
  }

  def formById(formTypeId: FormTypeId, version: Version, formId: FormId): Action[AnyContent] = formByIdPage(formTypeId, version, formId, 0)

  def formByIdPage(formTypeId: FormTypeId, version: Version, formId: FormId, currPage: Int): Action[AnyContent] = sec.SecureWithTemplateAsync(formTypeId, version) { implicit authContext => implicit request =>
    val userId = request.session.getUserId.get
    val envelopeId = request.session.getEnvelopeId.get
    val envelope = fileUploadService.getEnvelope(envelopeId)

    val result = if (IsEncrypt.is)
      SaveService.getFormByIdCache(formTypeId, version, userId)
    else
      SaveService.getFormById(formTypeId, version, formId)

    result.flatMap { form =>

      val fieldIdToStrings: Map[FieldId, Seq[String]] = form.formData.fields.map(fd => fd.id -> List(fd.value)).toMap

      val formTemplate = request.formTemplate
      envelope.flatMap(envelope =>
        Page(currPage, formTemplate, repeatService, envelope).renderPage(fieldIdToStrings, Some(formId), None))

    }
  }

  def save(formTypeId: FormTypeId, version: Version, pageIdx: Int) = sec.SecureWithTemplateAsync(formTypeId, version) { implicit authContext => implicit request =>
    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>
      val formTemplate = request.formTemplate
      val envelopeId = request.session.getEnvelopeId.get
      val envelope = fileUploadService.getEnvelope(envelopeId)
      val formId = request.session.getFormId.get

      val page = envelope.map(envelope => Page(pageIdx, formTemplate, repeatService, envelope))

      val atomicFields: Future[List[FieldValue]] = page.map(_.section.atomicFields(repeatService))

      val validatedData: Future[Seq[ValidatedType]] = for {
        atomicFields <- atomicFields
        validateData <- Future.sequence(atomicFields.map(fv =>
          validationService.validateComponents(fv, data, envelopeId)))
      } yield validateData

      val validatedDataResult: Future[ValidatedType] = validatedData.map(validatedData => Monoid[ValidatedType].combineAll(validatedData))

      val finalResult: Future[Either[List[FormFieldValidationResult], List[FormFieldValidationResult]]] =
        for {
          validatedDataResult <- validatedDataResult
          envelope <- envelope
          atomicFields <- atomicFields
        } yield ValidationUtil.evaluateValidationResult(atomicFields, validatedDataResult, data, envelope)

      def processSaveAndContinue(userId: UserId)(continue: Future[Result])(implicit hc: HeaderCarrier): Future[Result] = finalResult.flatMap {
        case Left(listFormValidation) =>
          val map: Map[FieldValue, FormFieldValidationResult] = listFormValidation.map { (validResult: FormFieldValidationResult) =>
            extractedFieldValue(validResult) -> validResult
          }.toMap

          page.flatMap(page => page.renderPage(data, Some(formId), Some(map.get)))

        case Right(listFormValidation) =>
          val formFieldIds = listFormValidation.map(_.toFormField)
          val formFields = formFieldIds.sequenceU.map(_.flatten).toList.flatten

          val formData = FormData(userId, formTypeId, version, "UTF-8", formFields)

          SaveService.updateFormData(formId, formData, false).flatMap {
            case SaveResult(_, Some(error)) => Future.successful(BadRequest(error))
            case _ =>

              continue
          }
      } //End processSaveAndContinue

      def processSaveAndExit(userId: UserId) = {

        val formFieldsList: Future[List[FormFieldValidationResult]] = finalResult.map {
          case Left(formFieldResultList) => formFieldResultList
          case Right(formFieldResultList) => formFieldResultList
        }

        val formFieldIds: Future[List[List[FormField]]] = formFieldsList.map(_.map(_.toFormFieldTolerant))
        val formFields: Future[List[FormField]] = formFieldIds.map(_.flatten)

        val formData = formFields.map(formFields => FormData(userId, formTypeId, version, "UTF-8", formFields))

        formData.flatMap(formData =>
          SaveService.updateFormData(formId, formData, tolerant = true).map(response => Ok(Json.toJson(response))))
      }

      val booleanExprs = formTemplate.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)
      val optSectionIdx = BooleanExpr.nextTrueIdxOpt(pageIdx, booleanExprs, data)
      val optNextPage = envelope.map(envelope => optSectionIdx.map(i => Page(i, formTemplate, repeatService, envelope)))
      val actionE = optNextPage.map(optNextPage => FormAction.determineAction(data, optNextPage))
      actionE.flatMap {
        case Right(action) =>
          val userId = request.session.getUserId.get
          action match {
            case SaveAndContinue(nextPageToRender) =>
              processSaveAndContinue(userId)(nextPageToRender.renderPage(data, Some(formId), None))
            case SaveAndExit =>
              processSaveAndExit(userId)
            case SaveAndSummary => {
              val redirect = if (IsEncrypt.is) {
                Redirect(routes.SummaryGen.summaryByIdCache(formTypeId, version, userId))
              } else {
                Redirect(routes.SummaryGen.summaryById(formTypeId, version, formId))
              }
              processSaveAndContinue(userId)(Future.successful(redirect))
            }
            case AddGroup(groupId) =>
              repeatService.appendNewGroup(groupId).flatMap { _ =>
                page.flatMap(page => page.renderPage(data, Some(formId), None))
              }
            case RemoveGroup(groupId) =>
              repeatService.removeGroup(groupId, data).flatMap { updatedData =>
                page.flatMap(page => page.renderPage(updatedData, Some(formId), None))
              }

          }
        case Left(error) => Future.successful(BadRequest(error))
      }
    }
  }

  private def isStarted(formTypeId: FormTypeId, version: Version)(implicit authContext: AuthContext, hc: HeaderCarrier): Future[(Option[Index], UserId)] = {
    authConnector.getUserDetails[UserId](authContext).flatMap { x =>
      RetrieveService.getStartedForm(x, formTypeId, version).map((_, x))
    }
  }

  private def extractedFieldValue(validResult: FormFieldValidationResult): FieldValue = validResult match {
    case FieldOk(fv, _) => fv
    case FieldError(fv, _, _) => fv
    case ComponentField(fv, _) => fv
    case FieldGlobalOk(fv, _) => fv
    case FieldGlobalError(fv, _, _) => fv
  }

  private lazy val validationService = validationModule.validationService
  private lazy val fileUploadService = fileUploadModule.fileUploadService
}

