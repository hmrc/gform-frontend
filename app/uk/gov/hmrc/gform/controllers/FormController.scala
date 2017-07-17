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

import javax.inject.Inject

import cats._
import cats.data._
import cats.instances.all._
import cats.syntax.all._

import play.api.data.Forms.mapping
import play.api.libs.json.Json
import play.api.mvc.{ Action, AnyContent, Request, Result }
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.FileUploadModule
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.gformbackend.model._
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models.components.{ FieldId, FieldValue }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SaveService }
import uk.gov.hmrc.gform.validation.ValidationModule
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class FormController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    configModule: ConfigModule,
    repeatService: RepeatingComponentService,
    fileUploadModule: FileUploadModule,
    authModule: AuthModule,
    validationModule: ValidationModule
) extends FrontendController {

  import AuthenticatedRequest._
  import GformSession._
  import controllersModule.i18nSupport._

  def newForm(formTypeId: FormTypeId) = auth.async { implicit c =>

    def updateSession(envelopeId: EnvelopeId, formTypeId: FormTypeId, formId: FormId, userId: UserId) = Future.successful(c.request.session
      .putFormId(formId)
      .putFormTypeId(formTypeId)
      .putEnvelopeId(envelopeId)
      .putUserId(userId))

    for {// format: OFF
      userId                <- authConnector.getUserDetails[UserId](authContext)
      formId                <- create(userId, formTypeId)
      (optForm, envelopeId) <- start(formTypeId, userId, formId)
      session               <- updateSession(envelopeId, formTypeId, formId, userId)
      result                <- result(formTypeId, formId, optForm, firstSection)
      // format: ON
    } yield result.withSession(session)
  }

  private def start(formTypeId: FormTypeId, userId: UserId, formId: FormId)(implicit hc: HeaderCarrier): Future[(Option[FormId], EnvelopeId)] =
    gformConnector.isStarted(formId).flatMap[(Option[FormId], EnvelopeId)](_.fold(gformConnector.newForm(formTypeId, userId, formId).map(x => (Option.empty[FormId], x.envelopeId)))(x => Future.successful((Some(formId), x))))

  private def result(formTypeId: FormTypeId, formId: FormId, formFound: Option[FormId], sectionNumber: SectionNumber)(implicit hc: HeaderCarrier, request: Request[_]) = {
    formFound match {
      case Some(_) => Future.successful(Ok(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, formId)))
      case None => Future.successful(Redirect(routes.FormController.form(formId, sectionNumber)))
    }
  }

  private def create(userId: UserId, formTypeId: FormTypeId): Future[FormId] = {
    Future.successful(FormId(userId, formTypeId))
  }

  def form(formId: FormId, sectionNumber: SectionNumber) = auth.async { implicit c =>

    for {// format: OFF
      form           <- gformConnector.getForm(formId)
      fieldData       = getFormData(form)
      formTemplateF   = gformConnector.getFormTemplate(form.formData.formTypeId)
      envelopeF       = fileUploadService.getEnvelope(form.envelopeId)
      formTemplate   <- formTemplateF
      envelope       <- envelopeF
      response       <- Page(formId, firstSection, formTemplate, repeatService, envelope, form.envelopeId).renderPage(fieldData, None, None)
      // format: ON
    } yield response
  }

  def fileUploadPage(formId: FormId, sectionNumber: SectionNumber, fId: String) = auth.async { implicit c =>
    val fileId = FileId(fId)
    val formTemplateF = gformConnector.getFormTemplate(
      c.request.session.getFormTypeId.get
    )
    val envelopeId = c.request.session.getEnvelopeId.get
    val actionUrl = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${routes.FormController.form(formId, sectionNumber)}"
    for {
      formTemplate <- formTemplateF
    } yield Ok(
      uk.gov.hmrc.gform.views.html.file_upload_page(formId, sectionNumber, fileId, formTemplate, actionUrl)
    )
  }

  private def getFormData(form: Form): Map[FieldId, List[String]] = form.formData.fields.map(fd => fd.id -> List(fd.value)).toMap

  private case class Choice(decision: String)

  private lazy val choice = play.api.data.Form(mapping(
    "decision" -> play.api.data.Forms.nonEmptyText
  )(Choice.apply)(Choice.unapply))

  def decision(formTypeId: FormTypeId, formId: FormId): Action[AnyContent] = auth.async { implicit c =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, formId))),
      success =>
        success.decision match {
          case "continue" => Future.successful(Redirect(routes.FormController.form(formId, firstSection /*TODO: once we store section number we could continumer from specific section*/ )))
          case "delete" =>
            val userId = request.session.getUserId.get
            val blankSession = request.session.removeEnvelopId
              .removeFormId
            gformConnector.deleteForm(formId).map { x =>
              Redirect(routes.FormController.newForm(formTypeId)).withSession(blankSession)
            }
          case _ =>
            val blankSession = request.session.removeEnvelopId
            Future.successful(Redirect(routes.FormController.newForm(formTypeId)).withSession(blankSession))
        }
    )
  }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber) = auth.async { implicit c =>

    val formF = gformConnector.getForm(formId)
    val envelopeIdF = formF.map(_.envelopeId)
    val envelopeF = for {
      envelopeId <- envelopeIdF
      envelope <- fileUploadService.getEnvelope(envelopeId)
    } yield envelope

    val formTemplateF = for {
      form <- formF
      formTemplate <- gformConnector.getFormTemplate(form.formData.formTypeId)
    } yield formTemplate

    val pageF = for {
      form <- formF
      envelope <- envelopeF
      formTemplate <- formTemplateF
    } yield Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId)

    val userIdF = authConnector.getUserDetails[UserId](authContext)

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val atomicFields = for {
        page <- pageF
      } yield page.section.atomicFields(repeatService)

      val validatedDataResult: Future[ValidatedType] = for {
        atomicFields <- atomicFields
        form <- formF
        envelopeId = form.envelopeId
        validatedData <- Future.sequence(atomicFields.map(fv =>
          validationService.validateComponents(fv, data, envelopeId)))
      } yield Monoid[ValidatedType].combineAll(validatedData)

      val finalResult: Future[Either[List[FormFieldValidationResult], List[FormFieldValidationResult]]] =
        for {
          validatedDataResult <- validatedDataResult
          form <- formF
          envelopeId = form.envelopeId
          envelope <- fileUploadService.getEnvelope(envelopeId)
          atomicFields <- atomicFields
        } yield ValidationUtil.evaluateValidationResult(atomicFields, validatedDataResult, data, envelope)

      def processSaveAndContinue(userId: UserId, form: Form)(continue: Future[Result])(implicit hc: HeaderCarrier): Future[Result] = finalResult.flatMap {
        case Left(listFormValidation) =>
          val map: Map[FieldValue, FormFieldValidationResult] = listFormValidation.map { (validResult: FormFieldValidationResult) =>
            extractedFieldValue(validResult) -> validResult
          }.toMap

          pageF.flatMap(page => page.renderPage(data, Some(formId), Some(map.get)))

        case Right(listFormValidation) =>

          val formFieldIds = listFormValidation.map(_.toFormField)
          val formFields = formFieldIds.sequenceU.map(_.flatten).toList.flatten

          val formData = FormData(userId, form.formData.formTypeId, "UTF-8", formFields)

          SaveService.updateFormData(formId, formData, false).flatMap {
            case SaveResult(_, Some(error)) => Future.successful(BadRequest(error))
            case _ =>

              continue
          }
      } //End processSaveAndContinue

      def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] = {

        val formFieldsList: Future[List[FormFieldValidationResult]] = finalResult.map {
          case Left(formFieldResultList) => formFieldResultList
          case Right(formFieldResultList) => formFieldResultList
        }

        val formFieldIds: Future[List[List[FormField]]] = formFieldsList.map(_.map(_.toFormFieldTolerant))
        val formFields: Future[List[FormField]] = formFieldIds.map(_.flatten)

        val formData = formFields.map(formFields => FormData(userId, form.formData.formTypeId, "UTF-8", formFields))

        formData.flatMap(formData =>
          SaveService.updateFormData(formId, formData, tolerant = true).map(response => Ok(Json.toJson(response))))
      }

      val formTemplate: FormTemplate = ???

      val booleanExprs = formTemplate.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)
      val optSectionIdx = BooleanExpr.nextTrueIdxOpt(sectionNumber.value, booleanExprs, data).map(SectionNumber(_))

      val optNextPage = for {
        envelope <- envelopeF
        envelopeId <- envelopeIdF
        formTemplate <- formTemplateF
      } yield optSectionIdx.map(sectionNumber => Page(formId, sectionNumber, formTemplate, repeatService, envelope, envelopeId))

      val actionE: Future[Either[String, FormAction]] = optNextPage.map(optNextPage => FormAction.determineAction(data, optNextPage))

      actionE.flatMap {
        case Right(action) =>

          action match {
            case SaveAndContinue(nextPageToRender) =>
              //              for {
              //                userId <- userIdF
              //                form <- gformConnector.getForm(formId)
              //                x = nextPageToRender.renderPage(data, Some(formId), None)
              //              } yield processSaveAndContinue(userId, form)(x)
              ???

            case SaveAndExit =>
              for {
                userId <- userIdF
                form <- formF
                envelopeId <- envelopeIdF
                result <- processSaveAndExit(userId, form, envelopeId)
              } yield result

            case SaveAndSummary =>
              for {
                userId <- userIdF
                form <- formF
                result <- processSaveAndContinue(userId, form)(Future.successful(Redirect(routes.SummaryGen.summaryById(formId))))
              } yield result

            case AddGroup(groupId) =>
              for {
                _ <- repeatService.appendNewGroup(groupId)
                page <- pageF
                result <- page.renderPage(data, Some(formId), None)
              } yield result

            case RemoveGroup(groupId) =>
              for {
                updatedData <- repeatService.removeGroup(groupId, data)
                page <- pageF
                result <- page.renderPage(updatedData, Some(formId), None)
              } yield result
          }
        case Left(error) => Future.successful(BadRequest(error))
      }

    }

  }

  private def extractedFieldValue(validResult: FormFieldValidationResult): FieldValue = validResult match {
    case FieldOk(fv, _) => fv
    case FieldError(fv, _, _) => fv
    case ComponentField(fv, _) => fv
    case FieldGlobalOk(fv, _) => fv
    case FieldGlobalError(fv, _, _) => fv
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val authConnector = authModule.authConnector
  private lazy val validationService = validationModule.validationService
}
