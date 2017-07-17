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
import uk.gov.hmrc.gform.models.{ Page, UserId }
import uk.gov.hmrc.gform.models.components.FieldId
import uk.gov.hmrc.gform.service.{ DeleteService, RepeatingComponentService, RetrieveService }
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components.{ FieldId, FieldValue }
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
  import controllersModule.i18nSupport._

  def newForm(formTypeId: FormTypeId) = auth.async { implicit c =>

    for {// format: OFF
      userId                   <- authConnector.getUserDetails[UserId](authContext)
      (form, wasFormFound)     <- getOrStartForm(formTypeId, userId)
      // format: ON
    } yield result(formTypeId, form._id, wasFormFound, SectionNumber.firstSection)
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(formTypeId: FormTypeId, userId: UserId)(implicit hc: HeaderCarrier): Future[(Form, Boolean)] = {
    val formId = FormId(userId, formTypeId)
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      form <- maybeForm.map(Future.successful(_)).getOrElse(gformConnector.newForm(formTypeId, userId))
    } yield (form, maybeForm.isDefined)
  }

  private def result(formTypeId: FormTypeId, formId: FormId, formFound: Boolean, sectionNumber: SectionNumber)(implicit hc: HeaderCarrier, request: Request[_]) = {
    if (formFound) {
      Ok(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, formId))
    } else {
      Redirect(routes.FormController.form(formId, sectionNumber))
    }
  }

  def form(formId: FormId, sectionNumber: SectionNumber) = auth.async { implicit c =>

    for {// format: OFF
      form           <- gformConnector.getForm(formId)
      fieldData       = getFormData(form)
      formTemplateF   = gformConnector.getFormTemplate(form.formData.formTypeId)
      envelopeF       = fileUploadService.getEnvelope(form.envelopeId)
      formTemplate   <- formTemplateF
      envelope       <- envelopeF
      response       <- Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId).renderPage(fieldData, formId, None)
      // format: ON
    } yield response
  }

  def fileUploadPage(formId: FormId, sectionNumber: SectionNumber, fId: String) = auth.async { implicit c =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, sectionNumber)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, sectionNumber)

    def actionUrl(envelopeId: EnvelopeId) = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`}&redirect-error-url=${`redirect-error-url`}"
    for {
      form <- gformConnector.getForm(formId)
      formTemplate <- gformConnector.getFormTemplate(form.formData.formTypeId)
    } yield Ok(
      uk.gov.hmrc.gform.views.html.file_upload_page(formId, sectionNumber, fileId, formTemplate, actionUrl(form.envelopeId))
    )
  }

  private def getFormData(form: Form): Map[FieldId, List[String]] = form.formData.fields.map(fd => fd.id -> List(fd.value)).toMap

  val choice = play.api.data.Form(play.api.data.Forms.single(
    "decision" -> play.api.data.Forms.nonEmptyText
  ))

  def decision(formTypeId: FormTypeId, formId: FormId): Action[AnyContent] = auth.async { implicit c =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(uk.gov.hmrc.gform.views.html.continue_form_page(formTypeId, formId))),
      {
        case "continue" => Future.successful(Redirect(routes.FormController.form(formId, firstSection /*TODO: once we store section number we could continumer from specific section*/ )))
        case "delete" => gformConnector.deleteForm(formId).map(_ => Redirect(routes.FormController.newForm(formTypeId)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTypeId)))
      }
    )
  }

  def delete(formTypeId: FormTypeId, formId: FormId): Action[AnyContent] = auth.async { implicit c =>
    gformConnector.deleteForm(formId).map { x =>
      Redirect(routes.FormController.newForm(formTypeId))
    }
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

          pageF.flatMap(page => page.renderPage(data, formId, Some(map.get)))

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

      val optNextPage = for {// format: OFF
        envelope     <- envelopeF
        envelopeId   <- envelopeIdF
        formTemplate <- formTemplateF
        booleanExprs  = formTemplate.sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)
        optSectionIdx = BooleanExpr.nextTrueIdxOpt(sectionNumber.value, booleanExprs, data).map(SectionNumber(_))
        // format: ON
      } yield optSectionIdx.map(sectionNumber => Page(formId, sectionNumber, formTemplate, repeatService, envelope, envelopeId))

      val actionE: Future[Either[String, FormAction]] = optNextPage.map(optNextPage => FormAction.determineAction(data, optNextPage))

      actionE.flatMap {
        case Right(action) =>

          action match {
            case SaveAndContinue(nextPageToRender) =>
              for {
                userId <- userIdF
                form <- formF
                result <- processSaveAndContinue(userId, form)(nextPageToRender.renderPage(data, formId, None))
              } yield result

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
                result <- page.renderPage(data, formId, None)
              } yield result

            case RemoveGroup(groupId) =>
              for {
                updatedData <- repeatService.removeGroup(groupId, data)
                page <- pageF
                result <- page.renderPage(updatedData, formId, None)
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
  private lazy val appConfig = configModule.appConfig

}
