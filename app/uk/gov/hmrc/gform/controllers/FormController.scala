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
import play.api.Logger
import play.api.mvc.{ Action, AnyContent, Request, Result }
import play.api.libs.json.Json
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadModule }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.models.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.prepop.PrepopModule
import uk.gov.hmrc.gform.service.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
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
    validationModule: ValidationModule,
    prePopModule: PrepopModule
) extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def newForm(formTemplateId: FormTemplateId) = authentication.async(Some(formTemplateId)) { implicit c =>
    result(formTemplateId, UserId(retrievals.userDetails.groupIdentifier))
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(formTemplateId: FormTemplateId, userId: UserId)(implicit hc: HeaderCarrier): Future[(Form, Boolean)] = {
    val formId = FormId(userId, formTemplateId)

    def startForm: Future[Form] = for {
      formId <- gformConnector.newForm(formTemplateId, userId)
      form <- gformConnector.getForm(formId)
    } yield form

    for {
      maybeForm <- gformConnector.maybeForm(formId)
      form <- maybeForm.map(Future.successful).getOrElse(startForm)
    } yield (form, maybeForm.isDefined)
  }

  private def result(formTemplateId: FormTemplateId, userId: UserId)(implicit hc: HeaderCarrier, request: Request[_]) = {
    for {
      (form, wasFormFound) <- getOrStartForm(formTemplateId, userId)
    } yield {
      if (wasFormFound) {
        Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.continue_form_page(formTemplateId, form._id))
      } else {
        Redirect(routes.FormController.form(form._id, SectionNumber.firstSection))
      }
    }
  }

  def form(formId: FormId, sectionNumber: SectionNumber) = authentication.async(formIdOpt = Some(formId)) { implicit c =>

    val form = maybeForm.get
    val fieldData = getFormData(form)

    for {// format: OFF
      _               <- repeatService.loadData(form.repeatingGroupStructure)
      envelopeF       =  fileUploadService.getEnvelope(form.envelopeId)
      envelope        <- envelopeF
      dynamicSections <- repeatService.getAllSections(formTemplate, fieldData)
      response        <- Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId, prepopService).renderPage(fieldData, formId, None, dynamicSections)
      // format: ON
    } yield response
  }

  def formError(formId: FormId, sectionNumber: SectionNumber) = authentication.async(formIdOpt = Some(formId)) { implicit c =>

    val form = maybeForm.get
    val fieldData = getFormData(form)
    val envelopeF = fileUploadService.getEnvelope(form.envelopeId)

    def getErrors(sections: List[Section], data: Map[FieldId, Seq[String]], envelope: Envelope, envelopeId: EnvelopeId) = {
      Logger.debug(data + "this is data in get errors")
      val fields = sections(sectionNumber.value).atomicFields(repeatService)
      val allFields = sections.flatMap(_.atomicFields(repeatService))
      Future.sequence(fields.map(fv => validationService.validateComponents(fv, data, envelopeId))).map(Monoid[ValidatedType].combineAll).map { validationResult =>
        ValidationUtil.evaluateValidationResult(allFields, validationResult, data, envelope) match {
          case Left(x) => x.map((validResult: FormFieldValidationResult) => extractedFieldValue(validResult) -> validResult).toMap
          case Right(y) => Map.empty[FieldValue, FormFieldValidationResult]
        }
      }
    }

    for {// format: OFF
      envelope        <- envelopeF
      dynamicSections <- repeatService.getAllSections(formTemplate, fieldData)
      errors          <- getErrors(dynamicSections, fieldData, envelope, form.envelopeId)
      response        <- Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId, prepopService).renderPage(fieldData, formId, Some(errors.get), dynamicSections)
      // format: ON
    } yield response
  }

  def fileUploadPage(formId: FormId, sectionNumber: SectionNumber, fId: String) = authentication.async(formIdOpt = Some(formId)) { implicit c =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, sectionNumber)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, sectionNumber)

    def actionUrl(envelopeId: EnvelopeId) = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`}&redirect-error-url=${`redirect-error-url`}"

    val form = maybeForm.get
    Future.successful(Ok(
      uk.gov.hmrc.gform.views.html.file_upload_page(formId, sectionNumber, fileId, formTemplate, actionUrl(form.envelopeId))
    ))
  }

  private def getFormData(form: Form): Map[FieldId, List[String]] = form.formData.fields.map(fd => fd.id -> List(fd.value)).toMap

  val choice = play.api.data.Form(play.api.data.Forms.single(
    "decision" -> play.api.data.Forms.nonEmptyText
  ))

  def decision(formTemplateId: FormTemplateId, formId: FormId): Action[AnyContent] = authentication.async(Some(formTemplateId)) { implicit c =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(uk.gov.hmrc.gform.views.html.hardcoded.pages.continue_form_page(formTemplateId, formId))),
      {
        case "continue" => Future.successful(Redirect(routes.FormController.form(formId, firstSection /*TODO: once we store section number we could continumer from specific section*/ )))
        case "delete" => Future.successful(Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.confirm_delete(formTemplateId, formId)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTemplateId)))
      }
    )
  }

  def delete(formTemplateId: FormTemplateId, formId: FormId): Action[AnyContent] = authentication.async(Some(formTemplateId)) { implicit c =>
    gformConnector.deleteForm(formId).map { x =>
      Redirect(routes.FormController.newForm(formTemplateId))
    }
  }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber) = authentication.async(formIdOpt = Some(formId)) { implicit c =>

    val form = maybeForm.get
    val envelopeF = for {
      envelope <- fileUploadService.getEnvelope(form.envelopeId)
    } yield envelope

    val pageF = for {
      envelope <- envelopeF
    } yield Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId, prepopService)

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val sectionsF = for {
        sections <- repeatService.getAllSections(formTemplate, data)
      } yield sections

      val sectionFieldsF = for {
        sections <- sectionsF
        section = sections(sectionNumber.value)
      } yield section.atomicFields(repeatService)

      val allFieldsInTemplateF = for {
        sections <- sectionsF
        allFieldsInTemplate = sections.flatMap(_.atomicFields(repeatService))
      } yield allFieldsInTemplate

      val validatedDataResultF: Future[ValidatedType] = for {
        sectionFields <- sectionFieldsF
        validatedData <- Future.sequence(sectionFields.map(fv =>
          validationService.validateComponents(fv, data, form.envelopeId)))
      } yield Monoid[ValidatedType].combineAll(validatedData)

      val finalResult: Future[Either[List[FormFieldValidationResult], List[FormFieldValidationResult]]] =
        for {
          validatedDataResult <- validatedDataResultF
          envelope <- fileUploadService.getEnvelope(form.envelopeId)
          allFieldsInTemplate <- allFieldsInTemplateF
        } yield ValidationUtil.evaluateValidationResult(allFieldsInTemplate, validatedDataResult, data, envelope)

      def processSaveAndContinue(userId: UserId, form: Form)(continueF: Future[Result])(implicit hc: HeaderCarrier): Future[Result] = finalResult.flatMap {
        case Left(listFormValidation) =>

          val formData = FormData(listFormValidation.flatMap(_.toFormFieldTolerant))

          for {
            keystore <- repeatService.getData()
            userData = UserData(formData, keystore)
            _ <- gformConnector.updateUserData(formId, userData)
          } yield Redirect(uk.gov.hmrc.gform.controllers.routes.FormController.formError(formId, sectionNumber))

        case Right(listFormValidation) =>

          val formFieldIds = listFormValidation.map(_.toFormField)
          val formFields = formFieldIds.sequenceU.map(_.flatten).toList.flatten

          val formData = FormData(formFields)

          val outCome: SaveResult => Future[Result] = {
            case SaveResult(_, Some(error)) => Future.successful(BadRequest(error))
            case _ => continueF
          }

          //TODO figure out if we should save the structure constantly or should we figure out when they leave the form another way other than the two buttons.
          for {
            keystore <- repeatService.getData()
            userData = UserData(formData, keystore)
            _ <- gformConnector.updateUserData(formId, userData)
            continue <- continueF
          } yield continue
      } //End processSaveAndContinue

      def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] = {

        val formFieldsList: Future[List[FormFieldValidationResult]] = finalResult.map {
          case Left(formFieldResultList) => formFieldResultList
          case Right(formFieldResultList) => formFieldResultList
        }

        val formFieldIds: Future[List[List[FormField]]] = formFieldsList.map(_.map(_.toFormFieldTolerant))
        val formFields: Future[List[FormField]] = formFieldIds.map(_.flatten)

        val formDataF = formFields.map(formFields => FormData(formFields))

        for {
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore)

          result <- gformConnector.updateUserData(formId, userData).map(response => Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.save_acknowledgement(formId, form.formTemplateId)))
        } yield result
      }

      def processBack(userId: UserId, form: Form)(continue: Future[Result]): Future[Result] = {
        val formFieldsList: Future[List[FormFieldValidationResult]] = finalResult.map {
          case Left(formFieldResultList) => formFieldResultList
          case Right(formFieldResultList) => formFieldResultList
        }

        val formFieldIds: Future[List[List[FormField]]] = formFieldsList.map(_.map(_.toFormFieldTolerant))
        val formFields: Future[List[FormField]] = formFieldIds.map(_.flatten)

        val formDataF = formFields.map(formFields => FormData(formFields))

        for {
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore)
          result <- gformConnector.updateUserData(formId, userData).flatMap(response => continue)
        } yield result

      }

      val optNextPage = for {// format: OFF
        envelope     <- envelopeF
        sections     <- sectionsF
        booleanExprs  = sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)
        optSectionIdx = BooleanExpr.nextTrueIdxOpt(sectionNumber, booleanExprs, data)
        // format: ON
      } yield optSectionIdx.map(sectionNumber => Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId, prepopService))

      val optBackPage = for {// format: OFF
        envelope     <- envelopeF
        sections     <- sectionsF
        booleanExprs  = sections.map(_.includeIf.getOrElse(IncludeIf(IsTrue)).expr)
        optSectionIdx = BooleanExpr.backTrueIdxOpt(sectionNumber, booleanExprs, data)
        // format: ON
      } yield optSectionIdx.map(sectionNumber => Page(formId, sectionNumber, formTemplate, repeatService, envelope, form.envelopeId, prepopService))

      val actionE: Future[Either[String, FormAction]] = for {
        optNextPage <- optNextPage
        optBackPage <- optBackPage
      } yield FormAction.determineAction(data, optNextPage, optBackPage)

      val userId = UserId(retrievals.userDetails.groupIdentifier)

      actionE.flatMap {
        case Right(action) =>
          action match {
            case SaveAndContinue(nextPageToRender) =>
              for {
                result <- processSaveAndContinue(userId, form)(Future.successful(Redirect(uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, nextPageToRender.sectionNumber))))
              } yield result
            case SaveAndExit =>
              for {
                result <- processSaveAndExit(userId, form, form.envelopeId)
              } yield result
            case Back(lastPage) =>
              for {
                result <- processBack(userId, form)(Future.successful(Redirect(uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, lastPage.sectionNumber))))
              } yield result
            case SaveAndSummary =>
              for {
                result <- processSaveAndContinue(userId, form)(Future.successful(Redirect(routes.SummaryGen.summaryById(formId))))
              } yield result

            case AddGroup(groupId) =>
              for {
                _ <- repeatService.appendNewGroup(groupId)
                page <- pageF
                dynamicSections <- sectionsF
                result <- page.renderPage(data, formId, None, dynamicSections)
              } yield result

            case RemoveGroup(groupId) =>
              for {
                updatedData <- repeatService.removeGroup(groupId, data)
                page <- pageF
                dynamicSections <- sectionsF
                result <- page.renderPage(updatedData, formId, None, dynamicSections)
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

  private lazy val prepopService = prePopModule.prepopService
  private lazy val authentication = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val validationService = validationModule.validationService
  private lazy val appConfig = configModule.appConfig
}
