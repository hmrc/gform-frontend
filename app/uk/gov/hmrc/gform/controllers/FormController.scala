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

package uk.gov.hmrc.gform
package controllers

import javax.inject.Inject

import helpers._
import cats._
import cats.data.{ EitherT, Validated }
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.all._
import cats.syntax.all._
import play.api.Logger
import play.api.mvc._
import uk.gov.hmrc.gform.auth.AuthModule
import uk.gov.hmrc.gform.config.ConfigModule
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadModule }
import uk.gov.hmrc.gform.gformbackend.GformBackendModule
import uk.gov.hmrc.gform.validation.ValidationUtil.{ GformError, ValidatedType }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.prepop.PrepopModule
import uk.gov.hmrc.gform.service.{ RepeatingComponentService, SectionRenderingService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationModule }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import views.html.hardcoded.pages._
import views.html._

class FormController @Inject() (
    controllersModule: ControllersModule,
    gformBackendModule: GformBackendModule,
    configModule: ConfigModule,
    repeatService: RepeatingComponentService,
    fileUploadModule: FileUploadModule,
    authModule: AuthModule,
    validationModule: ValidationModule,
    prePopModule: PrepopModule,
    renderer: SectionRenderingService
) extends FrontendController {

  import controllersModule.i18nSupport._

  def newForm(formTemplateId: FormTemplateId, lang: Option[String]) = authentication.async(formTemplateId) { implicit request => cache =>
    result(cache.formTemplate, UserId(cache.retrievals.userDetails.groupIdentifier), lang)
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

  private def result(formTemplate: FormTemplate, userId: UserId, lang: Option[String])(implicit hc: HeaderCarrier, request: Request[_]) = {
    for {
      (form, wasFormFound) <- getOrStartForm(formTemplate._id, userId)
    } yield {
      if (wasFormFound) {
        Ok(continue_form_page(formTemplate._id, form._id, lang))
      } else {
        Redirect(routes.FormController.form(form._id, formTemplate._id, SectionNumber.firstSection, formTemplate.sections.size, lang))
      }
    }
  }

  def form(formId: FormId, formTemplateId4Ga: FormTemplateId, sectionNumber: SectionNumber, totalSections: Int, lang: Option[String]) = authentication.async(formId) { implicit request => cache =>
    val fieldData = FormDataHelpers.formDataMap(cache.form.formData)

    for {// format: OFF
      _               <- repeatService.loadData(cache.form.repeatingGroupStructure)
      envelopeF       =  fileUploadService.getEnvelope(cache.form.envelopeId)
      envelope        <- envelopeF
      dynamicSections <- repeatService.getAllSections(cache.formTemplate, fieldData)
      html            <- renderer.renderSection(formId, sectionNumber, fieldData, cache.formTemplate, None, envelope, cache.form.envelopeId, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      // format: ON
    } yield Ok(html)
  }

  def formError(formId: FormId, formTemplateId4Ga: FormTemplateId, sectionNumber: SectionNumber, totalPage: Int, lang: Option[String]) = authentication.async(formId) { implicit request => cache =>

    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

    for {// format: OFF
      envelope          <- envelopeF
      sections          <- sectionsF
      section           = sections(sectionNumber.value)
      sectionFields     = repeatService.atomicFields(section)
      allFields         =  sections.flatMap(repeatService.atomicFields)
      componentsErrors  = validationService.validateComponents(sectionFields, data, cache.form.envelopeId)
      sectionErrors     = validationService.validateUsingValidators(section, data)
      v                 <- validationService.sequenceValidations(componentsErrors, sectionErrors)
      errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      html              <- renderer.renderSection(formId, sectionNumber, data, cache.formTemplate, Some(errors.get), envelope, cache.form.envelopeId, sections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      // format: ON
    } yield Ok(html)
  }

  def fileUploadPage(formId: FormId, formTemplateId4Ga: FormTemplateId, sectionNumber: SectionNumber, fId: String, totalSection: Int, lang: Option[String]) = authentication.async(formId) { implicit request => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, formTemplateId4Ga, sectionNumber, totalSection, lang)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, formTemplateId4Ga, sectionNumber, totalSection, lang)

    def actionUrl(envelopeId: EnvelopeId) = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`}&redirect-error-url=${`redirect-error-url`}"

    Future.successful(Ok(
      file_upload_page(formId, sectionNumber, fileId, cache.formTemplate, actionUrl(cache.form.envelopeId), totalSection, lang)
    ))
  }

  val choice = play.api.data.Form(play.api.data.Forms.single(
    "decision" -> play.api.data.Forms.nonEmptyText
  ))

  def decision(formTemplateId: FormTemplateId, formId: FormId, lang: Option[String]): Action[AnyContent] = authentication.async(formId) { implicit request => cache =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(continue_form_page(formTemplateId, formId, lang))),
      {
        case "continue" => Future.successful(Redirect(routes.FormController.form(formId, formTemplateId, firstSection, cache.formTemplate.sections.size, lang))) //TODO get dyanmic sections in here ???
        case "delete" => Future.successful(Ok(confirm_delete(formTemplateId, formId, lang)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, lang)))
      }
    )
  }

  def delete(formTemplateId: FormTemplateId, formId: FormId, lang: Option[String]): Action[AnyContent] = authentication.async(formId) { implicit request => cache =>
    gformConnector.deleteForm(formId).map(_ => Redirect(routes.FormController.newForm(formTemplateId, lang)))
  }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber, lang: Option[String]) = authentication.async(formId) { implicit request => cache =>

    val envelopeF = for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
    } yield envelope

    processResponseDataFromBody(request) { (data: Map[FieldId, Seq[String]]) =>

      val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

      val formFieldValidationResultsF: Future[Map[FieldValue, FormFieldValidationResult]] = for { // format: OFF
        sections          <- sectionsF
        envelope          <- envelopeF
        section           = sections(sectionNumber.value)
        sectionFields     = repeatService.atomicFields(section)
        allFields         = sections.flatMap(repeatService.atomicFields)
        componentsErrors  = validationService.validateComponents(sectionFields, data, cache.form.envelopeId)
        sectionErrors     = validationService.validateUsingValidators(section, data)
        v                 <- validationService.sequenceValidations(componentsErrors, sectionErrors)
        errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      // format: OFF
      } yield errors

      val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(!_.values.view.exists(!_.isOk))
      val fieldsF: Future[Seq[FormField]] = formFieldValidationResultsF.map(_.values.toSeq.flatMap(_.toFormField))
      val formDataF: Future[FormData] = fieldsF.map(FormData(_))

      def processSaveAndContinue(userId: UserId, form: Form, nextPage: Result)(implicit hc: HeaderCarrier): Future[Result] =
        for {
          formData <- formDataF
          keystore <- repeatService.getData()
          section <- sectionsF
          userData = UserData(formData, keystore)
          _ <- gformConnector.updateUserData(formId, userData)
          isFormValid <- isFormValidF
        } yield if (isFormValid) nextPage else Redirect(routes.FormController.formError(formId, cache.formTemplate._id, sectionNumber, section.size, lang))

      def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] = {

        for {
          section <- sectionsF
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore)

          result <- gformConnector.updateUserData(formId, userData).map(response => Ok(views.html.hardcoded.pages.save_acknowledgement(formId, form.formTemplateId, section.size, lang)))
        } yield result
      }

      def processBack(userId: UserId, form: Form)(continue: Future[Result]): Future[Result] = {

        for {
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore)
          result <- gformConnector.updateUserData(formId, userData).flatMap(response => continue)
        } yield result

      }

      def processAddGroup(groupId: String): Future[Result] = for {
        _ <- repeatService.appendNewGroup(groupId)
        envelope <- envelopeF
        dynamicSections <- sectionsF
        html <- renderer.renderSection(formId, sectionNumber, data, cache.formTemplate, None, envelope, cache.form.envelopeId, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      } yield Ok(html)

      def processRemoveGroup(groupId: String): Future[Result] = for {
        updatedData <- repeatService.removeGroup(groupId, data)
        envelope <- envelopeF
        dynamicSections <- sectionsF
        html <- renderer.renderSection(formId, sectionNumber, updatedData, cache.formTemplate, None, envelope, cache.form.envelopeId, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      } yield Ok(html)

      val userId = UserId(cache.retrievals.userDetails.groupIdentifier)
      val navigationF: Future[Direction] = sectionsF.map(sections => new Navigator(sectionNumber, sections, data).navigate)

      def redirection(call: Int => Call): Future[Result] = {
        for {
          section <- sectionsF
        } yield Redirect(call(section.size))
      }

      navigationF.flatMap {
        // format: OFF
        case SaveAndContinue(sn)            => redirection(uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, cache.formTemplate._id, sn, _, lang)).flatMap(x => processSaveAndContinue(userId, cache.form, x))
        case SaveAndExit                    => processSaveAndExit(userId, cache.form, cache.form.envelopeId)
        case Back(sn)                       => processBack(userId, cache.form)(redirection(uk.gov.hmrc.gform.controllers.routes.FormController.form(formId, cache.formTemplate._id, sn, _, lang)))
        case SaveAndSummary                 => processSaveAndContinue(userId, cache.form, Redirect(routes.SummaryGen.summaryById(formId, cache.formTemplate._id, lang)))
        case AddGroup(groupId)              => processAddGroup(groupId)
        case RemoveGroup(groupId)           => processRemoveGroup(groupId)
        // format: ON
      }

    }
  }

  private lazy val authentication = controllersModule.authenticatedRequestActions
  private lazy val gformConnector = gformBackendModule.gformConnector
  private lazy val firstSection = SectionNumber(0)
  private lazy val fileUploadService = fileUploadModule.fileUploadService
  private lazy val validationService = validationModule.validationService
  private lazy val appConfig = configModule.appConfig
  private lazy val formMaxAttachmentSizeMB = configModule.appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = configModule.appConfig.contentTypes
}
