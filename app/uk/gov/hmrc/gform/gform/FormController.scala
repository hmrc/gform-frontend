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

package uk.gov.hmrc.gform.gform

import cats.implicits._
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, FormTemplate, FormTemplateId, SectionNumber }
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService, ValidationUtil }
import uk.gov.hmrc.gform.views.html.form._
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.gform.views
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

class FormController(
    appConfig: AppConfig,
    frontendAppConfig: FrontendAppConfig,
    i18nSupport: I18nSupport,
    auth: AuthenticatedRequestActions,
    repeatService: RepeatingComponentService,
    fileUploadService: FileUploadService,
    validationService: ValidationService,
    renderer: SectionRenderingService,
    gformConnector: GformConnector
) extends FrontendController {

  import i18nSupport._

  def newForm(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) { implicit request => cache =>
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
        Ok(continue_form_page(formTemplate, form._id, lang, frontendAppConfig))
      } else {
        Redirect(routes.FormController.form(form._id, formTemplate._id, SectionNumber.firstSection, formTemplate.sections.size, lang))
      }
    }
  }

  def form(formId: FormId, formTemplateId4Ga: FormTemplateId, sectionNumber: SectionNumber, totalSections: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    // TODO: Handle cases when the form is no longer marked as InProgress
    val fieldData = FormDataHelpers.formDataMap(cache.form.formData)

    for {// format: OFF
      _               <- repeatService.loadData(cache.form.repeatingGroupStructure)
      envelopeF       =  fileUploadService.getEnvelope(cache.form.envelopeId)
      envelope        <- envelopeF
      dynamicSections <- repeatService.getAllSections(cache.formTemplate, fieldData)
      html            <- renderer.renderSection(cache.form, sectionNumber, fieldData, cache.formTemplate, None, envelope, cache.form.envelopeId, None, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      // format: ON
    } yield Ok(html)
  }

  def formError(formId: FormId, formTemplateId4Ga: FormTemplateId, sectionNumber: SectionNumber, totalPage: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

    for {// format: OFF
      envelope          <- envelopeF
      sections          <- sectionsF
      section           = sections(sectionNumber.value)
      sectionFields     = repeatService.atomicFields(section)
      allFields         =  sections.flatMap(repeatService.atomicFields)
      v                 <- validationService.validateForm(sectionFields, section, cache.form.envelopeId)(data)
      errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      html              <- renderer.renderSection(cache.form, sectionNumber, data, cache.formTemplate, Some(errors), envelope, cache.form.envelopeId, Some(v), sections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      // format: ON
    } yield Ok(html)
  }

  def fileUploadPage(formId: FormId, formTemplateId4Ga: FormTemplateId, sectionNumber: SectionNumber, fId: String, totalSection: Int, lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, formTemplateId4Ga, sectionNumber, totalSection, lang)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController.form(formId, formTemplateId4Ga, sectionNumber, totalSection, lang)

    def actionUrl(envelopeId: EnvelopeId) = s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`}&redirect-error-url=${`redirect-error-url`}"

    Ok(
      snippets.file_upload_page(formId, sectionNumber, fileId, cache.formTemplate, actionUrl(cache.form.envelopeId), totalSection, lang, frontendAppConfig)
    ).pure[Future]
  }

  val choice = play.api.data.Form(play.api.data.Forms.single(
    "decision" -> play.api.data.Forms.nonEmptyText
  ))

  def decision(formTemplateId: FormTemplateId, formId: FormId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    choice.bindFromRequest.fold(
      _ => Future.successful(BadRequest(continue_form_page(cache.formTemplate, formId, lang, frontendAppConfig))),
      {
        case "continue" => Future.successful(Redirect(routes.FormController.form(formId, formTemplateId, firstSection, cache.formTemplate.sections.size, lang))) //TODO get dyanmic sections in here ???
        case "delete" => Future.successful(Ok(confirm_delete(cache.formTemplate, formId, lang, frontendAppConfig)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, lang)))
      }
    )
  }

  def delete(formTemplateId: FormTemplateId, formId: FormId, lang: Option[String]): Action[AnyContent] = auth.async(formId) { implicit request => cache =>
    gformConnector.deleteForm(formId).map(_ => Redirect(routes.FormController.newForm(formTemplateId, lang)))
  }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber, lang: Option[String]) = auth.async(formId) { implicit request => cache =>

    val envelopeF = for {
      envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
    } yield envelope

    processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>

      val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

      val formFieldValidationResultsF: Future[Map[FormComponent, FormFieldValidationResult]] = for { // format: OFF
        sections          <- sectionsF
        envelope          <- envelopeF
        section           = sections(sectionNumber.value)
        sectionFields     = repeatService.atomicFields(section)
        allFields         = sections.flatMap(repeatService.atomicFields)
        v                 <- validationService.validateForm(sectionFields, section, cache.form.envelopeId)(data)
        errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      // format: OFF
      } yield errors.toMap

      val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(ValidationUtil.isFormValid)
      val fieldsF: Future[Seq[FormField]] = formFieldValidationResultsF.map(_.values.toSeq.flatMap(_.toFormField))
      val formDataF: Future[FormData] = fieldsF.map(FormData(_))

      def processSaveAndContinue(userId: UserId, form: Form, nextPage: Result)(implicit hc: HeaderCarrier): Future[Result] =
        for {
          formData <- formDataF
          keystore <- repeatService.getData()
          section <- sectionsF
          userData = UserData(formData, keystore, InProgress)
          _ <- gformConnector.updateUserData(formId, userData)
          isFormValid <- isFormValidF
        } yield if (isFormValid) nextPage else Redirect(routes.FormController.formError(formId, cache.formTemplate._id, sectionNumber, section.size, lang))

      def processSaveAndSummary(userId: UserId, form: Form)(implicit hc: HeaderCarrier): Future[Result] = {

          for {
            // format: OFF
            formData      <- formDataF
            keystore      <- repeatService.getData()
            section       <- sectionsF
            userData      = UserData(formData, keystore, Summary)
            _             <- gformConnector.updateUserData(formId, userData)
            isFormValid   <- isFormValidF
            gotoSummary   = Redirect(routes.SummaryController.summaryById(formId, cache.formTemplate._id, lang))
            gotoFormError = Redirect(routes.FormController.formError(formId, cache.formTemplate._id, sectionNumber, section.size, lang))
          // format: ON
        } yield if (isFormValid) gotoSummary else gotoFormError
      }

      def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] = {

        for {
          section <- sectionsF
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore, InProgress)

          result <- gformConnector.updateUserData(formId, userData).map(response => Ok(views.html.hardcoded.pages.save_acknowledgement(formId, cache.formTemplate, section.size, lang, frontendAppConfig)))
        } yield result
      }

      def processBack(userId: UserId, form: Form)(continue: Future[Result]): Future[Result] = {

        for {
          keystore <- repeatService.getData()
          formData <- formDataF
          userData = UserData(formData, keystore, InProgress)
          result <- gformConnector.updateUserData(formId, userData).flatMap(response => continue)
        } yield result
      }

      def processAddGroup(groupId: String): Future[Result] = for {
        //format OFF
        optCompList <- repeatService.appendNewGroup(groupId)
        dynamicSections <- sectionsF
        keystore <- repeatService.getData()
        formData <- formDataF
        userData = UserData(formData, keystore, InProgress)
        _ <- gformConnector.updateUserData(formId, userData)
        //format ON
      } yield Redirect(routes.FormController.form(formId, cache.formTemplate._id, sectionNumber, dynamicSections.size, lang).url + anchor(optCompList))

      def anchor(optCompList: Option[List[List[FormComponent]]]) =
        optCompList.map(list => s"#${list.last.head.id}").getOrElse("")

      def processRemoveGroup(idx: Int, groupId: String): Future[Result] = for {
        dynamicSections <- sectionsF
        updatedData <- repeatService.removeGroup(idx, groupId, data)
        repeatingGroups <- repeatService.getAllRepeatingGroups
        optCompList = repeatingGroups.getEntry[RepeatingGroup](groupId)
        envelope <- envelopeF
        section = dynamicSections(sectionNumber.value)
        allFields = dynamicSections.flatMap(repeatService.atomicFields)
        sectionFields = repeatService.atomicFields(section)
        v <- validationService.validateForm(sectionFields, section, cache.form.envelopeId)(updatedData)
        errors = validationService.evaluateValidation(v, allFields, updatedData, envelope).toMap
        formData = FormData(errors.values.toSeq.flatMap(_.toFormField))
        keystore <- repeatService.getData()
        userData = UserData(formData, keystore, InProgress)
        _ <- gformConnector.updateUserData(formId, userData)
      } yield Redirect(routes.FormController.form(formId, cache.formTemplate._id, sectionNumber, dynamicSections.size, lang).url + anchor(optCompList.map(_.list)))

      val userId = UserId(cache.retrievals.userDetails.groupIdentifier)
      val navigationF: Future[Direction] = sectionsF.map(sections => new Navigator(sectionNumber, sections, data).navigate)

      def redirection(call: Int => Call): Future[Result] = {
        for {
          section <- sectionsF
        } yield Redirect(call(section.size))
      }

      navigationF.flatMap {
        // format: OFF
        case SaveAndContinue(sn)            => redirection(routes.FormController.form(formId, cache.formTemplate._id, sn, _, lang)).flatMap(x => processSaveAndContinue(userId, cache.form, x))
        case SaveAndExit                    => processSaveAndExit(userId, cache.form, cache.form.envelopeId)
        case Back(sn)                       => processBack(userId, cache.form)(redirection(routes.FormController.form(formId, cache.formTemplate._id, sn, _, lang)))
        case SaveAndSummary                 => processSaveAndSummary(userId, cache.form)
        case BackToSummary                  => processSaveAndSummary(userId, cache.form)
        case AddGroup(groupId)              => processAddGroup(groupId)
        case RemoveGroup(idx, groupId)      => processRemoveGroup(idx, groupId)
        // format: ON
      }

    }
  }

  private lazy val firstSection = SectionNumber(0)
  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
