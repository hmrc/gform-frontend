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

import cats.implicits._
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.ops.FormTemplateIdSyntax
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ UserId => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
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

  private def redirectOrigin(
    formId: FormId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    lang: Option[String]): Result = {
    //TODO get dyanamic sections in here ???
    val originSection = new Origin(formTemplate.sections, retrievals).minSectionNumber
    val originSectionTitle4Ga = sectionTitle4GaFactory(formTemplate.sections(originSection.value).title)
    Redirect(routes.FormController.form(formId, formTemplate._id.to4Ga, originSection, originSectionTitle4Ga, lang))
  }

  def newForm(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      for {
        (form, wasFormFound) <- getOrStartForm(
                                 cache.formTemplate._id,
                                 UserId(cache.retrievals.userDetails.groupIdentifier))
      } yield {
        if (wasFormFound) {
          Ok(continue_form_page(cache.formTemplate, form._id, lang, frontendAppConfig))
        } else redirectOrigin(form._id, cache.retrievals, cache.formTemplate, lang)
      }
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(formTemplateId: FormTemplateId, userId: UserId)(
    implicit hc: HeaderCarrier): Future[(Form, Boolean)] = {
    val formId = FormId(userId, formTemplateId)

    def startForm: Future[Form] =
      for {
        formId <- gformConnector.newForm(formTemplateId, userId)
        form   <- gformConnector.getForm(formId)
      } yield form

    for {
      maybeForm <- gformConnector.maybeForm(formId)
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
      form <- maybeFormExceptSubmitted.map(Future.successful).getOrElse(startForm)
    } yield (form, maybeFormExceptSubmitted.isDefined)
  }

  def form(
    formId: FormId,
    formTemplateId4Ga: FormTemplateId4Ga,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    val fieldData = FormDataHelpers.formDataMap(cache.form.formData)

    for { // format: OFF
      _               <- repeatService.loadData(cache.form.repeatingGroupStructure)
      envelopeF       =  fileUploadService.getEnvelope(cache.form.envelopeId)
      envelope        <- envelopeF
      dynamicSections <- repeatService.getAllSections(cache.formTemplate, fieldData)
      html            <- renderer.renderSection(cache.form, sectionNumber, fieldData, cache.formTemplate, None, envelope, cache.form.envelopeId, None, dynamicSections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      // format: ON
    } yield Ok(html)
  }

  def formError(
    formId: FormId,
    formTemplateId4Ga: FormTemplateId4Ga,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val envelopeF = fileUploadService.getEnvelope(cache.form.envelopeId)
    val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

    for { // format: OFF
      envelope          <- envelopeF
      sections          <- sectionsF
      section           = sections(sectionNumber.value)
      sectionFields     = repeatService.atomicFields(section)
      allFields         =  sections.flatMap(repeatService.atomicFields)
      v                 <- validationService.validateForm(sectionFields, section, cache.form.envelopeId, cache.retrievals)(data)
      errors            = validationService.evaluateValidation(v, allFields, data, envelope)
      html              <- renderer.renderSection(cache.form, sectionNumber, data, cache.formTemplate, Some(errors), envelope, cache.form.envelopeId, Some(v), sections, formMaxAttachmentSizeMB, contentTypes, cache.retrievals, lang)
      // format: ON
    } yield Ok(html)
  }

  def fileUploadPage(
    formId: FormId,
    formTemplateId4Ga: FormTemplateId4Ga,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    fId: String,
    lang: Option[String]) = auth.async(formId) { implicit request => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formId, formTemplateId4Ga, sectionNumber, sectionTitle4Ga, lang)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formId, formTemplateId4Ga, sectionNumber, sectionTitle4Ga, lang)

    def actionUrl(envelopeId: EnvelopeId) =
      s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`.toString}&redirect-error-url=${`redirect-error-url`.toString}"

    Ok(
      snippets.file_upload_page(
        formId,
        sectionNumber,
        sectionTitle4Ga,
        fileId,
        cache.formTemplate,
        actionUrl(cache.form.envelopeId),
        lang,
        frontendAppConfig)
    ).pure[Future]
  }

  val choice = play.api.data.Form(
    play.api.data.Forms.single(
      "decision" -> play.api.data.Forms.nonEmptyText
    ))

  def decision(formTemplateId: FormTemplateId, formId: FormId, lang: Option[String]): Action[AnyContent] =
    auth.async(formId) { implicit request => cache =>
      choice.bindFromRequest.fold(
        _ => Future.successful(BadRequest(continue_form_page(cache.formTemplate, formId, lang, frontendAppConfig))), {
          case "continue" =>
            Future.successful(
              redirectOrigin(formId, cache.retrievals, cache.formTemplate, lang))
          case "delete" => Future.successful(Ok(confirm_delete(cache.formTemplate, formId, lang, frontendAppConfig)))
          case _        => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, lang)))
        }
      )
    }

  def delete(formTemplateId4Ga: FormTemplateId4Ga, formId: FormId, lang: Option[String]): Action[AnyContent] =
    auth.async(formId) { implicit request => cache =>
      gformConnector.deleteForm(formId).map(_ => Redirect(routes.FormController.newForm(cache.formTemplate._id, lang)))
    }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber, lang: Option[String]) = auth.async(formId) {
    implicit request => cache =>
      val envelopeF = for {
        envelope <- fileUploadService.getEnvelope(cache.form.envelopeId)
      } yield envelope

      processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>
        val sectionsF = repeatService.getAllSections(cache.formTemplate, data)

        val formFieldValidationResultsF: Future[Map[FormComponent, FormFieldValidationResult]] = for {
          // format: off
          sections      <- sectionsF
          envelope      <- envelopeF
          section       = sections(sectionNumber.value)
          sectionFields = repeatService.atomicFields(section)
          allFields     = sections.flatMap(repeatService.atomicFields)
          v             <- validationService.validateForm(sectionFields, section, cache.form.envelopeId, cache.retrievals)(data)
          errors        = validationService.evaluateValidation(v, allFields, data, envelope)
          // format: on
        } yield errors.toMap

        val isFormValidF: Future[Boolean] = formFieldValidationResultsF.map(ValidationUtil.isFormValid)
        val fieldsF: Future[Seq[FormField]] = formFieldValidationResultsF.map(_.values.toSeq.flatMap(_.toFormField))
        val formDataF: Future[FormData] = fieldsF.map(FormData(_))

        def processSaveAndContinue(userId: UserId, form: Form, sn: SectionNumber)(
          implicit hc: HeaderCarrier): Future[Result] =
          for {
            formData <- formDataF
            keystore <- repeatService.getData()
            section  <- sectionsF
            userData = UserData(formData, keystore, InProgress)
            _           <- gformConnector.updateUserData(formId, userData)
            isFormValid <- isFormValidF
            sectionTitle4Ga =  sectionTitle4GaFactory(cache.formTemplate.sections(sn.value).title)
            gotoForm        =  routes.FormController.form(formId, cache.formTemplate._id.to4Ga, sn, sectionTitle4Ga, lang)
            gotoFormError   =  routes.FormController
              .formError(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)
          } yield Redirect(if (isFormValid) gotoForm else gotoFormError)

        def processSaveAndSummary(userId: UserId, form: Form)(implicit hc: HeaderCarrier): Future[Result] =
          for {
            // format: OFF
            formData      <- formDataF
            keystore      <- repeatService.getData()
            sections      <- sectionsF
            userData      = UserData(formData, keystore, Summary)
            _             <- gformConnector.updateUserData(formId, userData)
            isFormValid   <- isFormValidF
            originSectionTitle4Ga =  sectionTitle4GaFactory(sections(sectionNumber.value).title)
            gotoSummary   =  routes.SummaryController.summaryById(formId, cache.formTemplate._id.to4Ga, lang)
            gotoFormError =  routes.FormController.formError(formId, cache.formTemplate._id.to4Ga, sectionNumber, originSectionTitle4Ga, lang) // format: ON
          } yield Redirect(if (isFormValid) gotoSummary else gotoFormError)

        def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] =
          for {
            section  <- sectionsF
            keystore <- repeatService.getData()
            formData <- formDataF
            userData = UserData(formData, keystore, InProgress)
            originSection = new Origin(cache.formTemplate.sections, cache.retrievals).minSectionNumber
            originSectionTitle4Ga =  sectionTitle4GaFactory(cache.formTemplate.sections(originSection.value).title)
            result <- gformConnector
                       .updateUserData(formId, userData)
                       .map(response =>
                         Ok(views.html.hardcoded.pages
                           .save_acknowledgement(formId,  cache.formTemplate, originSection, originSectionTitle4Ga, lang, frontendAppConfig)))
          } yield result

        def processBack(userId: UserId, form: Form, sn: SectionNumber): Future[Result] =
          for {
            keystore <- repeatService.getData()
            formData <- formDataF
            userData = UserData(formData, keystore, InProgress)
            sectionTitle4Ga = sectionTitle4GaFactory(cache.formTemplate.sections(sn.value).title)
            result <- gformConnector.updateUserData(formId, userData).map(response =>
                         Redirect(routes.FormController.form(formId, cache.formTemplate._id.to4Ga, sn, sectionTitle4Ga, lang)))
          } yield result

        def processAddGroup(groupId: String): Future[Result] =
          for {
            // format: OFF
            optCompList     <- repeatService.appendNewGroup(groupId)
            dynamicSections <- sectionsF
            keystore        <- repeatService.getData()
            formData        <- formDataF
            userData = UserData(formData, keystore, InProgress)
            _ <- gformConnector.updateUserData(formId, userData)
            sectionTitle4Ga =  sectionTitle4GaFactory(cache.formTemplate.sections(sectionNumber.value).title)
            // format: ON
          } yield
            Redirect(
              routes.FormController
                .form(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)
                .url + anchor(optCompList))

        def anchor(optCompList: Option[List[List[FormComponent]]]) =
          optCompList.map(list => s"#${list.last.head.id}").getOrElse("")

        def processRemoveGroup(idx: Int, groupId: String): Future[Result] =
          for {
            dynamicSections <- sectionsF
            updatedData     <- repeatService.removeGroup(idx, groupId, data)
            repeatingGroups <- repeatService.getAllRepeatingGroups
            optCompList = repeatingGroups.getEntry[RepeatingGroup](groupId)
            envelope <- envelopeF
            section = dynamicSections(sectionNumber.value)
            allFields = dynamicSections.flatMap(repeatService.atomicFields)
            sectionFields = repeatService.atomicFields(section)
            v <- validationService.validateForm(sectionFields, section, cache.form.envelopeId, cache.retrievals)(
                  updatedData)
            errors = validationService.evaluateValidation(v, allFields, updatedData, envelope).toMap
            formData = FormData(errors.values.toSeq.flatMap(_.toFormField))
            keystore <- repeatService.getData()
            userData = UserData(formData, keystore, InProgress)
            _ <- gformConnector.updateUserData(formId, userData)
            sectionTitle4Ga =  sectionTitle4GaFactory(cache.formTemplate.sections(sectionNumber.value).title)
          } yield
            Redirect(
              routes.FormController
                .form(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)
                .url + anchor(optCompList.map(_.list)))

        val userId = UserId(cache.retrievals.userDetails.groupIdentifier)
        val navigationF: Future[Direction] =
          sectionsF.map(sections => new Navigator(sectionNumber, sections, data, cache.retrievals).navigate)

        def redirection(call: Int => Call): Future[Result] =
          for {
            section <- sectionsF
          } yield Redirect(call(section.size))

        navigationF.flatMap {
          // format: OFF
          case SaveAndContinue(sn)       => processSaveAndContinue(userId, cache.form, sn)
          case SaveAndExit               => processSaveAndExit(userId, cache.form, cache.form.envelopeId)
          case Back(sn)                  => processBack(userId, cache.form, sn)
          case SaveAndSummary            => processSaveAndSummary(userId, cache.form)
          case BackToSummary             => processSaveAndSummary(userId, cache.form)
          case AddGroup(groupId)         => processAddGroup(groupId)
          case RemoveGroup(idx, groupId) => processRemoveGroup(idx, groupId)
          // format: ON
        }

      }
  }

  private lazy val firstSection = SectionNumber(0)
  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
