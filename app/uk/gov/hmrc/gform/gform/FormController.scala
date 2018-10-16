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

import cats.data.Validated.Valid
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.semigroup._
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.auth.core.AffinityGroup
import uk.gov.hmrc.gform.auth.models.{ MaterialisedRetrievals, UserDetails }
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.AgentAccessCode
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.form.FormData._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ UserId => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService, ValidationUtil }
import uk.gov.hmrc.gform.views.html.form._
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.gform.views
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future
import uk.gov.hmrc.http.HeaderCarrier

case class AccessCodeForm(accessCode: Option[String], accessOption: String)

class FormController(
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  fileUploadService: FileUploadService,
  validationService: ValidationService,
  renderer: SectionRenderingService,
  gformConnector: GformConnector,
  recalculation: Recalculation[Future, Throwable]
) extends FrontendController {

  import i18nSupport._

  private def redirectOrigin(
    maybeAccessCode: Option[AccessCode],
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate,
    lang: Option[String]): Result = {
    //TODO get dyanamic sections in here ???

    val originSection = new Origin(formTemplate.sections).minSectionNumber
    val sectionTitle4Ga = sectionTitle4GaFactory(formTemplate.sections(originSection.value).title)
    Redirect(
      routes.FormController
        .form(formTemplate._id, maybeAccessCode, originSection, sectionTitle4Ga, lang))
  }

  def dashboard(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      (cache.formTemplate.draftRetrievalMethod, cache.retrievals.affinityGroup) match {
        case (Some(FormAccessCodeForAgents), Some(AffinityGroup.Agent)) =>
          Future.successful(Ok(access_code_start(cache.formTemplate, AgentAccessCode.form, lang, frontendAppConfig)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, lang)))
      }
  }

  def newFormAgent(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      val accessCode = AccessCode(AgentAccessCode.random.value)
      for {
        _ <- startForm(formTemplateId, cache.retrievals.userDetails, Some(accessCode))
      } yield
        Redirect(routes.FormController.showAccessCode(formTemplateId, lang))
          .flashing(AgentAccessCode.key -> accessCode.value)
  }

  def showAccessCode(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      request.flash.get(AgentAccessCode.key) match {
        case Some(accessCode) =>
          Future.successful(
            Ok(start_new_form(cache.formTemplate, AgentAccessCode(accessCode), lang, frontendAppConfig)))
        case None => Future.successful(Redirect(routes.FormController.dashboard(formTemplateId, lang)))
      }
  }

  def newForm(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      {
        for {
          (maybeAccessCode, wasFormFound) <- getOrStartForm(formTemplateId, cache.retrievals.userDetails, None)
        } yield
          if (wasFormFound) {
            Ok(continue_form_page(cache.formTemplate, maybeAccessCode, lang, frontendAppConfig))
          } else redirectOrigin(maybeAccessCode, cache.retrievals, cache.formTemplate, lang)
      }
  }

  def newFormPost(formTemplateId: FormTemplateId, lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId) { implicit request => cache =>
      AgentAccessCode.form.bindFromRequest.fold(
        hasErrors =>
          Future.successful(BadRequest(access_code_start(cache.formTemplate, hasErrors, lang, frontendAppConfig))),
        accessCodeF => {
          accessCodeF.accessOption match {
            case AgentAccessCode.optionNew =>
              Future.successful(Redirect(routes.FormController.newFormAgent(formTemplateId, lang)))
            case AgentAccessCode.optionAccess => {
              val maybeAccessCode: Option[AccessCode] = accessCodeF.accessCode.map(a => AccessCode(a))
              for {
                maybeForm <- getForm(FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode))
              } yield
                maybeForm match {
                  case Some(_) =>
                    redirectOrigin(maybeAccessCode, cache.retrievals, cache.formTemplate, lang)
                  case None =>
                    BadRequest(
                      access_code_start(
                        cache.formTemplate,
                        AgentAccessCode.form.bindFromRequest().withError(AgentAccessCode.key, "error.notfound"),
                        lang,
                        frontendAppConfig
                      )
                    )
                }
            }
          }
        }
      )
    }

  private def getForm(formId: FormId)(implicit hc: HeaderCarrier): Future[Option[Form]] =
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
    } yield maybeFormExceptSubmitted

  private def startFreshForm(
    formTemplateId: FormTemplateId,
    userDetails: UserDetails,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[Option[AccessCode]] =
    for {
      _ <- gformConnector.newForm(formTemplateId, UserId(userDetails.groupIdentifier), maybeAccessCode)
    } yield maybeAccessCode

  private def startForm(formTemplateId: FormTemplateId, userDetails: UserDetails, maybeAccessCode: Option[AccessCode])(
    implicit hc: HeaderCarrier): Future[Option[AccessCode]] = {
    val formId = FormId(userDetails, formTemplateId, None)
    def formIdAlreadyExists = Future.failed(new Exception(s"Form $formId already exists"))
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      maybeAccessCodeResult <- if (maybeForm.isDefined) formIdAlreadyExists
                              else startFreshForm(formTemplateId, userDetails, maybeAccessCode)
    } yield maybeAccessCodeResult
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(
    formTemplateId: FormTemplateId,
    userDetails: UserDetails,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[(Option[AccessCode], Boolean)] =
    for {
      maybeForm <- gformConnector.maybeForm(FormId(userDetails, formTemplateId, None))
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
      maybeNoAccessCode = maybeFormExceptSubmitted.map(_ => None.pure[Future])
      maybeAccessCodeResult <- maybeNoAccessCode.getOrElse(startFreshForm(formTemplateId, userDetails, maybeAccessCode))
    } yield (maybeAccessCodeResult, maybeFormExceptSubmitted.isDefined)

  private type Validator = (
    FormDataRecalculated,
    List[Section],
    SectionNumber,
    EnvelopeId,
    MaterialisedRetrievals,
    HeaderCarrier,
    FormTemplate) => Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)]

  private def renderSection(
    maybeAccessCode: Option[AccessCode],
    formTemplateId: FormTemplateId,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String],
    validator: Validator) = auth.async(formTemplateId, maybeAccessCode) { implicit request => cache =>
    val formTemplate = cache.formTemplate
    val envelopeId = cache.form.envelopeId
    val retrievals = cache.retrievals
    val dataRaw: Map[FormComponentId, Seq[String]] = FormDataHelpers.formDataMap(cache.form.formData)

    for {
      data <- recalculation.recalculateFormData(dataRaw, cache.formTemplate, retrievals)
      sections = RepeatingComponentService.getAllSections(formTemplate, data)
      (errors, v, envelope) <- validator(
                                data,
                                sections,
                                sectionNumber,
                                envelopeId,
                                retrievals,
                                implicitly,
                                cache.formTemplate)
      html = renderer.renderSection(
        maybeAccessCode,
        cache.form,
        sectionNumber,
        data,
        formTemplate,
        errors,
        envelope,
        envelopeId,
        v,
        sections,
        formMaxAttachmentSizeMB,
        contentTypes,
        retrievals,
        lang
      )
    } yield Ok(html)
  }

  def form(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String]) =
    renderSection(maybeAccessCode, formTemplateId, sectionNumber, sectionTitle4Ga, lang, doNotValidate)

  def formError(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String]) =
    renderSection(maybeAccessCode, formTemplateId, sectionNumber, sectionTitle4Ga, lang, validate)

  def fileUploadPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    fId: String,
    lang: Option[String]) = auth.async(formTemplateId, maybeAccessCode) { implicit request => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang)

    def actionUrl(envelopeId: EnvelopeId) =
      s"/file-upload/upload/envelopes/${envelopeId.value}/files/${fileId.value}?redirect-success-url=${`redirect-success-url`.toString}&redirect-error-url=${`redirect-error-url`.toString}"

    Ok(
      snippets.file_upload_page(
        maybeAccessCode,
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

  def decision(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => cache =>
      choice.bindFromRequest
        .fold(
          _ => BadRequest(continue_form_page(cache.formTemplate, maybeAccessCode, lang, frontendAppConfig)), {
            case "continue" =>
              redirectOrigin(maybeAccessCode, cache.retrievals, cache.formTemplate, lang)
            case "delete" =>
              Ok(confirm_delete(cache.formTemplate, maybeAccessCode, lang, frontendAppConfig))
            case _ => Redirect(routes.FormController.newForm(formTemplateId, lang))
          }
        )
        .pure[Future]
    }

  def delete(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => cache =>
      gformConnector
        .deleteForm(FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode))
        .map(_ => Redirect(routes.FormController.newForm(formTemplateId, lang)))
    }

  val deleteOnExit = delete _

  def envelopeF(envelopeId: EnvelopeId)(implicit headerCarrier: HeaderCarrier): Future[Envelope] =
    fileUploadService.getEnvelope(envelopeId)

  def validate(
    formDataRecalculated: FormDataRecalculated,
    sections: List[Section],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    headerCarrier: HeaderCarrier,
    formTemplate: FormTemplate
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)] = {
    val section = sections(sectionNumber.value)
    val allFC = submittedFCs(formDataRecalculated, sections.flatMap(_.expandSection.allFCs))
    val sectionFields = submittedFCs(formDataRecalculated, section.expandSection.allFCs)
    implicit val hc = headerCarrier
    for {
      envelope <- envelopeF(envelopeId)
      v <- validationService
            .validateForm(sectionFields, section, envelopeId, retrievals, formTemplate)(formDataRecalculated)
    } yield (validationService.evaluateValidation(v, allFC, formDataRecalculated, envelope), v, envelope)
  }

  def doNotValidate(
    data: FormDataRecalculated,
    sections: List[Section],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    headerCarrier: HeaderCarrier,
    formTemplate: FormTemplate
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)] = {
    implicit val hc = headerCarrier
    for {
      envelope <- envelopeF(envelopeId)
    } yield (List.empty, Valid(()), envelope)
  }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    lang: Option[String]
  ) = auth.async(formTemplateId, maybeAccessCode) { implicit request => cache =>
    processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>
      val envelopeId = cache.form.envelopeId
      val retrievals = cache.retrievals

      val dataAndSections: Future[(FormDataRecalculated, List[Section])] = for {
        formDataRecalculated <- recalculation.recalculateFormData(data, cache.formTemplate, retrievals)

      } yield {
        val sections = RepeatingComponentService.getAllSections(cache.formTemplate, formDataRecalculated)
        (formDataRecalculated, sections)
      }

      def validateForm: Future[(Boolean, FormData)] =
        for {
          (data, sections) <- dataAndSections
          formData <- validate(data, sections, sectionNumber, envelopeId, retrievals, implicitly, cache.formTemplate)
                       .map {
                         case (validationResult, _, _) =>
                           (
                             ValidationUtil.isFormValid(validationResult.toMap),
                             FormData(validationResult.flatMap(_._2.toFormField)))
                       }
        } yield formData

      def processSaveAndContinue(userDetails: UserDetails, form: Form, sn: SectionNumber)(
        implicit hc: HeaderCarrier): Future[Result] =
        for {
          (_, sections)           <- dataAndSections
          (isFormValid, formData) <- validateForm
          _ <- gformConnector
                .updateUserData(FormId(userDetails, formTemplateId, maybeAccessCode), UserData(formData, InProgress))
        } yield {
          val sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
          val gotoForm = routes.FormController
            .form(cache.formTemplate._id, maybeAccessCode, sn, sectionTitle4Ga, lang)
          val gotoFormError = routes.FormController
            .formError(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang)

          Redirect(if (isFormValid) gotoForm else gotoFormError)
        }

      def processSaveAndSummary(userDetails: UserDetails, form: Form)(implicit hc: HeaderCarrier): Future[Result] =
        for {
          (_, sections)           <- dataAndSections
          (isFormValid, formData) <- validateForm
          userData = UserData(formData, Summary)
          _ <- gformConnector.updateUserData(FormId(userDetails, formTemplateId, maybeAccessCode), userData)
        } yield {

          val sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
          val gotoSummary = routes.SummaryController
            .summaryById(formTemplateId, maybeAccessCode, lang)
          val gotoFormError = routes.FormController
            .formError(cache.formTemplate._id, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang)

          Redirect(if (isFormValid) gotoSummary else gotoFormError)
        }

      def processSaveAndExit(userDetails: UserDetails, form: Form, envelopeId: EnvelopeId): Future[Result] =
        for {
          (_, sections) <- dataAndSections
          (_, formData) <- validateForm
          userData = UserData(formData, InProgress)
          originSection = new Origin(sections).minSectionNumber
          sectionTitle4Ga = sectionTitle4GaFactory(sections(originSection.value).title)
          result <- gformConnector
                     .updateUserData(FormId(userDetails, formTemplateId, maybeAccessCode), userData)
                     .map(
                       response =>
                         maybeAccessCode match {
                           case (Some(accessCode)) =>
                             Ok(
                               save_with_access_code(
                                 accessCode,
                                 cache.formTemplate,
                                 originSection,
                                 sectionTitle4Ga,
                                 lang,
                                 frontendAppConfig))
                           case _ =>
                             Ok(
                               save_acknowledgement(
                                 cache.formTemplate,
                                 originSection,
                                 sectionTitle4Ga,
                                 lang,
                                 frontendAppConfig))
                       }
                     )
        } yield result

      def processBack(userDetails: UserDetails, form: Form, sn: SectionNumber): Future[Result] =
        for {
          (_, sections) <- dataAndSections
          (_, formData) <- validateForm
          userData = UserData(formData, InProgress)
          sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
          result <- gformConnector
                     .updateUserData(FormId(userDetails, formTemplateId, maybeAccessCode), userData)
                     .map(response =>
                       Redirect(routes.FormController.form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, lang)))
        } yield result

      def processAddGroup(groupId: String): Future[Result] = {
        val startPos = groupId.indexOf('-') + 1
        val groupComponentId = FormComponentId(groupId.substring(startPos))

        for {
          (_, sections) <- dataAndSections
          (_, formData) <- validateForm
          (groupFormData, anchor) = addNextGroup(findFormComponent(groupComponentId, sections), formData)
          userData = UserData(formData |+| groupFormData, InProgress)
          _ <- gformConnector
                .updateUserData(FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode), userData)
          sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
        } yield
          Redirect(
            routes.FormController
              .form(cache.formTemplate._id, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang)
              .url + anchor.map("#" + _).getOrElse(""))
      }

      def processRemoveGroup(idx: Int, groupId: String): Future[Result] =
        for {
          (data, sections) <- dataAndSections
          maybeGroupFc = findFormComponent(FormComponentId(groupId), sections)

          updatedData = removeGroupFromData(idx, maybeGroupFc, data)

          (errors, _, _) <- validate(
                             data,
                             sections,
                             sectionNumber,
                             envelopeId,
                             retrievals,
                             implicitly,
                             cache.formTemplate)
          formData = FormData(errors.toMap.values.toSeq.flatMap(_.toFormField))
          userData = UserData(formData, InProgress)
          _ <- gformConnector
                .updateUserData(FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode), userData)
          sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
        } yield
          Redirect(
            routes.FormController
              .form(cache.formTemplate._id, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang)
              .url
          )

      val userDetails = cache.retrievals.userDetails
      val navigationF: Future[Direction] = for {
        (data, sections) <- dataAndSections
      } yield new Navigator(sectionNumber, sections, data).navigate

      navigationF.flatMap {
        case SaveAndContinue(sn)       => processSaveAndContinue(userDetails, cache.form, sn)
        case SaveAndExit               => processSaveAndExit(userDetails, cache.form, cache.form.envelopeId)
        case Back(sn)                  => processBack(userDetails, cache.form, sn)
        case SaveAndSummary            => processSaveAndSummary(userDetails, cache.form)
        case BackToSummary             => processSaveAndSummary(userDetails, cache.form)
        case AddGroup(groupId)         => processAddGroup(groupId)
        case RemoveGroup(idx, groupId) => processRemoveGroup(idx, groupId)
      }
    }
  }

  private lazy val firstSection = SectionNumber(0)
  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
