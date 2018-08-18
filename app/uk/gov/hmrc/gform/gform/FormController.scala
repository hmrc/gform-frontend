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

import cats.Semigroup
import cats.data.Validated.Valid
import cats.instances.future._
import cats.syntax.applicative._
import cats.syntax.monoid._
import play.api.Logger
import play.api.i18n.I18nSupport
import play.api.mvc._
import scala.concurrent.ExecutionContext
import uk.gov.hmrc.auth.core.{ AffinityGroup, Enrolments }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.ops.FormTemplateIdSyntax
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

case class AccessCodeForm(accessCode: String)

private class Identifiers(cache: AuthCacheWithoutForm, val accessCode: Option[AccessCode]) {
  val formTemplateId: FormTemplateId = cache.formTemplate._id
  val userId: UserId = UserId(cache.retrievals.userDetails.groupIdentifier)
  val formId: FormId = FormId(userId, formTemplateId, accessCode)
}

private object Identifiers {
  def apply(cache: AuthCacheWithoutForm, accessCode: AccessCode) = new Identifiers(cache, Some(accessCode))
  def apply(cache: AuthCacheWithoutForm) = new Identifiers(cache, None)
}

class FormController(
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
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
    val sectionTitle4Ga = sectionTitle4GaFactory(formTemplate.sections(originSection.value).title)
    Redirect(routes.FormController.form(formId, formTemplate._id.to4Ga, originSection, sectionTitle4Ga, lang))
  }

  val accessCodeForm: play.api.data.Form[AccessCodeForm] =
    play.api.data
      .Form(
        play.api.data.Forms
          .mapping(AccessCode.key -> play.api.data.Forms.nonEmptyText)(AccessCodeForm.apply)(AccessCodeForm.unapply))

  def dashboard(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      cache.retrievals.affinityGroup match {
        case Some(AffinityGroup.Agent) if appConfig.feature.concurrentAgentAccess =>
          Future.successful(
            Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages
              .dashboard(cache.formTemplate, accessCodeForm, lang, frontendAppConfig)))
        case _ => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, lang)))
      }
  }

  def newFormAgent(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      val accessCode = AccessCode.random
      for {
        _ <- startForm(Identifiers(cache, accessCode))
      } yield
        Redirect(routes.FormController.showAccessCode(formTemplateId, lang))
          .flashing(AccessCode.key -> accessCode.value)
  }

  def showAccessCode(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      request.flash.get(AccessCode.key) match {
        case Some(accessCode) =>
          Future.successful(
            Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages
              .start_new_form(cache.formTemplate, AccessCode(accessCode), lang, frontendAppConfig)))
        case None => Future.successful(Redirect(routes.FormController.dashboard(formTemplateId, lang)))
      }
  }

  def newForm(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      for {
        (form, wasFormFound) <- getOrStartForm(Identifiers(cache))
      } yield {
        if (wasFormFound) {
          Ok(continue_form_page(cache.formTemplate, form._id, lang, frontendAppConfig))
        } else redirectOrigin(form._id, cache.retrievals, cache.formTemplate, lang)
      }
  }

  def newFormPost(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId) {
    implicit request => cache =>
      accessCodeForm.bindFromRequest.fold(
        hasErrors =>
          Future.successful(
            BadRequest(uk.gov.hmrc.gform.views.html.hardcoded.pages
              .dashboard(cache.formTemplate, hasErrors, lang, frontendAppConfig))),
        accessCodeF => {
          val accessCode = AccessCode(accessCodeF.accessCode)
          for {
            maybeForm <- getForm(Identifiers(cache, accessCode))
          } yield
            maybeForm match {
              case Some(form) => redirectOrigin(form._id, cache.retrievals, cache.formTemplate, lang)
              case None       => Redirect(routes.FormController.dashboard(formTemplateId, lang))
            }
        }
      )
  }

  private def getForm(ids: Identifiers)(implicit hc: HeaderCarrier): Future[Option[Form]] =
    for {
      maybeForm <- gformConnector.maybeForm(ids.formId)
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
    } yield maybeFormExceptSubmitted

  private def startFreshForm(ids: Identifiers)(implicit hc: HeaderCarrier): Future[Form] = {
    import ids._
    for {
      formId <- gformConnector.newForm(formTemplateId, userId, accessCode)
      form   <- gformConnector.getForm(formId)
    } yield form
  }

  private def startForm(ids: Identifiers)(implicit hc: HeaderCarrier): Future[Form] = {
    val formId = ids.formId
    def formIdAlreadyExists = Future.failed(new Exception(s"Form $formId already exists"))
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      form      <- if (maybeForm.isDefined) formIdAlreadyExists else startFreshForm(ids)
    } yield form
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(ids: Identifiers)(implicit hc: HeaderCarrier): Future[(Form, Boolean)] =
    for {
      maybeForm <- gformConnector.maybeForm(ids.formId)
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
      form <- maybeFormExceptSubmitted.map(Future.successful).getOrElse(startFreshForm(ids))
    } yield (form, maybeFormExceptSubmitted.isDefined)

  private type Validator = (
    Map[FormComponentId, Seq[String]],
    List[Section],
    SectionNumber,
    EnvelopeId,
    MaterialisedRetrievals,
    HeaderCarrier) => Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)]

  private def renderSection(
    formId: FormId,
    formTemplateId4Ga: FormTemplateId4Ga,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String],
    validator: Validator) = auth.async(formId) { implicit request => cache =>
    val formTemplate = cache.formTemplate
    val envelopeId = cache.form.envelopeId
    val retrievals = cache.retrievals
    val data = FormDataHelpers.formDataMap(cache.form.formData)
    val sections = RepeatingComponentService.getAllSections(formTemplate, data)
    for {
      (errors, v, envelope) <- validator(data, sections, sectionNumber, envelopeId, retrievals, implicitly)
      html <- renderer.renderSection(
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
               lang)
    } yield Ok(html)
  }

  def form(
    formId: FormId,
    formTemplateId4Ga: FormTemplateId4Ga,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String]) =
    renderSection(formId, formTemplateId4Ga, sectionNumber, sectionTitle4Ga, lang, doNotValidate)

  def formError(
    formId: FormId,
    formTemplateId4Ga: FormTemplateId4Ga,
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String]) =
    renderSection(formId, formTemplateId4Ga, sectionNumber, sectionTitle4Ga, lang, validate)

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
            Future.successful(redirectOrigin(formId, cache.retrievals, cache.formTemplate, lang))
          case "delete" => Future.successful(Ok(confirm_delete(cache.formTemplate, formId, lang, frontendAppConfig)))
          case _        => Future.successful(Redirect(routes.FormController.newForm(formTemplateId, lang)))
        }
      )
    }

  def delete(formTemplateId4Ga: FormTemplateId4Ga, formId: FormId, lang: Option[String]): Action[AnyContent] =
    auth.async(formId) { implicit request => cache =>
      gformConnector.deleteForm(formId).map(_ => Redirect(routes.FormController.newForm(cache.formTemplate._id, lang)))
    }

  val deleteOnExit = delete _

  def envelopeF(envelopeId: EnvelopeId)(implicit headerCarrier: HeaderCarrier): Future[Envelope] =
    fileUploadService.getEnvelope(envelopeId)

  def validate(
    data: Map[FormComponentId, Seq[String]],
    sections: List[Section],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    headerCarrier: HeaderCarrier
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)] = {
    val section = sections(sectionNumber.value)
    val allFC = submittedFCs(data, sections.flatMap(_.expandSection.allFCs))
    val sectionFields = submittedFCs(data, section.expandSection.allFCs)
    implicit val hc = headerCarrier
    for {
      envelope <- envelopeF(envelopeId)
      v        <- validationService.validateForm(sectionFields, section, envelopeId, retrievals)(data)
    } yield (validationService.evaluateValidation(v, allFC, data, envelope), v, envelope)
  }

  def doNotValidate(
    data: Map[FormComponentId, Seq[String]],
    sections: List[Section],
    sectionNumber: SectionNumber,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    headerCarrier: HeaderCarrier
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)] = {
    implicit val hc = headerCarrier
    for {
      envelope <- envelopeF(envelopeId)
    } yield (List.empty, Valid(()), envelope)
  }

  def updateFormData(formId: FormId, sectionNumber: SectionNumber, lang: Option[String]) = auth.async(formId) {
    implicit request => cache =>
      processResponseDataFromBody(request) { (data: Map[FormComponentId, Seq[String]]) =>
        val sections = RepeatingComponentService.getAllSections(cache.formTemplate, data)

        val envelopeId = cache.form.envelopeId
        val retrievals = cache.retrievals

        def validateForm: Future[(Boolean, FormData)] =
          validate(data, sections, sectionNumber, envelopeId, retrievals, implicitly)
            .map {
              case (validationResult, _, _) =>
                (
                  ValidationUtil.isFormValid(validationResult.toMap),
                  FormData(validationResult.flatMap(_._2.toFormField)))
            }

        def processSaveAndContinue(userId: UserId, form: Form, sn: SectionNumber)(
          implicit hc: HeaderCarrier): Future[Result] =
          for {
            (isFormValid, formData) <- validateForm
            _                       <- gformConnector.updateUserData(formId, UserData(formData, InProgress))
          } yield {
            val sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
            val gotoForm = routes.FormController.form(formId, cache.formTemplate._id.to4Ga, sn, sectionTitle4Ga, lang)
            val gotoFormError = routes.FormController
              .formError(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)

            Redirect(if (isFormValid) gotoForm else gotoFormError)
          }

        def processSaveAndSummary(userId: UserId, form: Form)(implicit hc: HeaderCarrier): Future[Result] =
          for {
            (isFormValid, formData) <- validateForm
            formDataUpdE = Recalculation.recalculateFormData(formData, cache.formTemplate)
            formDataUpd = formDataUpdE match {
              case Left(graphException) => Logger.error(graphException.reportProblem); formData
              case Right(fd)            => fd
            }
            userData = UserData(formDataUpd, Summary)
            _ <- gformConnector.updateUserData(formId, userData)
          } yield {

            val sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
            val gotoSummary = routes.SummaryController.summaryById(formId, cache.formTemplate._id.to4Ga, lang)
            val gotoFormError = routes.FormController
              .formError(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)

            Redirect(if (isFormValid) gotoSummary else gotoFormError)
          }

        def processSaveAndExit(userId: UserId, form: Form, envelopeId: EnvelopeId): Future[Result] =
          for {
            (_, formData) <- validateForm
            userData = UserData(formData, InProgress)
            originSection = new Origin(sections, cache.retrievals).minSectionNumber
            sectionTitle4Ga = sectionTitle4GaFactory(sections(originSection.value).title)
            result <- gformConnector
                       .updateUserData(formId, userData)
                       .map(
                         response =>
                           Ok(
                             views.html.hardcoded.pages
                               .save_acknowledgement(
                                 formId,
                                 cache.formTemplate,
                                 originSection,
                                 sectionTitle4Ga,
                                 lang,
                                 frontendAppConfig)))
          } yield result

        def processBack(userId: UserId, form: Form, sn: SectionNumber): Future[Result] =
          for {
            (_, formData) <- validateForm
            userData = UserData(formData, InProgress)
            sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
            result <- gformConnector
                       .updateUserData(formId, userData)
                       .map(response =>
                         Redirect(
                           routes.FormController.form(formId, cache.formTemplate._id.to4Ga, sn, sectionTitle4Ga, lang)))
          } yield result

        def processAddGroup(groupId: String): Future[Result] = {
          val startPos = groupId.indexOf('-') + 1
          val groupComponentId = FormComponentId(groupId.substring(startPos))

          for {
            (_, formData) <- validateForm
            (groupFormData, anchor) = addNextGroup(findFormComponent(groupComponentId, sections), formData)
            userData = UserData(formData |+| groupFormData, InProgress)
            _ <- gformConnector.updateUserData(formId, userData)
            sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
          } yield
            Redirect(
              routes.FormController
                .form(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)
                .url + anchor.map("#" + _).getOrElse(""))
        }

        def processRemoveGroup(idx: Int, groupId: String): Future[Result] = {
          val maybeGroupFc: Option[FormComponent] = findFormComponent(FormComponentId(groupId), sections)

          val updatedData = removeGroupFromData(idx, maybeGroupFc, data)

          for {
            (errors, _, _) <- validate(updatedData, sections, sectionNumber, envelopeId, retrievals, implicitly)
            formData = FormData(errors.toMap.values.toSeq.flatMap(_.toFormField))
            userData = UserData(formData, InProgress)
            _ <- gformConnector.updateUserData(formId, userData)
            sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
          } yield
            Redirect(
              routes.FormController
                .form(formId, cache.formTemplate._id.to4Ga, sectionNumber, sectionTitle4Ga, lang)
                .url
            )
        }

        val userId = UserId(cache.retrievals.userDetails.groupIdentifier)
        val navigationF: Future[Direction] =
          Future.successful(new Navigator(sectionNumber, sections, data, cache.retrievals).navigate)

        def redirection(call: Int => Call): Future[Result] = Future.successful(Redirect(call(sections.size)))

        navigationF.flatMap {
          case SaveAndContinue(sn)       => processSaveAndContinue(userId, cache.form, sn)
          case SaveAndExit               => processSaveAndExit(userId, cache.form, cache.form.envelopeId)
          case Back(sn)                  => processBack(userId, cache.form, sn)
          case SaveAndSummary            => processSaveAndSummary(userId, cache.form)
          case BackToSummary             => processSaveAndSummary(userId, cache.form)
          case AddGroup(groupId)         => processAddGroup(groupId)
          case RemoveGroup(idx, groupId) => processRemoveGroup(idx, groupId)
        }
      }
  }

  private lazy val firstSection = SectionNumber(0)
  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
