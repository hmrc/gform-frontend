/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.graph.{ Data, Recalculation }
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
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.fileupload.Available

import scala.concurrent.Future

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

  private def validateForm(
    data: FormDataRecalculated,
    sections: List[Section],
    sn: SectionNumber,
    cache: AuthCacheWithForm)(
    implicit request: Request[AnyContent]
  ): Future[(Boolean, FormData)] =
    for {
      formData <- validate(data, sections, sn, cache.form.envelopeId, cache.retrievals, cache.formTemplate)
                   .map {
                     case (validationResult, _, _) =>
                       (
                         ValidationUtil.isFormValid(validationResult.toMap),
                         FormData(validationResult.flatMap(_._2.toFormField)))
                   }
    } yield formData

  private def fastForwardValidate(
    data: FormDataRecalculated,
    dataRaw: Data,
    sections: List[Section],
    cache: AuthCacheWithForm)(
    implicit request: Request[AnyContent]
  ): Future[Option[SectionNumber]] =
    new Origin(sections, data).availableSectionNumbers.foldLeft(Future.successful(None: Option[SectionNumber])) {
      case (accF, currentSn) =>
        accF.flatMap { acc =>
          acc match {
            case Some(sn) => Future.successful(Some(sn))
            case None =>
              validateForm(data, sections, currentSn, cache).map {
                case (isValid, _) =>
                  val section = sections(currentSn.value)

                  val fcIds = section
                    .expandSection(dataRaw)
                    .expandedFCs
                    .flatMap(_.allIdsExceptInfoMessages)

                  val firstVisit = !fcIds.forall(dataRaw.isDefinedAt)
                  val stop = section.continueIf.contains(Stop) || firstVisit
                  if (isValid && !stop) None else Some(currentSn)
              }
          }
        }
    }

  private def redirectOrigin(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    data: FormDataRecalculated,
    dataRaw: Data,
    sections: List[Section],
    lang: Option[String])(
    implicit request: Request[AnyContent]
  ): Future[Result] = {

    val formTemplate = cache.formTemplate

    fastForwardValidate(data, dataRaw, sections, cache).map {
      case Some(sn) =>
        val section = sections(sn.value)
        val sectionTitle4Ga = sectionTitle4GaFactory(section.title)
        Redirect(
          routes.FormController
            .form(formTemplate._id, maybeAccessCode, sn, sectionTitle4Ga, lang, SeYes))
      case None => Redirect(routes.SummaryController.summaryById(formTemplate._id, maybeAccessCode, lang))

    }
  }

  // TODO: this method should really be in the SignOutController which does not yet exist
  def keepAlive() = auth.keepAlive()

  def dashboard(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId, lang) {
    implicit request => cache =>
      val formId = FormId(cache.retrievals.userDetails, formTemplateId, None)
      for {
        maybeForm <- gformConnector.maybeForm(formId)
      } yield
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals.affinityGroup, maybeForm) match {
          case (Some(FormAccessCodeForAgents), Some(AffinityGroup.Agent), None) =>
            Ok(access_code_start(cache.formTemplate, AgentAccessCode.form, lang, frontendAppConfig))
          case _ => Redirect(routes.FormController.newForm(formTemplateId, lang))
        }
  }

  def newFormAgent(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId, lang) {
    implicit request => cache =>
      val accessCode = AccessCode(AgentAccessCode.random.value)
      for {
        _ <- startForm(formTemplateId, cache.retrievals.userDetails, Some(accessCode))
      } yield
        Redirect(routes.FormController.showAccessCode(formTemplateId, lang))
          .flashing(AgentAccessCode.key -> accessCode.value)
  }

  def showAccessCode(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId, lang) {
    implicit request => cache =>
      request.flash.get(AgentAccessCode.key) match {
        case Some(accessCode) =>
          Future.successful(
            Ok(start_new_form(cache.formTemplate, AgentAccessCode(accessCode), lang, frontendAppConfig)))
        case None => Future.successful(Redirect(routes.FormController.dashboard(formTemplateId, lang)))
      }
  }

  def newForm(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId, lang) {
    implicit request => cache =>
      val noAccessCode = Option.empty[AccessCode]
      for {
        (formId, wasFormFound) <- getOrStartForm(formTemplateId, cache.retrievals.userDetails, noAccessCode)
        result <- if (wasFormFound) {
                   Ok(continue_form_page(cache.formTemplate, noAccessCode, lang, frontendAppConfig)).pure[Future]
                 } else {
                   for {
                     maybeForm <- getForm(formId)
                     res <- maybeForm match {
                             case Some(form) => redirectFromEmpty(cache, form, noAccessCode, lang)
                             case None       => Future.failed(new NotFoundException(s"Form with id $formId not found."))
                           }
                   } yield res
                 }
      } yield result
  }

  private def redirectFromEmpty(
    cache: AuthCacheWithoutForm,
    form: Form,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String])(implicit request: Request[AnyContent]) = {
    val dataRaw = Map.empty[FormComponentId, Seq[String]]
    val cacheWithForm = cache.toAuthCacheWithForm(form)
    redirectWithRecalculation(cacheWithForm, dataRaw, maybeAccessCode, lang)
  }

  private def redirectWithRecalculation(
    cache: AuthCacheWithForm,
    dataRaw: Data,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String])(implicit request: Request[AnyContent]) =
    for {
      (data, sections) <- recalculateDataAndSections(dataRaw, cache)
      res              <- redirectOrigin(maybeAccessCode, cache, data, dataRaw, sections, lang)
    } yield res

  def newFormPost(formTemplateId: FormTemplateId, lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, lang) { implicit request => cache =>
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
                res <- maybeForm match {
                        case Some(form) => redirectFromEmpty(cache, form, maybeAccessCode, lang)
                        case None =>
                          BadRequest(
                            access_code_start(
                              cache.formTemplate,
                              AgentAccessCode.form.bindFromRequest().withError(AgentAccessCode.key, "error.notfound"),
                              lang,
                              frontendAppConfig
                            )
                          ).pure[Future]
                      }
              } yield res
            }
          }
        }
      )
    }

  private def getForm(formId: FormId)(implicit hc: HeaderCarrier): Future[Option[Form]] =
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
      maybeEnvelope <- maybeFormExceptSubmitted.fold(Option.empty[Envelope].pure[Future]) { f =>
                        getEnvelope(f.envelopeId)
                      }
      mayBeFormExceptWithEnvelope <- (maybeFormExceptSubmitted, maybeEnvelope) match {
                                      case (None, _)          => None.pure[Future]
                                      case (Some(f), None)    => gformConnector.deleteForm(f._id).map(_ => None)
                                      case (Some(_), Some(_)) => maybeFormExceptSubmitted.pure[Future]
                                    }
    } yield mayBeFormExceptWithEnvelope

  private def getEnvelope(envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[Option[Envelope]] =
    for {
      maybeEnvelope <- fileUploadService.getMaybeEnvelope(envelopeId)
      maybeFormExceptSubmitted = maybeEnvelope
    } yield maybeFormExceptSubmitted

  private def startFreshForm(
    formTemplateId: FormTemplateId,
    userDetails: UserDetails,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[FormId] =
    for {
      formId <- gformConnector.newForm(formTemplateId, UserId(userDetails.groupIdentifier), maybeAccessCode)
    } yield formId

  private def startForm(formTemplateId: FormTemplateId, userDetails: UserDetails, maybeAccessCode: Option[AccessCode])(
    implicit hc: HeaderCarrier): Future[FormId] = {
    val formId = FormId(userDetails, formTemplateId, None)
    def formIdAlreadyExists = Future.failed(new Exception(s"Form $formId already exists"))
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      formId <- if (maybeForm.isDefined) formIdAlreadyExists
               else startFreshForm(formTemplateId, userDetails, maybeAccessCode)
    } yield formId
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(
    formTemplateId: FormTemplateId,
    userDetails: UserDetails,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[(FormId, Boolean)] =
    for {
      maybeFormExceptSubmitted <- getForm(FormId(userDetails, formTemplateId, None))
      formId <- {
        maybeFormExceptSubmitted.fold(startFreshForm(formTemplateId, userDetails, maybeAccessCode))(_._id.pure[Future])
      }
    } yield (formId, maybeFormExceptSubmitted.isDefined)

  def form(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    lang: Option[String],
    suppressErrors: SuppressErrors) = auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
    val formTemplate = cache.formTemplate
    val envelopeId = cache.form.envelopeId
    val retrievals = cache.retrievals
    val dataRaw: Data = FormDataHelpers.formDataMap(cache.form.formData)

    for {
      (data, sections) <- recalculateDataAndSections(dataRaw, cache)
      (errors, v, envelope) <- suppressErrors match {
                                case SeYes => envelopeF(envelopeId).map((List.empty, Valid(()), _))
                                case SeNo =>
                                  validate(data, sections, sectionNumber, envelopeId, retrievals, cache.formTemplate)
                              }
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

  def fileUploadPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    fId: String,
    lang: Option[String]) = auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang, SeYes)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang, SeYes)

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
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      choice.bindFromRequest
        .fold(
          _ =>
            BadRequest(continue_form_page(cache.formTemplate, maybeAccessCode, lang, frontendAppConfig)).pure[Future], {
            case "continue" =>
              val dataRaw = FormDataHelpers.formDataMap(cache.form.formData)
              redirectWithRecalculation(cache, dataRaw, maybeAccessCode, lang)
            case "delete" =>
              Ok(confirm_delete(cache.formTemplate, maybeAccessCode, lang, frontendAppConfig)).pure[Future]
            case _ => Redirect(routes.FormController.newForm(formTemplateId, lang)).pure[Future]
          }
        )
    }

  def delete(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      gformConnector
        .deleteForm(FormId(cache.retrievals.userDetails, formTemplateId, maybeAccessCode))
        .map(_ =>
          (cache.formTemplate.draftRetrievalMethod, cache.retrievals.affinityGroup) match {
            case (Some(FormAccessCodeForAgents), Some(AffinityGroup.Agent)) =>
              Redirect(routes.FormController.newFormAgent(formTemplateId, lang))
            case _ => Redirect(routes.FormController.newForm(formTemplateId, lang))
        })
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
    formTemplate: FormTemplate
  )(
    implicit request: Request[AnyContent]
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType, Envelope)] = {
    val section = sections(sectionNumber.value)
    val nonSubmittedYet = nonSubmittedFCsOfNonGroup(formDataRecalculated, section)
    val allFC = submittedFCs(formDataRecalculated, sections.flatMap(_.expandSection(formDataRecalculated.data).allFCs)) ++ nonSubmittedYet
    val sectionFields = submittedFCs(formDataRecalculated, section.expandSection(formDataRecalculated.data).allFCs) ++ nonSubmittedYet

    for {
      envelope <- envelopeF(envelopeId)
      v <- validationService
            .validateForm(sectionFields, section, envelopeId, retrievals, formTemplate)(formDataRecalculated)
    } yield (validationService.evaluateValidation(v, allFC, formDataRecalculated, envelope), v, envelope)
  }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    lang: Option[String]
  ) = auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
    processResponseDataFromBody(request) { (dataRaw: Data) =>
      val form: Form = cache.form
      val envelopeId = form.envelopeId
      val retrievals = cache.retrievals
      val userDetails = retrievals.userDetails

      val formId = FormId(userDetails, formTemplateId, maybeAccessCode)

      def validateAndUpdateData(data: FormDataRecalculated, sections: List[Section])(
        result: Option[SectionNumber] => Result): Future[Result] =
        for {
          (_, formData) <- validateForm(data, sections, sectionNumber, cache)
          maybeSn       <- fastForwardValidate(data, dataRaw, sections, cache)
          userData = UserData(formData, maybeSn.fold(Summary: FormStatus)(_ => InProgress))
          res <- gformConnector.updateUserData(formId, userData).map(_ => result(maybeSn))
        } yield res

      def processSaveAndContinue(
        data: FormDataRecalculated,
        sections: List[Section]
      ): Future[Result] =
        validateAndUpdateData(data, sections) {
          case Some(sn) =>
            val sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
            Redirect(
              routes.FormController
                .form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, lang, SuppressErrors(sectionNumber < sn)))
          case None =>
            Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, lang))
        }

      def processSaveAndExit(data: FormDataRecalculated, sections: List[Section]): Future[Result] =
        validateAndUpdateData(data, sections) { maybeSn =>
          val formTemplate = cache.formTemplate
          maybeAccessCode match {
            case Some(accessCode) =>
              Ok(save_with_access_code(accessCode, formTemplate, lang, frontendAppConfig))
            case None =>
              val call = maybeSn match {
                case Some(sn) =>
                  val sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
                  routes.FormController.form(formTemplateId, None, sn, sectionTitle4Ga, lang, SeYes)
                case None => routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, lang)
              }
              Ok(save_acknowledgement(formTemplate, call, lang, frontendAppConfig))
          }
        }

      def processBack(data: FormDataRecalculated, sections: List[Section], sn: SectionNumber): Future[Result] =
        validateAndUpdateData(data, sections) { _ =>
          val sectionTitle4Ga = sectionTitle4GaFactory(sections(sn.value).title)
          Redirect(routes.FormController.form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, lang, SeYes))
        }

      def handleGroup(data: FormDataRecalculated, sections: List[Section], anchor: String): Future[Result] =
        validateAndUpdateData(data, sections) { _ =>
          val sectionTitle4Ga = sectionTitle4GaFactory(sections(sectionNumber.value).title)
          Redirect(
            routes.FormController
              .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang, SeYes)
              .url + anchor
          )
        }

      def processAddGroup(data: FormDataRecalculated, sections: List[Section], groupId: String): Future[Result] = {
        val startPos = groupId.indexOf('-') + 1
        val groupComponentId = FormComponentId(groupId.substring(startPos))
        val maybeGroupFc = findFormComponent(groupComponentId, sections)
        val (updatedData, anchor) = addNextGroup(maybeGroupFc, data)

        handleGroup(updatedData, sections, anchor.map("#" + _).getOrElse(""))
      }

      def processRemoveGroup(
        data: FormDataRecalculated,
        sections: List[Section],
        idx: Int,
        groupId: String): Future[Result] = {
        val maybeGroupFc = findFormComponent(FormComponentId(groupId), sections)
        val updatedData = removeGroupFromData(idx, maybeGroupFc, data)

        handleGroup(updatedData, sections, "")
      }

      for {
        (data, sections) <- recalculateDataAndSections(dataRaw, cache)
        nav = new Navigator(sectionNumber, sections, data).navigate
        res <- nav match {
                case SaveAndContinue           => processSaveAndContinue(data, sections)
                case SaveAndExit               => processSaveAndExit(data, sections)
                case Back(sn)                  => processBack(data, sections, sn)
                case AddGroup(groupId)         => processAddGroup(data, sections, groupId)
                case RemoveGroup(idx, groupId) => processRemoveGroup(data, sections, idx, groupId)
              }
      } yield res
    }
  }

  private def recalculateDataAndSections(data: Data, cache: AuthCacheWithForm)(
    implicit request: Request[AnyContent]): Future[(FormDataRecalculated, List[Section])] =
    for {
      formDataRecalculated <- recalculation.recalculateFormData(data, cache.formTemplate, cache.retrievals)
    } yield {
      val sections = RepeatingComponentService.getAllSections(cache.formTemplate, formDataRecalculated)
      (formDataRecalculated, sections)
    }

  private lazy val firstSection = SectionNumber(0)
  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
