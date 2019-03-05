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

import cats.instances.future._
import cats.instances.option._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.eq._
import cats.syntax.validated._
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.{ IsAgent, MaterialisedRetrievals }
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.models.{ AgentAccessCode, ProcessData, ProcessDataService }
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.obligation.ObligationService
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ UserId => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.{ FormFieldValidationResult, ValidationService, ValidationUtil }
import uk.gov.hmrc.gform.views.html.form._
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.gform.models.gform.FormComponentValidation

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
  processDataService: ProcessDataService[Future, Throwable],
  obligationService: ObligationService,
  formService: FormService
) extends FrontendController {

  import i18nSupport._

  case class FormValidationOutcome(isValid: Boolean, formData: FormData, validatedType: ValidatedType[ValidationResult])

  private def validateForm(
    data: FormDataRecalculated,
    sections: List[Section],
    sn: SectionNumber,
    cache: AuthCacheWithForm)(
    implicit request: Request[AnyContent]
  ): Future[Option[FormValidationOutcome]] =
    for {
      formData <- validate(
                   data,
                   sections,
                   sn,
                   cache.form.envelopeId,
                   cache.retrievals,
                   cache.form.thirdPartyData,
                   cache.formTemplate)
                   .map {
                     case (validationResult, validatedType, _) =>
                       splitFormComponentValidation(validationResult.headOption).map(fcv =>
                         validateFormHelper(List(fcv), validatedType))

                   }
    } yield formData

  def splitFormComponentValidation(
    optionFcv: Option[(FormComponent, FormFieldValidationResult)]): Option[FormComponentValidation] =
    optionFcv.map(fcv => FormComponentValidation(fcv._1, fcv._2))

  private def validateFormHelper(
    validationResult: List[FormComponentValidation],
    validatedType: ValidatedType[ValidationResult]): FormValidationOutcome = {
    val isFormValid =
      ValidationUtil.isFormValid(validationResult.map(x => x.formComponent -> x.formFieldValidationResult).toMap)
    val formComponents =
      if (isFormValid) formService.removeCommas(validationResult) else validationResult

    FormValidationOutcome(
      isFormValid,
      FormData(formComponents.flatMap {
        case FormComponentValidation(_, formFieldValidationResult) => formFieldValidationResult.toFormField
      }),
      validatedType
    )
  }

  private def fastForwardValidate(processData: ProcessData, cache: AuthCacheWithForm)(
    implicit request: Request[AnyContent]
  ): Future[Option[SectionNumber]] = {

    val sections = processData.sections
    val data = processData.data

    new Origin(sections, data).availableSectionNumbers.foldLeft(Future.successful(None: Option[SectionNumber])) {
      case (accF, currentSn) =>
        accF.flatMap { acc =>
          acc match {
            case Some(sn) => Future.successful(Some(sn))
            case None =>
              validateForm(data, sections, currentSn, cache).map {
                case formValidation: FormValidationOutcome =>
                  val section = sections(currentSn.value)
                  val hasBeenVisited = processData.visitIndex.visitsIndex.contains(currentSn.value)

                  val stop = section.continueIf.contains(Stop) || !hasBeenVisited
                  if (formValidation.isValid && !stop) None else Some(currentSn)
              }
          }
        }
    }
  }

  private def redirectOrigin(
    maybeAccessCode: Option[AccessCode],
    cache: AuthCacheWithForm,
    processData: ProcessData,
    lang: Option[String])(
    implicit request: Request[AnyContent]
  ): Future[Result] = {

    val formTemplate = cache.formTemplate

    fastForwardValidate(processData, cache).map {
      case Some(sn) =>
        val section = processData.sections(sn.value)
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
      val formId = FormId(cache.retrievals, formTemplateId, None)
      for {
        maybeForm <- gformConnector.maybeForm(formId)
      } yield
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals, maybeForm) match {
          case (Some(FormAccessCodeForAgents), IsAgent(), None) =>
            Ok(access_code_start(cache.formTemplate, AgentAccessCode.form, lang, frontendAppConfig))
          case _ => Redirect(routes.FormController.newForm(formTemplateId, lang))
        }
  }

  def newFormAgent(formTemplateId: FormTemplateId, lang: Option[String]) = auth.async(formTemplateId, lang) {
    implicit request => cache =>
      val accessCode = AccessCode(AgentAccessCode.random.value)
      for {
        _ <- startForm(formTemplateId, cache.retrievals, Some(accessCode))
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
        (formId, wasFormFound) <- getOrStartForm(formTemplateId, cache.retrievals, noAccessCode)
        result <- if (wasFormFound) {
                   Ok(continue_form_page(cache.formTemplate, choice, noAccessCode, lang, frontendAppConfig))
                     .pure[Future]
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
      processData <- processDataService.getProcessData(dataRaw, cache)
      res         <- redirectOrigin(maybeAccessCode, cache, processData, lang)
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
                maybeForm <- getForm(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
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
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[FormId] =
    for {
      formId <- gformConnector.newForm(formTemplateId, UserId(retrievals), maybeAccessCode)
    } yield formId

  private def startForm(
    formTemplateId: FormTemplateId,
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[FormId] = {
    val formId = FormId(retrievals, formTemplateId, None)
    def formIdAlreadyExists = Future.failed(new Exception(s"Form $formId already exists"))
    for {
      maybeForm <- gformConnector.maybeForm(formId)
      formId <- if (maybeForm.isDefined) formIdAlreadyExists
               else startFreshForm(formTemplateId, retrievals, maybeAccessCode)
    } yield formId
  }

  //true - it got the form, false - new form was created
  private def getOrStartForm(
    formTemplateId: FormTemplateId,
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[(FormId, Boolean)] =
    for {
      maybeFormExceptSubmitted <- getForm(FormId(retrievals, formTemplateId, None))
      formId <- maybeFormExceptSubmitted.fold(startFreshForm(formTemplateId, retrievals, maybeAccessCode))(
                 _._id.pure[Future])
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
    val obligations = cache.obligations
    val dataRaw: Data = FormDataHelpers.formDataMap(cache.form.formData)

    val visits: VisitIndex = cache.form.visitsIndex
    val visitsIndex: VisitIndex = visits.visit(sectionNumber)

    for {
      update <- obligationService.updateObligations(
                 cache.oldForm._id,
                 UserData(
                   cache.form.formData,
                   cache.form.status,
                   cache.form.visitsIndex,
                   cache.form.thirdPartyData,
                   cache.form.obligations),
                 cache.oldForm,
                 cache.form
               )
      (data, sections) <- processDataService.recalculateDataAndSections(dataRaw, cache)
      (errors, v, envelope) <- suppressErrors match {
                                case SeYes => envelopeF(envelopeId).map((List.empty, ValidationResult.empty.valid, _))
                                case SeNo =>
                                  validate(
                                    data,
                                    sections,
                                    sectionNumber,
                                    envelopeId,
                                    retrievals,
                                    cache.form.thirdPartyData,
                                    cache.formTemplate)
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
        visitsIndex,
        lang,
        obligations
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
            BadRequest(
              continue_form_page(
                cache.formTemplate,
                choice.bindFromRequest().withError("decision", "error.required"),
                maybeAccessCode,
                lang,
                frontendAppConfig)).pure[Future], {
            case "continue" =>
              val dataRaw = FormDataHelpers.formDataMap(cache.form.formData) + cache.form.visitsIndex.toVisitsTuple
              redirectWithRecalculation(cache, dataRaw, maybeAccessCode, lang)
            case "delete" =>
              deleteForm(maybeAccessCode, lang, cache)
            case _ => Redirect(routes.FormController.newForm(formTemplateId, lang)).pure[Future]
          }
        )
    }

  def deleteForm(maybeAccessCode: Option[AccessCode], lang: Option[String], cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier): Future[Result] = {
    val formTemplateId = cache.formTemplate._id
    gformConnector
      .deleteForm(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
      .map(_ =>
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals) match {
          case (Some(FormAccessCodeForAgents), IsAgent()) =>
            Redirect(routes.FormController.newFormAgent(formTemplateId, lang))
          case _ => Redirect(routes.FormController.newForm(formTemplateId, lang))
      })
  }
  def delete(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    lang: Option[String]): Action[AnyContent] =
    auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
      deleteForm(maybeAccessCode, lang, cache)
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
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate
  )(
    implicit request: Request[AnyContent]
  ): Future[(List[(FormComponent, FormFieldValidationResult)], ValidatedType[ValidationResult], Envelope)] = {
    val section = sections(sectionNumber.value)
    val nonSubmittedYet = nonSubmittedFCsOfNonGroup(formDataRecalculated, section)
    val allFC = submittedFCs(formDataRecalculated, sections.flatMap(_.expandSection(formDataRecalculated.data).allFCs)) ++ nonSubmittedYet
    val sectionFields = submittedFCs(formDataRecalculated, section.expandSection(formDataRecalculated.data).allFCs) ++ nonSubmittedYet

    for {
      envelope <- envelopeF(envelopeId)
      v <- validationService
            .validateForm(sectionFields, section, envelopeId, retrievals, thirdPartyData, formTemplate)(
              formDataRecalculated)
    } yield (validationService.evaluateValidation(v, allFC, formDataRecalculated, envelope), v, envelope)
  }

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    lang: Option[String]
  ) = auth.async(formTemplateId, lang, maybeAccessCode) { implicit request => cache =>
    processResponseDataFromBody(request) { (dataRaw: Data) =>
      val formId = FormId(cache.retrievals, formTemplateId, maybeAccessCode)

      def validateAndUpdateData(cache: AuthCacheWithForm, processData: ProcessData)(
        toResult: Option[SectionNumber] => Result): Future[Result] =
        for {
          Some(FormValidationOutcome(_, formData, v)) <- validateForm(
                                                          processData.data,
                                                          processData.sections,
                                                          sectionNumber,
                                                          cache)
          res <- {
            val before: ThirdPartyData = cache.form.thirdPartyData
            val after: ThirdPartyData = before.updateFrom(v)

            val needsSecondPhaseRecalculation =
              (before.desRegistrationResponse, after.desRegistrationResponse).mapN(_ =!= _)

            val cacheUpd = cache.copy(form = cache.form.copy(thirdPartyData = after, formData = formData))

            if (needsSecondPhaseRecalculation.getOrElse(false)) {
              val newDataRaw = formData.copy(fields = cache.form.visitsIndex.toFormField +: formData.fields).toData
              for {
                newProcessData <- processDataService.getProcessData(newDataRaw, cacheUpd)
                result         <- validateAndUpdateData(cacheUpd, newProcessData)(toResult) // recursive call
              } yield result
            } else {
              updateUserData(cacheUpd, processData)(toResult)
            }
          }
        } yield res

      def updateUserData(cache: AuthCacheWithForm, processData: ProcessData)(
        toResult: Option[SectionNumber] => Result): Future[Result] =
        for {
          maybeSn <- fastForwardValidate(processData, cache)
          userData = UserData(
            cache.form.formData,
            maybeSn.fold(Summary: FormStatus)(_ => InProgress),
            processData.visitIndex,
            cache.form.thirdPartyData,
            cache.form.obligations
          )
          res <- gformConnector.updateUserData(formId, userData).map(_ => toResult(maybeSn))
        } yield res

      def processSaveAndContinue(
        processData: ProcessData
      ): Future[Result] =
        validateAndUpdateData(cache, processData) {
          case Some(sn) =>
            val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sn.value).title)
            Redirect(
              routes.FormController
                .form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, lang, SuppressErrors(sectionNumber < sn)))
          case None =>
            Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, lang))
        }

      def processSaveAndExit(processData: ProcessData): Future[Result] =
        validateAndUpdateData(cache, processData) { maybeSn =>
          val formTemplate = cache.formTemplate
          val envelopeExpiryDate = cache.form.envelopeExpiryDate
          maybeAccessCode match {
            case Some(accessCode) =>
              Ok(save_with_access_code(accessCode, formTemplate, lang, frontendAppConfig))
            case None =>
              val call = maybeSn match {
                case Some(sn) =>
                  val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sn.value).title)
                  routes.FormController.form(formTemplateId, None, sn, sectionTitle4Ga, lang, SeYes)
                case None => routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, lang)
              }
              Ok(save_acknowledgement(envelopeExpiryDate, formTemplate, call, lang, frontendAppConfig))
          }
        }

      def processBack(processData: ProcessData, sn: SectionNumber): Future[Result] =
        validateAndUpdateData(cache, processData) { _ =>
          val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sn.value).title)
          Redirect(routes.FormController.form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, lang, SeYes))
        }

      def handleGroup(processData: ProcessData, anchor: String): Future[Result] =
        validateAndUpdateData(cache, processData) { _ =>
          val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sectionNumber.value).title)
          Redirect(
            routes.FormController
              .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, lang, SeYes)
              .url + anchor
          )
        }

      def processAddGroup(processData: ProcessData, groupId: String): Future[Result] = {
        val startPos = groupId.indexOf('-') + 1
        val groupComponentId = FormComponentId(groupId.substring(startPos))
        val maybeGroupFc = findFormComponent(groupComponentId, processData.sections)
        val (updatedData, anchor) = addNextGroup(maybeGroupFc, processData.data)

        handleGroup(processData.copy(data = updatedData), anchor.map("#" + _).getOrElse(""))
      }

      def processRemoveGroup(processData: ProcessData, idx: Int, groupId: String): Future[Result] = {
        val maybeGroupFc = findFormComponent(FormComponentId(groupId), processData.sections)
        val updatedData = removeGroupFromData(idx, maybeGroupFc, processData.data)

        handleGroup(processData.copy(data = updatedData), "")
      }

      for {
        processData <- processDataService.getProcessData(dataRaw, cache)
        nav = new Navigator(sectionNumber, processData.sections, processData.data).navigate
        res <- nav match {
                case SaveAndContinue           => processSaveAndContinue(processData)
                case SaveAndExit               => processSaveAndExit(processData)
                case Back(sn)                  => processBack(processData, sn)
                case AddGroup(groupId)         => processAddGroup(processData, groupId)
                case RemoveGroup(idx, groupId) => processRemoveGroup(processData, idx, groupId)
              }
      } yield res
    }
  }

  private lazy val firstSection = SectionNumber(0)
  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
