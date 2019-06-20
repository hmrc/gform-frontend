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
import com.softwaremill.quicklens._
import play.api.data
import play.api.i18n.{ DefaultLangs, I18nSupport }
import play.api.mvc._
import uk.gov.hmrc.gform.auth.models.{ IsAgent, MaterialisedRetrievals }
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gform.handlers.{ FormControllerRequestHandler, FormHandlerResult, NotToBeRedirected, ToBeRedirected }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.gform.{ ForceReload, FormValidationOutcome, NoSpecificAction, ObligationsAction }
import uk.gov.hmrc.gform.models.{ AgentAccessCode, ProcessData, ProcessDataService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SectionTitle4Ga._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ UserId => _, _ }
import uk.gov.hmrc.gform.validation.ValidationService
import uk.gov.hmrc.gform.views.html.form._
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

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
  handler: FormControllerRequestHandler,
  lookupExtractors: LookupExtractors,
  langs: DefaultLangs
) extends FrontendController {

  import i18nSupport._

  implicit val frontendConfig: FrontendAppConfig = frontendAppConfig

  // TODO: this method should really be in the SignOutController which does not yet exist
  def keepAlive() = auth.keepAlive()

  def dashboard(formTemplateId: FormTemplateId) = auth.async(formTemplateId) {
    implicit request => implicit lang => cache =>
      import cache._

      val formId = FormId(retrievals, formTemplateId, None)

      gformConnector.maybeForm(formId).map { form =>
        handler.handleDashboard(formTemplate, retrievals, form) match {
          case NotToBeRedirected(_) =>
            Ok(access_code_start(formTemplate, AgentAccessCode.form, frontendAppConfig))
          case ToBeRedirected => Redirect(routes.FormController.newForm(formTemplateId))
        }
      }
  }

  def newFormAgent(formTemplateId: FormTemplateId) = auth.async(formTemplateId) {
    implicit request => implicit l => cache =>
      val accessCode = AccessCode(AgentAccessCode.random.value)
      for {
        _ <- startForm(formTemplateId, cache.retrievals, Some(accessCode))
      } yield
        Redirect(routes.FormController.showAccessCode(formTemplateId))
          .flashing(AgentAccessCode.key -> accessCode.value)
  }

  def showAccessCode(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.async(formTemplateId) { implicit request => implicit l => cache =>
      Future.successful {
        val accessCode = request.flash.get(AgentAccessCode.key)
        handler.handleAccessCode(accessCode) match {
          case NotToBeRedirected(code) =>
            Ok(start_new_form(cache.formTemplate, AgentAccessCode(code), frontendAppConfig))
          case ToBeRedirected => Redirect(routes.FormController.dashboard(formTemplateId))
        }
      }
    }

  def newForm(formTemplateId: FormTemplateId) = auth.async(formTemplateId) { implicit request => implicit l => cache =>
    val noAccessCode = Option.empty[AccessCode]
    for {
      (formId, wasFormFound) <- getOrStartForm(formTemplateId, cache.retrievals, noAccessCode)
      result <- if (wasFormFound) {
                 Ok(continue_form_page(cache.formTemplate, choice, noAccessCode, frontendAppConfig))
                   .pure[Future]
               } else {
                 for {
                   maybeForm <- getForm(formId)
                   res <- maybeForm match {
                           case Some(form) =>
                             redirectFromEmpty(cache, form, noAccessCode, NoSpecificAction)
                           case None => Future.failed(new NotFoundException(s"Form with id $formId not found."))
                         }
                 } yield res
               }
    } yield result
  }

  private def redirectFromEmpty(
    cache: AuthCacheWithoutForm,
    form: Form,
    maybeAccessCode: Option[AccessCode],
    obligationsAction: ObligationsAction)(implicit request: Request[AnyContent], l: LangADT) = {
    val dataRaw = Map.empty[FormComponentId, Seq[String]]
    val cacheWithForm = cache.toAuthCacheWithForm(form)
    redirectWithRecalculation(cacheWithForm, dataRaw, maybeAccessCode, obligationsAction)
  }

  private def redirectWithRecalculation(
    cache: AuthCacheWithForm,
    dataRaw: Data,
    maybeAccessCode: Option[AccessCode],
    obligationsAction: ObligationsAction)(implicit request: Request[AnyContent], l: LangADT): Future[Result] =
    for {
      processData <- processDataService
                      .getProcessData(dataRaw, cache, gformConnector.getAllTaxPeriods, obligationsAction)
      res <- updateUserData(cache, processData)(redirectResult(cache, maybeAccessCode, processData, _))
    } yield res

  private def redirectResult(
    cache: AuthCacheWithForm,
    maybeAccessCode: Option[AccessCode],
    processData: ProcessData,
    maybeSectionNumber: Option[SectionNumber])(implicit l: LangADT): Result =
    maybeSectionNumber match {
      case Some(sn) =>
        val section = processData.sections(sn.value)
        val sectionTitle4Ga = sectionTitle4GaFactory(section.title.value)
        Redirect(
          routes.FormController
            .form(cache.formTemplate._id, maybeAccessCode, sn, sectionTitle4Ga, SeYes))
      case None => Redirect(routes.SummaryController.summaryById(cache.formTemplate._id, maybeAccessCode))
    }

  def newFormPost(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.async(formTemplateId) { implicit request => implicit l => cache =>
      AgentAccessCode.form.bindFromRequest.fold(
        hasErrors => Future.successful(BadRequest(access_code_start(cache.formTemplate, hasErrors, frontendAppConfig))),
        accessCodeF => {
          accessCodeF.accessOption match {
            case AgentAccessCode.optionNew =>
              Future.successful(Redirect(routes.FormController.newFormAgent(formTemplateId)))
            case AgentAccessCode.optionAccess => {
              val maybeAccessCode: Option[AccessCode] = accessCodeF.accessCode.map(a => AccessCode(a))
              for {
                maybeForm <- getForm(FormId(cache.retrievals, formTemplateId, maybeAccessCode))
                res <- maybeForm match {
                        case Some(form) =>
                          redirectFromEmpty(cache, form, maybeAccessCode, NoSpecificAction)
                        case None =>
                          BadRequest(
                            access_code_start(
                              cache.formTemplate,
                              AgentAccessCode.form.bindFromRequest().withError(AgentAccessCode.key, "error.notfound"),
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
                        fileUploadService.getMaybeEnvelope(f.envelopeId)
                      }
      mayBeFormExceptWithEnvelope <- (maybeFormExceptSubmitted, maybeEnvelope) match {
                                      case (None, _)          => None.pure[Future]
                                      case (Some(f), None)    => gformConnector.deleteForm(f._id).map(_ => None)
                                      case (Some(_), Some(_)) => maybeFormExceptSubmitted.pure[Future]
                                    }
    } yield mayBeFormExceptWithEnvelope

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
    suppressErrors: SuppressErrors) = auth.async(formTemplateId, maybeAccessCode) {
    implicit request => implicit l => cache =>
      handler
        .handleForm(
          sectionNumber,
          suppressErrors,
          cache,
          processDataService.recalculateDataAndSections,
          fileUploadService.getEnvelope,
          validationService.validateFormComponents,
          validationService.evaluateValidation
        )
        .map(handlerResult =>
          Ok(renderer.renderSection(
            maybeAccessCode,
            cache.form,
            sectionNumber,
            handlerResult.data,
            cache.formTemplate,
            handlerResult.result,
            handlerResult.envelope,
            cache.form.envelopeId,
            handlerResult.validatedType,
            handlerResult.sections,
            formMaxAttachmentSizeMB,
            contentTypes,
            cache.retrievals,
            cache.form.visitsIndex.visit(sectionNumber),
            cache.form.thirdPartyData.obligations
          )))
  }

  def fileUploadPage(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber,
    sectionTitle4Ga: SectionTitle4Ga,
    fId: String) = auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
    val fileId = FileId(fId)

    val `redirect-success-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, SeYes)
    val `redirect-error-url` = appConfig.`gform-frontend-base-url` + routes.FormController
      .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, SeYes)

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
        frontendAppConfig)
    ).pure[Future]
  }

  val choice: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "decision" -> play.api.data.Forms.nonEmptyText
    ))

  def decision(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
      choice.bindFromRequest
        .fold(
          _ =>
            BadRequest(
              continue_form_page(
                cache.formTemplate,
                choice.bindFromRequest().withError("decision", "error.required"),
                maybeAccessCode,
                frontendAppConfig)).pure[Future], {
            case "continue" =>
              val dataRaw = FormDataHelpers.formDataMap(cache.form.formData) + cache.form.visitsIndex.toVisitsTuple
              redirectWithRecalculation(cache, dataRaw, maybeAccessCode, ForceReload)
            case "delete" =>
              deleteForm(maybeAccessCode, cache)
            case _ => Redirect(routes.FormController.newForm(formTemplateId)).pure[Future]
          }
        )
    }

  def deleteForm(maybeAccessCode: Option[AccessCode], cache: AuthCacheWithForm)(
    implicit hc: HeaderCarrier): Future[Result] = {
    val formTemplateId = cache.formTemplate._id
    gformConnector
      .deleteForm(cache.form._id)
      .map(_ =>
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals) match {
          case (Some(FormAccessCodeForAgents), IsAgent()) =>
            Redirect(routes.FormController.newFormAgent(formTemplateId))
          case _ => Redirect(routes.FormController.newForm(formTemplateId))
      })
  }
  def delete(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
      deleteForm(maybeAccessCode, cache)
    }

  val deleteOnExit = delete _

  private def updateUserData(cache: AuthCacheWithForm, processData: ProcessData)(
    toResult: Option[SectionNumber] => Result)(implicit request: Request[AnyContent], l: LangADT): Future[Result] =
    for {
      maybeSn <- handler.handleFastForwardValidate(
                  processData,
                  cache,
                  FormService.extractedValidateFormHelper,
                  fileUploadService.getEnvelope,
                  validationService.validateFormComponents,
                  validationService.evaluateValidation
                )
      userData = UserData(
        cache.form.formData,
        maybeSn.fold(Summary: FormStatus)(_ => InProgress),
        processData.visitIndex,
        cache.form.thirdPartyData.modify(_.obligations).setTo(processData.obligations)
      )
      res <- gformConnector.updateUserData(cache.form._id, userData).map(_ => toResult(maybeSn))
    } yield res

  def updateFormData(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    sectionNumber: SectionNumber
  ) = auth.async(formTemplateId, maybeAccessCode) { implicit request => implicit l => cache =>
    processResponseDataFromBody(request) { (dataRaw: Data) =>
      def validateAndUpdateData(cache: AuthCacheWithForm, processData: ProcessData)(
        toResult: Option[SectionNumber] => Result): Future[Result] =
        for {
          FormValidationOutcome(_, formData, v) <- handler.handleFormValidation(
                                                    processData.data,
                                                    processData.sections,
                                                    sectionNumber,
                                                    cache,
                                                    FormService.extractedValidateFormHelper,
                                                    fileUploadService.getEnvelope,
                                                    validationService.validateFormComponents,
                                                    validationService.evaluateValidation
                                                  )
          res <- {
            val before: ThirdPartyData = cache.form.thirdPartyData
            val after: ThirdPartyData = before.updateFrom(v)

            val needsSecondPhaseRecalculation =
              (before.desRegistrationResponse, after.desRegistrationResponse).mapN(_ =!= _)

            val cacheUpd =
              cache.copy(
                form = cache.form
                  .copy(thirdPartyData = after.copy(obligations = processData.obligations), formData = formData))

            if (needsSecondPhaseRecalculation.getOrElse(false)) {
              val newDataRaw = formData.copy(fields = cache.form.visitsIndex.toFormField +: formData.fields).toData
              for {
                newProcessData <- processDataService
                                   .getProcessData(
                                     newDataRaw,
                                     cacheUpd,
                                     gformConnector.getAllTaxPeriods,
                                     NoSpecificAction)
                result <- validateAndUpdateData(cacheUpd, newProcessData)(toResult) // recursive call
              } yield result
            } else {
              updateUserData(cacheUpd, processData)(toResult)
            }
          }
        } yield res

      def processSaveAndContinue(
        processData: ProcessData
      ): Future[Result] =
        validateAndUpdateData(cache, processData) {
          case Some(sn) =>
            val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sn.value).title.value)
            Redirect(
              routes.FormController
                .form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, SuppressErrors(sectionNumber < sn)))
          case None =>
            Redirect(routes.SummaryController.summaryById(formTemplateId, maybeAccessCode))
        }

      def processSaveAndExit(processData: ProcessData): Future[Result] =
        validateAndUpdateData(cache, processData) { maybeSn =>
          val formTemplate = cache.formTemplate
          val envelopeExpiryDate = cache.form.envelopeExpiryDate
          maybeAccessCode match {
            case Some(accessCode) =>
              Ok(save_with_access_code(accessCode, formTemplate, frontendAppConfig))
            case None =>
              val call = maybeSn match {
                case Some(sn) =>
                  val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sn.value).title.value)
                  routes.FormController.form(formTemplateId, None, sn, sectionTitle4Ga, SeYes)
                case None => routes.SummaryController.summaryById(formTemplateId, maybeAccessCode)
              }
              Ok(save_acknowledgement(envelopeExpiryDate, formTemplate, call, frontendAppConfig))
          }
        }

      def processBack(processData: ProcessData, sn: SectionNumber): Future[Result] =
        validateAndUpdateData(cache, processData) { _ =>
          val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sn.value).title.value)
          Redirect(routes.FormController.form(formTemplateId, maybeAccessCode, sn, sectionTitle4Ga, SeYes))
        }

      def handleGroup(processData: ProcessData, anchor: String): Future[Result] =
        validateAndUpdateData(cache, processData) { _ =>
          val sectionTitle4Ga = sectionTitle4GaFactory(processData.sections(sectionNumber.value).title.value)
          Redirect(
            routes.FormController
              .form(formTemplateId, maybeAccessCode, sectionNumber, sectionTitle4Ga, SeYes)
              .url + anchor
          )
        }

      def processAddGroup(processData: ProcessData, groupId: String): Future[Result] = {
        val startPos = groupId.indexOf('-') + 1
        val groupComponentId = FormComponentId(groupId.substring(startPos))
        val maybeGroupFc = findFormComponent(groupComponentId, processData.sections)
        val (updatedData, anchor) = addNextGroup(maybeGroupFc, processData.data, lookupExtractors)

        handleGroup(processData.copy(data = updatedData), anchor.map("#" + _).getOrElse(""))
      }

      def processRemoveGroup(processData: ProcessData, idx: Int, groupId: String): Future[Result] = {
        val maybeGroupFc = findFormComponent(FormComponentId(groupId), processData.sections)
        val updatedData = removeGroupFromData(idx, maybeGroupFc, processData.data)

        handleGroup(processData.copy(data = updatedData), "")
      }

      for {
        processData <- processDataService
                        .getProcessData(dataRaw, cache, gformConnector.getAllTaxPeriods, NoSpecificAction)
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

  private lazy val formMaxAttachmentSizeMB = appConfig.formMaxAttachmentSizeMB
  private lazy val contentTypes = appConfig.contentTypes
}
