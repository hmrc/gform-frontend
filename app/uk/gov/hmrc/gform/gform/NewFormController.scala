/*
 * Copyright 2023 HM Revenue & Customs
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
import cats.syntax.applicative._
import cats.syntax.eq._
import com.softwaremill.quicklens._
import org.slf4j.LoggerFactory
import play.api.data
import play.api.i18n.{ I18nSupport, Lang, Messages }
import play.api.mvc._
import uk.gov.hmrc.auth.core.ConfidenceLevel
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.api.NinoInsightsConnector
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers.CookieNames._
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.eval.InitFormEvaluator
import uk.gov.hmrc.gform.eval.smartstring.RealSmartStringEvaluatorFactory
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gformbackend.{ GformBackEndAlgebra, GformConnector }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.{ AccessCodePage, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.objectStore.{ Envelope, ObjectStoreService }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode.toJsonStr
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.submission.Submission
import uk.gov.hmrc.gform.views.hardcoded._
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

case class AccessCodeForm(accessCode: String, isContinue: String)

class NewFormController(
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  objectStoreService: ObjectStoreService,
  gformConnector: GformConnector,
  fastForwardService: FastForwardService,
  auditService: AuditService,
  messagesControllerComponents: MessagesControllerComponents,
  gformBackEnd: GformBackEndAlgebra[Future],
  ninoInsightsConnector: NinoInsightsConnector[Future],
  englishMessages: Messages,
  acknowledgementPdfService: AcknowledgementPdfService
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {
  import i18nSupport._

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val frontendConfig: FrontendAppConfig = frontendAppConfig

  private val noAccessCode = Option.empty[AccessCode]

  private def formTemplateIdCookie(formTemplateId: FormTemplateId) =
    Cookie(formTemplateIdCookieName, formTemplateId.value, secure = true)

  def dashboard(formTemplateId: FormTemplateId, se: SuppressErrors) =
    auth.authWithOptReferrerCheckWithoutRetrievingForm(
      formTemplateId,
      OperationWithoutForm.ViewDashboard
    ) { implicit request => implicit lang => cache =>
      val cookie = formTemplateIdCookie(formTemplateId)

      def checkDraftRetrievalMethod(draftRetrievalMethod: DraftRetrievalMethod) =
        (draftRetrievalMethod, cache.retrievals) match {
          case (BySubmissionReference, _) => showAccesCodePage(cache, cache.formTemplate, se)
          case (FormAccessCodeForAgents(_), IsAgent()) | (FormAccessCode(_), _) =>
            exitPageHandler(cache, se, showAccesCodePage)
          case _ =>
            Redirect(routes.NewFormController.newOrContinue(formTemplateId).url, request.queryString).pure[Future]
        }

      if (!cache.formTemplate.languages.languages.contains(lang)) {
        Redirect(
          routes.NewFormController.dashboard(
            formTemplateId
          )
        ).withLang(Lang("en")).pure[Future]
      } else {
        val result =
          cache.formTemplate.draftRetrieval
            .flatMap(dr => cache.retrievals.getAffinityGroup.flatMap(ag => dr.mapping.get(ag)))
            .collect { case drm => checkDraftRetrievalMethod(drm) }
            .getOrElse(checkDraftRetrievalMethod(cache.formTemplate.draftRetrievalMethod))

        result.map(_.withCookies(cookie))
      }
    }

  def dashboardClean(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.EditForm) {
      implicit request => _ => cache => _ => _ =>
        val queryParams = QueryParams.fromRequest(request)
        fastForwardService.deleteForm(formTemplateId, cache, queryParams)
    }

  def dashboardNewFormLink(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.NewFormLink) {
      implicit request => _ => cache => _ => _ =>
        val queryParams = QueryParams.fromRequest(request)
        fastForwardService.deleteForm(formTemplateId, cache, queryParams)
    }

  /** To request a new confirmation code when verifying an email, user will have to start whole journey again in new session.
    */
  def dashboardWithNewSession(formTemplateId: FormTemplateId) = Action.async { _ =>
    Redirect(
      frontendConfig.getBasGatewayFrontendSignOutUrl(
        Option(routes.NewFormController.dashboard(formTemplateId).url)
      )
    ).pure[Future]
  }

  def dashboardWithCompositeAuth(formTemplateId: FormTemplateId) = Action.async { implicit request =>
    val compositeAuthDetails: CompositeAuthDetails =
      jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
    val formTemplateContext = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateContext.formTemplate
    Redirect(routes.NewFormController.dashboard(formTemplateId).url, request.queryString)
      .addingToSession(
        COMPOSITE_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
          compositeAuthDetails.add(formTemplate, AuthConfig.hmrcSimpleModule)
        )
      )
      .pure[Future]
  }

  /** This handles cases when draftRetrievalMethod submissionReference or formAccessCodeForAgents has been
    * added to the formTemplate after form went live without draftRetrievalMethod.
    *
    * It will try to load form without accessCode and only if such a form doesn't exists present user with Access Code page
    */
  private def showAccesCodePage(cache: AuthCacheWithoutForm, formTemplate: FormTemplate, se: SuppressErrors)(implicit
    request: Request[AnyContent],
    l: LangADT
  ) = {
    val userId = UserId(cache.retrievals)
    val formTemplateId = cache.formTemplate._id
    val formIdData = FormIdData.Plain(userId, formTemplateId)

    def showAccessCodePage: Future[Result] =
      cache.formTemplate.draftRetrieval
        .flatMap(dr => cache.retrievals.getAffinityGroup.flatMap(ag => dr.mapping.get(ag)))
        .collect { case drm => checkDraftRetrievalMethod(drm) }
        .getOrElse(checkDraftRetrievalMethod(cache.formTemplate.draftRetrievalMethod))

    def checkDraftRetrievalMethod(draftRetrievalMethod: DraftRetrievalMethod): Future[Result] =
      draftRetrievalMethod match {
        case BySubmissionReference => showAccessCodeList(cache, userId, formTemplateId)
        case _ =>
          val accessCodeStart = AccessCodePage.decision
            .bindFromRequest()
            .fold(
              errors => new AccessCodeStart(cache.formTemplate, errors, frontendConfig, se),
              _ => new AccessCodeStart(cache.formTemplate, AccessCodePage.decision, frontendConfig, se)
            )

          def switchLanguageForAgentCodeStart: (FormTemplateId, Option[AccessCode], String) => Call =
            (formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], lang: String) =>
              uk.gov.hmrc.gform.gform.routes.LanguageSwitchController
                .switchToLanguageNoDataChange(lang)
          Ok(
            access_code_start(
              frontendAppConfig.copy(routeToSwitchLanguageDataChange = switchLanguageForAgentCodeStart),
              accessCodeStart
            )
          )
            .pure[Future]
      }

    handleForm(formIdData, cache.formTemplate)(showAccessCodePage) { form =>
      Redirect(routes.NewFormController.newOrContinue(formTemplateId)).pure[Future]
    }
  }

  private def showAccessCodeList(cache: AuthCacheWithoutForm, userId: UserId, formTemplateId: FormTemplateId)(implicit
    request: Request[AnyContent],
    l: LangADT
  ) =
    for {
      formOverviews <- gformConnector.getAllForms(userId, formTemplateId)
    } yield {
      val accessCodeList = new AccessCodeList(cache.formTemplate, formOverviews)
      Ok(
        access_code_list(
          accessCodeList,
          frontendAppConfig
        )
      )
    }

  def showAccessCode(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.ShowAccessCode) {
      implicit request => implicit l => cache =>
        Future.successful {
          val accessCode = request.flash.get(AccessCodePage.key)
          accessCode match {
            case Some(code) =>
              val displayAccessCode = new DisplayAccessCode(cache.formTemplate, AccessCode(code))
              Ok(start_new_form(displayAccessCode, frontendAppConfig))
            case None => Redirect(routes.NewFormController.dashboard(formTemplateId))
          }
        }
    }

  private val choice: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "decision" -> play.api.data.Forms.nonEmptyText
    )
  )

  private def removeConfirmations(
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  )(implicit m: Messages): AuthCacheWithForm = {

    val (confirmations, currentConfirmations) = ConfirmationService
      .processConfirmation(
        formModelOptics,
        cache.form
      )

    val confirmationToReset = confirmations
      .map(_.question.id.modelComponentId)
      .toSet

    val updateFormField = cache.form.formData.fields.filter(formField => !confirmationToReset(formField.id))

    cache
      .modify(_.form.formData.fields)
      .using(_ => updateFormField)
      .modify(_.form.thirdPartyData.confirmations)
      .using(_ => Some(currentConfirmations))
  }

  def decision(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => sse => formModelOptics =>
        val queryParams = QueryParams.fromRequest(request)
        choice
          .bindFromRequest()
          .fold(
            errorForm => {

              val continueFormPage = new ContinueFormPage(cache.formTemplate, errorForm)

              BadRequest(continue_form_page(frontendAppConfig, continueFormPage)).pure[Future]
            },
            {
              case "continue" =>
                val cacheUpd = removeConfirmations(cache, formModelOptics)

                for {
                  updatedCache <- maybeUpdateItmpCache(request, cacheUpd, formModelOptics)
                  res <-
                    cache.formTemplate.formKind.fold(_ =>
                      fastForwardService
                        .redirectFastForward[SectionSelectorType.Normal](
                          updatedCache,
                          noAccessCode,
                          formModelOptics,
                          None,
                          SuppressErrors.Yes
                        )
                    )(_ =>
                      Redirect(
                        uk.gov.hmrc.gform.tasklist.routes.TaskListController
                          .landingPage(cache.formTemplateId, noAccessCode)
                      ).pure[Future]
                    )
                } yield res
              case "delete" => fastForwardService.deleteForm(formTemplateId, cache, queryParams)
              case _        => Redirect(routes.NewFormController.newOrContinue(formTemplateId)).pure[Future]
            }
          )
    }

  private val downloadOrNewChoice: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "downloadOrNew" -> play.api.data.Forms.nonEmptyText
    )
  )

  def downloadDecision(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        val queryParams: QueryParams = QueryParams.fromRequest(request)
        downloadOrNewChoice
          .bindFromRequest()
          .fold(
            _ =>
              Redirect(
                routes.NewFormController.downloadOldOrNewForm(formTemplateId, SuppressErrors.No).url,
                queryParams.toPlayQueryParams
              ).pure[Future],
            {
              case "download" =>
                Redirect(
                  routes.NewFormController.lastSubmission(formTemplateId, noAccessCode).url,
                  queryParams.toPlayQueryParams
                )
                  .pure[Future]
              case "startNew" => newForm(formTemplateId, cache, queryParams)
              case _ =>
                Redirect(routes.NewFormController.newOrContinue(formTemplateId).url, queryParams.toPlayQueryParams)
                  .pure[Future]
            }
          )
    }

  def downloadOldOrNewForm(
    formTemplateId: FormTemplateId,
    se: SuppressErrors
  ) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        for {
          formIdData <- Future.successful(FormIdData.Plain(UserId(cache.retrievals), cache.formTemplate._id))
          maybeForm  <- gformConnector.maybeForm(formIdData, cache.formTemplate)
          maybeSubmission <-
            maybeForm
              .filter(_.status === Submitted && cache.formTemplate.downloadPreviousSubmissionPdf)
              .fold(Option.empty[Submission].pure[Future]) { form =>
                gformConnector
                  .submissionDetails(
                    FormIdData(cache.retrievals, cache.formTemplate._id, noAccessCode),
                    form.envelopeId
                  )
                  .flatMap { maybeCurrentSubmission =>
                    maybeCurrentSubmission.fold {
                      cache.formTemplate.legacyFormIds.fold(Option.empty[Submission].pure[Future]) { legacyFormIds =>
                        gformConnector.getSubmissionByLegacyIds(
                          FormIdData(cache.retrievals, cache.formTemplate._id, noAccessCode),
                          form.envelopeId
                        )(legacyFormIds)
                      }
                    } { currentSubmission =>
                      Some(currentSubmission).pure[Future]
                    }
                  }
              }
          res <-
            maybeSubmission.fold(newForm(formTemplateId, cache, QueryParams.fromRequest(request))) { sub =>
              val downloadOrNewFormPage: DownloadOrNewFormPage = downloadOrNewChoice
                .bindFromRequest()
                .fold(
                  errorForm =>
                    new DownloadOrNewFormPage(cache.formTemplate, errorForm, sub.submittedDate, se, frontendAppConfig),
                  _ =>
                    new DownloadOrNewFormPage(
                      cache.formTemplate,
                      downloadOrNewChoice,
                      sub.submittedDate,
                      se,
                      frontendAppConfig
                    )
                )
              Ok(download_or_new(frontendAppConfig, downloadOrNewFormPage)).pure[Future]
            }
        } yield res
    }

  def newSubmissionReference(formTemplateId: FormTemplateId) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        newForm(formTemplateId, cache, QueryParams.empty)
    }

  def continue(cache: AuthCacheWithoutForm, formTemplate: FormTemplate, se: SuppressErrors)(implicit
    request: Request[AnyContent],
    lang: LangADT
  ): Future[Result] = {
    val queryParams: QueryParams = QueryParams.fromRequest(request)

    for {
      formIdData <- Future.successful(FormIdData.Plain(UserId(cache.retrievals), formTemplate._id))
      _ <- if (appConfig.`email-to-gg-migration`) {
             gformConnector.migrateEmailToGG(formIdData, cache.retrievals, formTemplate)
           } else { Future.successful(()) }
      res <-
        handleForm(formIdData, formTemplate)(
          Redirect(routes.NewFormController.downloadOldOrNewForm(formTemplate._id).url, queryParams.toPlayQueryParams)
            .pure[Future]
        ) { form =>
          for {
            formTemplate <- if (formTemplate._id === form.formTemplateId)
                              formTemplate.pure[Future]
                            else gformConnector.getFormTemplate(form.formTemplateId)

            res <- cache.formTemplate.draftRetrieval
                     .flatMap(dr => cache.retrievals.getAffinityGroup.flatMap(ag => dr.mapping.get(ag)))
                     .collect { case drm => checkDraftRetrievalMethod(drm, formTemplate, cache, form, queryParams) }
                     .getOrElse(
                       checkDraftRetrievalMethod(
                         formTemplate.draftRetrievalMethod,
                         formTemplate,
                         cache,
                         form,
                         queryParams
                       )
                     )

          } yield res
        }
    } yield res
  }

  private def checkDraftRetrievalMethod(
    draftRetrievalMethod: DraftRetrievalMethod,
    formTemplate: FormTemplate,
    cache: AuthCacheWithoutForm,
    form: Form,
    queryParams: QueryParams
  )(implicit request: Request[AnyContent], lang: LangADT) =
    draftRetrievalMethod match {
      case NotPermitted =>
        fastForwardService.deleteForm(
          formTemplate._id,
          cache.toAuthCacheWithForm(form, noAccessCode),
          queryParams
        )
      case OnePerUser(ContinueOrDeletePage.Skip) | FormAccessCode(ContinueOrDeletePage.Skip) | FormAccessCode(
            ContinueOrDeletePage.Skip
          ) =>
        auditService.sendFormResumeEvent(form, cache.retrievals)
        redirectContinue[SectionSelectorType.Normal](
          cache.copy(formTemplate = formTemplate),
          form,
          noAccessCode,
          request
        )
      case _ =>
        auditService.sendFormResumeEvent(form, cache.retrievals)
        val continueFormPage = new ContinueFormPage(formTemplate, choice)
        Ok(continue_form_page(frontendAppConfig, continueFormPage)).pure[Future]
    }

  def newOrContinue(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        exitPageHandler(cache, SuppressErrors.Yes, continue)
    }

  private val startNewOrLogout: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "downloadThenNew" -> play.api.data.Forms.nonEmptyText
    )
  )

  def newOrSignout(formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode]): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        val queryParams: QueryParams = QueryParams.fromRequest(request)
        startNewOrLogout
          .bindFromRequest()
          .fold(
            _ =>
              Redirect(
                routes.NewFormController.lastSubmission(cache.formTemplate._id, maybeAccessCode, SuppressErrors.No)
              )
                .pure[Future],
            {
              case "signOut"  => Redirect(routes.SignOutController.signOut(cache.formTemplate._id)).pure[Future]
              case "startNew" => newForm(cache.formTemplate._id, cache, queryParams)
              case "startNewAccess" =>
                for {
                  (newCache, drm) <- getDraftRetrievalMethod(cache)
                  res             <- processNewForm(newCache, drm)
                } yield res
              case AccessCodePage.optionContinue =>
                Redirect(routes.NewFormController.accessCodeRetrieveForm(formTemplateId)).pure[Future]
              case AccessCodePage.optionDownload =>
                Redirect(routes.NewFormController.accessCodeDownload(formTemplateId)).pure[Future]
              case _ => Redirect(routes.NewFormController.newOrContinue(cache.formTemplate._id)).pure[Future]
            }
          )
    }

  def lastSubmission(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    se: SuppressErrors
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](
      formTemplateId,
      maybeAccessCode,
      OperationWithForm.DownloadSummaryPdf
    ) { implicit request => implicit l => cache => implicit ss => formModelOptics =>
      val queryParams: QueryParams = QueryParams.fromRequest(request)
      for {
        maybeSubmission <- gformConnector
                             .maybeOneOfSubmissionDetails(
                               FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
                               FormIdData(cache.retrievals, cache.formTemplateId, maybeAccessCode),
                               cache.form.envelopeId
                             )
        maybeDownloadableSub = maybeSubmission.filter(_ => cache.formTemplate.downloadPreviousSubmissionPdf)
        res <- maybeDownloadableSub.fold {
                 Redirect(routes.NewFormController.newOrContinue(formTemplateId).url, queryParams.toPlayQueryParams)
                   .pure[Future]
               } { submission =>
                 acknowledgementPdfService
                   .getRenderedPdfSize(
                     cache,
                     maybeAccessCode,
                     formModelOptics
                   )
                   .map { pdfSize =>
                     val downloadThenNew: DownloadThenNewFormPage = startNewOrLogout
                       .bindFromRequest()
                       .fold(
                         error =>
                           new DownloadThenNewFormPage(
                             cache.formTemplate,
                             error,
                             submission,
                             pdfSize,
                             maybeAccessCode,
                             se
                           ),
                         _ =>
                           new DownloadThenNewFormPage(
                             cache.formTemplate,
                             startNewOrLogout,
                             submission,
                             pdfSize,
                             maybeAccessCode,
                             se
                           )
                       )
                     Ok(download_then_new(frontendConfig, downloadThenNew))
                   }
               }
      } yield res
    }

  private def newForm(formTemplateId: FormTemplateId, cache: AuthCacheWithoutForm, queryParams: QueryParams)(implicit
    request: Request[AnyContent],
    l: LangADT
  ) = {

    def notFound(formIdData: FormIdData): Future[Result] =
      Future.failed(new NotFoundException(s"Form with id ${formIdData.toFormId} not found."))

    for {
      formIdData        <- startFreshForm(formTemplateId, cache.retrievals, queryParams)
      maybeFormTemplate <- gformConnector.maybeFormTemplate(formTemplateId)
      res <- maybeFormTemplate.fold(notFound(formIdData)) { formTemplate =>
               handleForm(formIdData, formTemplate)(notFound(formIdData)) { form =>
                 auditService.sendFormCreateEvent(form, cache.retrievals)
                 redirectContinue[SectionSelectorType.Normal](
                   cache.copy(formTemplate = formTemplate),
                   form,
                   formIdData.maybeAccessCode,
                   request
                 )
               }
             }
    } yield res
  }

  private def getDraftRetrievalMethod(
    cache: AuthCacheWithoutForm
  )(implicit hc: HeaderCarrier, messages: Messages): Future[(AuthCacheWithoutForm, DraftRetrievalMethod)] =
    for {
      newCache <-
        InitFormEvaluator
          .makeCacheWithDataRetrieve(
            cache,
            cache.formTemplate.authConfig,
            None,
            cache.formTemplate.dataRetrieve,
            gformConnector,
            ninoInsightsConnector
          )
      res <- newCache.formTemplate.draftRetrieval
               .flatMap(dr => newCache.retrievals.getAffinityGroup.flatMap(ag => dr.mapping.get(ag)))
               .collect {
                 case BySubmissionReference                => (newCache, BySubmissionReference)
                 case FormAccessCode(continueOrDeletePage) => (newCache, FormAccessCodeForAgents(continueOrDeletePage))
               }
               .getOrElse((newCache.formTemplate.draftRetrievalMethod, newCache.retrievals) match {
                 case (BySubmissionReference, _)                    => (newCache, BySubmissionReference)
                 case (drm @ FormAccessCodeForAgents(_), IsAgent()) => (newCache, drm)
                 case other                                         => throw new Exception(s"Invalid retrieval method, got $other")
               })
               .pure[Future]
    } yield res

  def accessCodeDownload(formTemplateId: FormTemplateId, se: SuppressErrors): Action[AnyContent] =
    accessCode(formTemplateId, se, isContinue = false)

  def accessCodeRetrieveForm(formTemplateId: FormTemplateId, se: SuppressErrors): Action[AnyContent] =
    accessCode(formTemplateId, se, isContinue = true)

  private val notFoundParm = QueryParam("nf")

  private def accessCode(formTemplateId: FormTemplateId, se: SuppressErrors, isContinue: Boolean): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit lang => cache =>
        for {
          (newCache, drm) <- getDraftRetrievalMethod(cache)
          res <- {
            val queryParams: QueryParams = QueryParams.fromRequest(request)
            val notFound: Boolean = queryParams.params.getOrElse(notFoundParm, QueryParamValue("false")).value.toBoolean
            val firstBindForm: data.Form[AccessCodeForm] = AccessCodePage.form(drm).bindFromRequest()
            val accessCodeForm: data.Form[AccessCodeForm] =
              if (notFound) firstBindForm.withError(AccessCodePage.key, "error.notfound") else firstBindForm

            val accessCodeEnter: AccessCodeEnter = accessCodeForm
              .fold(
                errorForm => new AccessCodeEnter(newCache.formTemplate, errorForm, isContinue, se),
                _ => new AccessCodeEnter(newCache.formTemplate, AccessCodePage.form(drm), isContinue, se)
              )
            def switchLanguageForAgentCodeStart: (FormTemplateId, Option[AccessCode], String) => Call =
              (formTemplateId: FormTemplateId, maybeAccessCode: Option[AccessCode], lang: String) =>
                uk.gov.hmrc.gform.gform.routes.LanguageSwitchController
                  .switchToLanguageNoDataChange(lang)

            Ok(
              access_code_enter(
                frontendAppConfig.copy(routeToSwitchLanguageDataChange = switchLanguageForAgentCodeStart),
                accessCodeEnter,
                routes.NewFormController.dashboard(formTemplateId)
              )
            ).pure[Future]
          }
        } yield res

    }

  def accessCodePost(formTemplateId: FormTemplateId): Action[AnyContent] = {
    def badRequest(errorForm: play.api.data.Form[AccessCodeForm], notFound: Boolean = false)(implicit
      request: Request[AnyContent]
    ) = {
      val isContinue = errorForm.data.getOrElse(AccessCodePage.isContinueKey, "false") == "true"

      val queryParams: QueryParams = QueryParams.fromRequest(request) + (
        QueryParam(AccessCodePage.key) -> QueryParamValue(errorForm.data.getOrElse(AccessCodePage.key, "")),
        notFoundParm                   -> QueryParamValue(notFound.toString)
      )

      val url =
        if (isContinue) routes.NewFormController.accessCodeRetrieveForm(formTemplateId, SuppressErrors.No).url
        else routes.NewFormController.accessCodeDownload(formTemplateId, SuppressErrors.No).url

      Redirect(
        url,
        queryParams.toPlayQueryParams
      ).pure[Future]
    }

    def notFound(drm: DraftRetrievalMethod)(implicit request: Request[AnyContent]) =
      badRequest(
        AccessCodePage
          .form(drm)
          .bindFromRequest(),
        notFound = true
      )

    def optionAccess(drm: DraftRetrievalMethod, access: String, cache: AuthCacheWithoutForm)(implicit
      hc: HeaderCarrier,
      request: Request[AnyContent],
      lang: LangADT
    ) = {
      val accessCode: AccessCode = AccessCode(access)
      val formIdData = FormIdData.WithAccessCode(UserId(cache.retrievals), formTemplateId, accessCode)
      handleForm(formIdData, cache.formTemplate)(notFound(drm)) { form =>
        redirectContinue[SectionSelectorType.Normal](cache, form, Some(accessCode), request)
      }
    }

    def optionDownload(drm: DraftRetrievalMethod, access: String, cache: AuthCacheWithoutForm)(implicit
      hc: HeaderCarrier,
      request: Request[AnyContent]
    ) = {
      val accessCode: AccessCode = AccessCode(access)
      for {
        formIdData <-
          Future.successful(
            FormIdData.WithAccessCode(UserId(cache.retrievals), cache.formTemplate._id, accessCode)
          )
        maybeForm <- gformConnector.maybeForm(formIdData, cache.formTemplate)
        res <- maybeForm
                 .filter(_.status === Submitted)
                 .fold(notFound(drm)) { _ =>
                   Redirect(
                     routes.NewFormController
                       .lastSubmission(cache.formTemplate._id, Some(accessCode), SuppressErrors.Yes)
                   ).pure[Future]
                 }
      } yield res
    }

    def processSubmittedData(cache: AuthCacheWithoutForm, drm: DraftRetrievalMethod)(implicit
      request: Request[AnyContent],
      l: LangADT
    ): Future[Result] =
      AccessCodePage
        .form(drm)
        .bindFromRequest()
        .fold(
          errorsForm => badRequest(errorsForm),
          accessCodeForm =>
            accessCodeForm.isContinue match {
              case "true"  => optionAccess(drm, accessCodeForm.accessCode, cache)
              case "false" => optionDownload(drm, accessCodeForm.accessCode, cache)
              case otherwise =>
                Future.failed(
                  new Exception(
                    s"accessCodePost endpoint called, unknown choice selected: $otherwise"
                  )
                )
            }
        )

    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit lang => cache =>
        for {
          (newCache, drm) <- getDraftRetrievalMethod(cache)
          res             <- processSubmittedData(newCache, drm)
        } yield res
    }
  }

  private def processNewForm(cache: AuthCacheWithoutForm, drm: DraftRetrievalMethod)(implicit
    hc: HeaderCarrier
  ): Future[Result] = {
    def processNewFormData(formIdData: FormIdData, drm: DraftRetrievalMethod) =
      formIdData match {
        case FormIdData.WithAccessCode(_, formTemplateId, accessCode) =>
          Redirect(routes.NewFormController.showAccessCode(formTemplateId))
            .flashing(AccessCodePage.key -> accessCode.value)
            .pure[Future]
        case FormIdData.Plain(_, _) =>
          Future.failed(
            new Exception(
              s"processNewForm endpoint for DraftRetrievalMethod: $drm is being seen as OnePerUser on the backend"
            )
          )
      }

    def notFound(formIdData: FormIdData) =
      Future.failed(new NotFoundException(s"Form with id ${formIdData.toFormId} not found for agent."))

    for {
      formIdData <- startFreshForm(cache.formTemplate._id, cache.retrievals, QueryParams.empty)
      _ <- handleForm(formIdData, cache.formTemplate)(notFound(formIdData)) { form =>
             auditService.sendFormCreateEvent(form, cache.retrievals).pure[Future]
           }
      result <- processNewFormData(formIdData, drm)
    } yield result
  }

  def newFormPost(formTemplateId: FormTemplateId): Action[AnyContent] = {
    def processSubmittedData(cache: AuthCacheWithoutForm, drm: DraftRetrievalMethod)(implicit
      request: Request[AnyContent]
    ): Future[Result] = {
      val queryParams: QueryParams = QueryParams.fromRequest(request)
      AccessCodePage.decision
        .bindFromRequest()
        .fold(
          errors =>
            Redirect(
              routes.NewFormController.dashboard(formTemplateId, SuppressErrors.No).url,
              queryParams.toPlayQueryParams
            )
              .pure[Future],
          {
            case AccessCodePage.optionNew => processNewForm(cache, drm)
            case AccessCodePage.optionDownload =>
              Redirect(routes.NewFormController.accessCodeDownload(formTemplateId)).pure[Future]
            case AccessCodePage.optionContinue =>
              Redirect(routes.NewFormController.accessCodeRetrieveForm(formTemplateId)).pure[Future]
            case otherwise =>
              Future.failed(
                new Exception(
                  s"newFormPost endpoint called for '${formTemplateId.value}', unknown choice selected: $otherwise"
                )
              )
          }
        )
    }

    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) { implicit request => lang => cache =>
      for {
        (newCache, drm) <- getDraftRetrievalMethod(cache)
        res             <- processSubmittedData(newCache, drm)
      } yield res
    }
  }

  def continue(formTemplateId: FormTemplateId, submissionRef: SubmissionRef) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit lang => cache =>
        val userId = UserId(cache.retrievals)

        def notFound = {
          logger.error(s"Form not found for formTemplateId $formTemplateId and submissionRef: $submissionRef")
          showAccessCodeList(cache, userId, formTemplateId)
        }

        val accessCode = AccessCode.fromSubmissionRef(submissionRef)
        val formIdData = FormIdData.WithAccessCode(userId, formTemplateId, accessCode)
        handleForm(formIdData, cache.formTemplate)(notFound) { form =>
          redirectContinue[SectionSelectorType.Normal](cache, form, Some(accessCode), request)
        }
    }

  private def getForm(formIdData: FormIdData, formTemplate: FormTemplate)(implicit
    hc: HeaderCarrier
  ): Future[Option[Form]] =
    for {
      maybeForm <- gformConnector.maybeForm(formIdData, formTemplate)
      maybeFormExceptSubmitted = maybeForm.filter(_.status != Submitted)
      maybeEnvelope <- maybeFormExceptSubmitted.fold(Option.empty[Envelope].pure[Future]) { f =>
                         objectStoreService.getEnvelope(f.envelopeId).map(Some(_))
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
    queryParams: QueryParams
  )(implicit hc: HeaderCarrier): Future[FormIdData] = gformBackEnd.newForm(formTemplateId, retrievals, queryParams)

  def handleForm[A](
    formIdData: FormIdData,
    formTemplate: FormTemplate
  )(notFound: => Future[A])(found: Form => Future[A])(implicit hc: HeaderCarrier): Future[A] =
    for {
      maybeForm <- getForm(formIdData, formTemplate)
      result    <- maybeForm.fold(notFound)(found)
    } yield result

  def redirectContinue[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithoutForm,
    form: Form,
    accessCode: Option[AccessCode],
    request: Request[AnyContent]
  )(implicit
    hc: HeaderCarrier,
    l: LangADT,
    messages: Messages
  ): Future[Result] = {
    val cacheWithForm = cache.toAuthCacheWithForm(form, accessCode)
    val formModelOptics = FormModelOptics.mkFormModelOptics[DataOrigin.Mongo, U](
      cacheWithForm.variadicFormData[SectionSelectorType.Normal],
      cacheWithForm
    )
    for {
      cacheUpdated <- maybeUpdateItmpCache(request, cacheWithForm, formModelOptics)
      r <- cache.formTemplate.formKind.fold { classic =>
             fastForwardService.redirectFastForward(cacheUpdated, accessCode, formModelOptics, None, SuppressErrors.Yes)
           } { taskList =>
             val url =
               uk.gov.hmrc.gform.tasklist.routes.TaskListController.landingPage(cache.formTemplate._id, accessCode)
             for {
               _      <- gformBackEnd.updateUserData(cacheUpdated.form, accessCode)
               result <- Redirect(url).pure[Future]
             } yield result
           }
    } yield r
  }

  private def exitPageHandler(
    cache: AuthCacheWithoutForm,
    se: SuppressErrors,
    continue: (AuthCacheWithoutForm, FormTemplate, SuppressErrors) => Future[Result]
  )(implicit
    request: Request[AnyContent],
    l: LangADT
  ): Future[Result] = {
    val formTemplate = cache.formTemplate
    for {
      itmpRetrievals <- if (formTemplate.isSpecimen) {
                          Future.successful(Option.empty[ItmpRetrievals])
                        } else {
                          formTemplate.dataRetrieve.fold(Future.successful(Option.empty[ItmpRetrievals]))(_ =>
                            auth.getItmpRetrievals(request).map(Some(_))
                          )
                        }
      newCache <-
        InitFormEvaluator
          .makeCacheWithDataRetrieve(
            cache,
            formTemplate.authConfig,
            itmpRetrievals,
            formTemplate.dataRetrieve,
            gformConnector,
            ninoInsightsConnector
          )
      initFormEvaluator = InitFormEvaluator(newCache, formTemplate.authConfig, itmpRetrievals)
      res <- {
        val maybeExitPage = formTemplate.exitPages.flatMap { exitPages =>
          exitPages.toList.find { exitPage =>
            initFormEvaluator.evalIncludeIf(exitPage.`if`)
          }
        }

        maybeExitPage.fold(continue(newCache, formTemplate, se)) { exitPage =>
          implicit val sse = (new RealSmartStringEvaluatorFactory(englishMessages)).noForm(
            initFormEvaluator.evalExpr
          )
          Ok(exit_page(cache.formTemplate, exitPage, frontendAppConfig)).pure[Future]
        }
      }
    } yield res
  }

  private def evalItmpExpr(exprs: List[Expr], itmpAuthContexts: List[AuthCtx]) = {
    val leafs = exprs.flatMap(_.leafs())
    leafs.exists(itmpAuthContexts.contains)
  }

  private def hasItmpExpr(exprs: List[Expr]) = {
    val itmpAuthContexts = List(
      AuthCtx(AuthInfo.ItmpAddress),
      AuthCtx(AuthInfo.ItmpName),
      AuthCtx(AuthInfo.ItmpDateOfBirth),
      AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.GivenName)),
      AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.MiddleName)),
      AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.FamilyName))
    )
    evalItmpExpr(exprs, itmpAuthContexts)
  }

  private def hasItmpNameExpr(exprs: List[Expr]) = {
    val itmpAuthContexts = List(
      AuthCtx(AuthInfo.ItmpName),
      AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.GivenName)),
      AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.MiddleName)),
      AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.FamilyName))
    )
    evalItmpExpr(exprs, itmpAuthContexts)
  }

  private def hasItmpDateOfBirthExpr(exprs: List[Expr]) = {
    val itmpAuthContexts = List(
      AuthCtx(AuthInfo.ItmpDateOfBirth)
    )
    evalItmpExpr(exprs, itmpAuthContexts)
  }

  private def hasItmpAddressExpr(exprs: List[Expr]) = {
    val itmpAuthContexts = List(
      AuthCtx(AuthInfo.ItmpAddress)
    )
    evalItmpExpr(exprs, itmpAuthContexts)
  }

  private def maybeUpdateItmpCache(
    request: Request[AnyContent],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): Future[AuthCacheWithForm] = {
    def formHasAuthItmpReferences(): Boolean = {
      val formModel = formModelOptics.formModelRenderPageOptics.formModel

      val allBracketExprs = formModel.brackets.toBrackets.toList.flatMap(_.allExprs(formModel))
      val allCustomExprs = cache.formTemplateContext.formTemplate.formKind.allCustomExprs
      val expressionsOutExprs =
        cache.formTemplateContext.formTemplate.expressionsOutput.fold(List.empty[Expr])(_.lookup.values.toList)
      val allExprs = allBracketExprs ++ allCustomExprs ++ expressionsOutExprs

      hasItmpExpr(allExprs)
    }

    def modifyCacheItmpRetrievals(
      c: AuthCacheWithForm,
      itmpRetrievals: ItmpRetrievals
    ): AuthCacheWithForm = {
      val cacheItmpRetrievals = c.form.thirdPartyData.itmpRetrievals
      if (cacheItmpRetrievals =!= Some(itmpRetrievals)) {

        val formModel = formModelOptics.formModelRenderPageOptics.formModel
        val modelComponentIds = formModel.confirmationPageMap.flatMap { case (sectionNumber, confirmation) =>
          val allBracketExprs =
            formModel.brackets.withSectionNumber(sectionNumber).allExprs(formModel)

          if (cacheItmpRetrievals.flatMap(_.itmpName) != itmpRetrievals.itmpName && hasItmpNameExpr(allBracketExprs)) {
            Some(confirmation.question.id.modelComponentId)
          } else if (
            cacheItmpRetrievals
              .flatMap(_.itmpDateOfBirth) != itmpRetrievals.itmpDateOfBirth && hasItmpDateOfBirthExpr(allBracketExprs)
          ) {
            Some(confirmation.question.id.modelComponentId)
          } else if (
            cacheItmpRetrievals
              .flatMap(_.itmpAddress) != itmpRetrievals.itmpAddress && hasItmpAddressExpr(allBracketExprs)
          ) {
            Some(confirmation.question.id.modelComponentId)
          } else None
        }

        c.modify(_.form.thirdPartyData.itmpRetrievals)
          .using(_ => Some(itmpRetrievals))
          .modify(_.form.formData)
          .using(_ => formModelOptics.clearModelComponentIds(modelComponentIds).pageOpticsData.toFormData)
      } else c
    }

    cache.retrievals match {
      case AuthenticatedRetrievals(_, _, AffinityGroup.Individual, _, Some(_), _, confidenceLevel, _)
          if formHasAuthItmpReferences() && confidenceLevel != ConfidenceLevel.L50 =>
        auth.getItmpRetrievals(request).flatMap { itmpRetrievals =>
          modifyCacheItmpRetrievals(cache, itmpRetrievals).pure[Future]
        }
      case _ => modifyCacheItmpRetrievals(cache, ItmpRetrievals(None, None, None)).pure[Future]
    }
  }
}
