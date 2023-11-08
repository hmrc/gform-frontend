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

import cats.syntax.eq._
import cats.instances.future._
import cats.syntax.applicative._
import com.softwaremill.quicklens._
import org.slf4j.LoggerFactory
import play.api.data
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc._
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, IsAgent, MaterialisedRetrievals, OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.eval.NoFormEvaluation
import uk.gov.hmrc.gform.eval.smartstring.RealSmartStringEvaluatorFactory
import uk.gov.hmrc.gform.gformbackend.GformBackEndAlgebra
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.CookieNames._
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.models.{ AccessCodePage, SectionSelector, SectionSelectorType }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode.toJsonStr
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormIdData, FormModelOptics, QueryParams, Submitted }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.views.html.hardcoded.pages._
import uk.gov.hmrc.gform.views.hardcoded.{ AccessCodeList, AccessCodeStart, ContinueFormPage, DisplayAccessCode }
import uk.gov.hmrc.http.{ HeaderCarrier, NotFoundException }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.{ AuthenticatedRetrievals, ItmpRetrievals }
import uk.gov.hmrc.auth.core.ConfidenceLevel
import uk.gov.hmrc.gform.api.NinoInsightsConnector

case class AccessCodeForm(accessCode: Option[String], accessOption: String)

class NewFormController(
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  fileUploadService: FileUploadService,
  gformConnector: GformConnector,
  fastForwardService: FastForwardService,
  auditService: AuditService,
  recalculation: Recalculation[Future, Throwable],
  messagesControllerComponents: MessagesControllerComponents,
  gformBackEnd: GformBackEndAlgebra[Future],
  ninoInsightsConnector: NinoInsightsConnector[Future],
  englishMessages: Messages
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {
  import i18nSupport._

  private val logger = LoggerFactory.getLogger(getClass)

  implicit val frontendConfig: FrontendAppConfig = frontendAppConfig

  private val noAccessCode = Option.empty[AccessCode]

  private def formTemplateIdCookie(formTemplateId: FormTemplateId) =
    Cookie(formTemplateIdCookieName, formTemplateId.value, secure = true)

  def dashboard(formTemplateId: FormTemplateId) =
    auth.authWithOptReferrerCheckWithoutRetrievingForm(
      formTemplateId,
      OperationWithoutForm.ViewDashboard
    ) { implicit request => implicit lang => cache =>
      val cookie = formTemplateIdCookie(formTemplateId)

      val result =
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals) match {
          case (BySubmissionReference, _) => showAccesCodePage(cache, cache.formTemplate)
          case (drm @ FormAccessCodeForAgents(_), IsAgent()) =>
            exitPageHandler(cache, cache.formTemplate, showAccesCodePage)
          case _ =>
            Redirect(routes.NewFormController.newOrContinue(formTemplateId).url, request.queryString).pure[Future]
        }

      result.map(_.withCookies(cookie))
    }

  def dashboardClean(formTemplateId: FormTemplateId) =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, noAccessCode, OperationWithForm.EditForm) {
      implicit request => _ => cache => _ => _ =>
        val queryParams = QueryParams.fromRequest(request)
        fastForwardService.deleteForm(formTemplateId, cache, queryParams)
    }

  /** To request a new confirmation code when verifying an email, user will have to start whole journey again in new session.
    */
  def dashboardWithNewSession(formTemplateId: FormTemplateId) = Action.async { request =>
    Redirect(routes.NewFormController.dashboard(formTemplateId)).withSession().pure[Future]
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
  private def showAccesCodePage(cache: AuthCacheWithoutForm, formTemplate: FormTemplate)(implicit
    request: Request[AnyContent],
    l: LangADT
  ) = {
    val userId = UserId(cache.retrievals)
    val formTemplateId = cache.formTemplate._id
    val formIdData = FormIdData.Plain(userId, formTemplateId)
    val draftRetrievalMethod = formTemplate.draftRetrievalMethod

    def showAccessCodePage =
      draftRetrievalMethod match {
        case BySubmissionReference => showAccessCodeList(cache, userId, formTemplateId)
        case _ =>
          val form = AccessCodePage.form(draftRetrievalMethod)
          val accessCodeStart = new AccessCodeStart(cache.formTemplate, form)
          Ok(access_code_start(frontendAppConfig, accessCodeStart))
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
                cache.formTemplate.formKind.fold(_ =>
                  fastForwardService
                    .redirectFastForward[SectionSelectorType.Normal](cache, noAccessCode, formModelOptics, None)
                )(_ =>
                  Redirect(
                    uk.gov.hmrc.gform.tasklist.routes.TaskListController
                      .landingPage(cache.formTemplateId, noAccessCode)
                  ).pure[Future]
                )
              case "delete" => fastForwardService.deleteForm(formTemplateId, cache, queryParams)
              case _        => Redirect(routes.NewFormController.newOrContinue(formTemplateId)).pure[Future]
            }
          )
    }

  def newSubmissionReference(formTemplateId: FormTemplateId) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        newForm(formTemplateId, cache, QueryParams.empty)
    }

  private def continue(cache: AuthCacheWithoutForm, formTemplate: FormTemplate)(implicit
    request: Request[AnyContent],
    lang: LangADT
  ): Future[Result] = {
    val queryParams: QueryParams = QueryParams.fromRequest(request)
    for {
      formIdData <- Future.successful(FormIdData.Plain(UserId(cache.retrievals), formTemplate._id))
      res <-
        handleForm(formIdData, formTemplate)(
          newForm(formTemplate._id, cache.copy(formTemplate = formTemplate), queryParams)
        ) { form =>
          for {
            formTemplate <- if (formTemplate._id === form.formTemplateId)
                              formTemplate.pure[Future]
                            else gformConnector.getFormTemplate(form.formTemplateId)

            res <-
              formTemplate.draftRetrievalMethod match {
                case NotPermitted =>
                  fastForwardService.deleteForm(
                    formTemplate._id,
                    cache.toAuthCacheWithForm(form, noAccessCode),
                    queryParams
                  )
                case OnePerUser(ContinueOrDeletePage.Skip) | FormAccessCodeForAgents(ContinueOrDeletePage.Skip) =>
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
          } yield res
        }
    } yield res
  }

  def newOrContinue(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        gformConnector.getFormTemplateContext(formTemplateId).map(_.formTemplate).flatMap { formTemplate =>
          exitPageHandler(cache, formTemplate, continue)
        }
    }

  private def newForm(formTemplateId: FormTemplateId, cache: AuthCacheWithoutForm, queryParams: QueryParams)(implicit
    request: Request[AnyContent],
    l: LangADT
  ) = {

    def notFound(formIdData: FormIdData) =
      Future.failed(new NotFoundException(s"Form with id ${formIdData.toFormId} not found."))

    for {
      formIdData <- startFreshForm(formTemplateId, cache.retrievals, queryParams)
      res <- handleForm(formIdData, cache.formTemplate)(notFound(formIdData)) { form =>
               auditService.sendFormCreateEvent(form, cache.retrievals)
               redirectContinue[SectionSelectorType.Normal](cache, form, formIdData.maybeAccessCode, request)
             }
    } yield res
  }

  def newFormPost(formTemplateId: FormTemplateId): Action[AnyContent] = {
    def badRequest(formTemplate: FormTemplate, errors: play.api.data.Form[AccessCodeForm])(implicit
      request: Request[AnyContent],
      lang: LangADT
    ) = {
      val accessCodeStart = new AccessCodeStart(formTemplate, errors)
      BadRequest(access_code_start(frontendAppConfig, accessCodeStart))
    }

    def notFound(formTemplate: FormTemplate)(implicit request: Request[AnyContent], lang: LangADT) =
      badRequest(
        formTemplate,
        AccessCodePage
          .form(formTemplate.draftRetrievalMethod)
          .bindFromRequest()
          .withError(AccessCodePage.key, "error.notfound")
      )

    def noAccessCodeProvided = Future.failed[Result](new Exception(s"AccessCode not provided, cannot continue."))

    def optionAccess(accessCodeForm: AccessCodeForm, cache: AuthCacheWithoutForm)(implicit
      hc: HeaderCarrier,
      request: Request[AnyContent],
      lang: LangADT
    ) = {
      val maybeAccessCode: Option[AccessCode] = accessCodeForm.accessCode.map(a => AccessCode(a))
      maybeAccessCode.fold(noAccessCodeProvided) { accessCode =>
        val formIdData = FormIdData.WithAccessCode(UserId(cache.retrievals), formTemplateId, accessCode)
        handleForm(formIdData, cache.formTemplate)(notFound(cache.formTemplate).pure[Future]) { form =>
          redirectContinue[SectionSelectorType.Normal](cache, form, maybeAccessCode, request)
        }
      }
    }

    def processNewFormData(formIdData: FormIdData, drm: DraftRetrievalMethod) =
      formIdData match {
        case FormIdData.WithAccessCode(_, formTemplateId, accessCode) =>
          Redirect(routes.NewFormController.showAccessCode(formTemplateId))
            .flashing(AccessCodePage.key -> accessCode.value)
            .pure[Future]
        case FormIdData.Plain(_, _) =>
          Future.failed(
            new Exception(
              s"newFormPost endpoind for DraftRetrievalMethod: $drm is being seen as OnePerUser on the backend"
            )
          )
      }

    def processSubmittedData(cache: AuthCacheWithoutForm, drm: DraftRetrievalMethod)(implicit
      request: Request[AnyContent],
      l: LangADT
    ): Future[Result] =
      AccessCodePage
        .form(drm)
        .bindFromRequest()
        .fold(
          (hasErrors: data.Form[AccessCodeForm]) => Future.successful(badRequest(cache.formTemplate, hasErrors)),
          accessCodeForm =>
            accessCodeForm.accessOption match {
              case AccessCodePage.optionNew =>
                def notFound(formIdData: FormIdData) =
                  Future.failed(new NotFoundException(s"Form with id ${formIdData.toFormId} not found for agent."))
                for {
                  formIdData <- startFreshForm(formTemplateId, cache.retrievals, QueryParams.empty)
                  _ <- handleForm(formIdData, cache.formTemplate)(notFound(formIdData)) { form =>
                         auditService.sendFormCreateEvent(form, cache.retrievals).pure[Future]
                       }
                  result <- processNewFormData(formIdData, drm)
                } yield result
              case AccessCodePage.optionAccess =>
                optionAccess(accessCodeForm, cache)
              case other => throw new Exception(s"Invalid AccessCodePage, got $other")
            }
        )

    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit lang => cache =>
        for {
          noFormEvaluation <- noFormEval(cache)
          newCache = noFormEvaluation.toCache
          res <- (newCache.formTemplate.draftRetrievalMethod, newCache.retrievals) match {
                   case (BySubmissionReference, _)                    => processSubmittedData(newCache, BySubmissionReference)
                   case (drm @ FormAccessCodeForAgents(_), IsAgent()) => processSubmittedData(newCache, drm)
                   case otherwise =>
                     Future.failed(
                       new Exception(
                         s"newFormPost endpoind called, but draftRetrievalMethod is not allowed for a user or formTemplate: $otherwise"
                       )
                     )
                 }
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
                         fileUploadService.getEnvelope(f.envelopeId)(formTemplate.isObjectStore).map(Some(_))
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
  )(implicit hc: HeaderCarrier): Future[FormIdData] =
    for {
      newFormData <-
        gformConnector
          .newForm(formTemplateId, UserId(retrievals), AffinityGroupUtil.fromRetrievals(retrievals), queryParams)
    } yield newFormData

  private def handleForm[A](
    formIdData: FormIdData,
    formTemplate: FormTemplate
  )(notFound: => Future[A])(found: Form => Future[A])(implicit hc: HeaderCarrier): Future[A] =
    for {
      maybeForm <- getForm(formIdData, formTemplate)
      result    <- maybeForm.fold(notFound)(found)
    } yield result

  private def redirectContinue[U <: SectionSelectorType: SectionSelector](
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

    for {
      formModelOptics <- FormModelOptics.mkFormModelOptics[DataOrigin.Mongo, Future, U](
                           cacheWithForm.variadicFormData[SectionSelectorType.Normal],
                           cacheWithForm,
                           recalculation
                         )
      cacheUpdated <- maybeUpdateItmpCache(request, cacheWithForm, formModelOptics)
      r <- cache.formTemplate.formKind.fold { classic =>
             fastForwardService.redirectFastForward(cacheUpdated, accessCode, formModelOptics, None)
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

  private def noFormEval(
    cache: AuthCacheWithoutForm
  )(implicit
    request: Request[AnyContent]
  ): Future[NoFormEvaluation] = {
    val formTemplate = cache.formTemplate

    for {
      itmpRetrievals <- formTemplate.dataRetrieve.fold(Future.successful(Option.empty[ItmpRetrievals]))(_ =>
                          auth.getItmpRetrievals(request).map(Some(_))
                        )
      noFormEvaluation <-
        new NoFormEvaluation(
          cache,
          formTemplate.authConfig,
          itmpRetrievals,
          gformConnector,
          ninoInsightsConnector
        )
          .withDataRetrieve(formTemplate.dataRetrieve)
    } yield noFormEvaluation
  }

  private def exitPageHandler(
    cache: AuthCacheWithoutForm,
    formTemplate: FormTemplate,
    continue: (AuthCacheWithoutForm, FormTemplate) => Future[Result]
  )(implicit
    request: Request[AnyContent],
    l: LangADT
  ): Future[Result] =
    for {
      noFormEvaluation <- noFormEval(cache)
      res <- {
        val maybeExitPage = formTemplate.exitPages.flatMap { exitPages =>
          exitPages.toList.find { exitPage =>
            noFormEvaluation.evalIncludeIf(exitPage.`if`)
          }
        }

        maybeExitPage.fold(continue(noFormEvaluation.toCache, formTemplate)) { exitPage =>
          implicit val sse = (new RealSmartStringEvaluatorFactory(englishMessages)).noForm
          Ok(exit_page(cache.formTemplate, exitPage, frontendAppConfig)).pure[Future]
        }
      }
    } yield res

  private def maybeUpdateItmpCache(
    request: Request[AnyContent],
    cache: AuthCacheWithForm,
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): Future[AuthCacheWithForm] = {
    def formHasAuthItmpReferences(): Boolean = {
      val formModel = formModelOptics.formModelRenderPageOptics.formModel

      val allBracketExprs = formModel.brackets.toBracketsPlains.toList.flatMap(_.allExprs(formModel))
      val allCustomExprs = cache.formTemplateContext.formTemplate.formKind.allCustomExprs
      val expressionsOutExprs =
        cache.formTemplateContext.formTemplate.expressionsOutput.fold(List.empty[Expr])(_.lookup.map(_._2).toList)
      val allExprs = allBracketExprs ++ allCustomExprs ++ expressionsOutExprs

      allExprs.contains(AuthCtx(AuthInfo.ItmpAddress)) ||
      allExprs.contains(AuthCtx(AuthInfo.ItmpName)) ||
      allExprs.contains(AuthCtx(AuthInfo.ItmpDateOfBirth)) ||
      allExprs.contains(AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.GivenName))) ||
      allExprs.contains(AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.MiddleName))) ||
      allExprs.contains(AuthCtx(AuthInfo.ItmpNameLens(ItmpNameFocus.FamilyName)))
    }

    def modifyCacheItmpRetrievals(c: AuthCacheWithForm, itmpRetrievals: ItmpRetrievals): AuthCacheWithForm =
      c.modify(_.form.thirdPartyData.itmpRetrievals).using(_ => Some(itmpRetrievals))

    cache.retrievals match {
      case AuthenticatedRetrievals(_, _, AffinityGroup.Individual, _, Some(_), _, confidenceLevel)
          if formHasAuthItmpReferences() && confidenceLevel != ConfidenceLevel.L50 =>
        auth.getItmpRetrievals(request).map { itmpRetrievals =>
          modifyCacheItmpRetrievals(cache, itmpRetrievals)
        }
      case _ => modifyCacheItmpRetrievals(cache, ItmpRetrievals(None, None, None)).pure[Future]
    }
  }
}
