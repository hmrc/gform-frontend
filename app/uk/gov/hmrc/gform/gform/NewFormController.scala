/*
 * Copyright 2022 HM Revenue & Customs
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
import org.slf4j.LoggerFactory
import play.api.data
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc._
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, IsAgent, MaterialisedRetrievals, OperationWithForm, OperationWithoutForm }
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
  messagesControllerComponents: MessagesControllerComponents
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
          case (BySubmissionReference, _)                    => showAccesCodePage(cache, BySubmissionReference)
          case (drm @ FormAccessCodeForAgents(_), IsAgent()) => showAccesCodePage(cache, drm)
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
    val formTemplateWithRedirects = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateWithRedirects.formTemplate
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
  private def showAccesCodePage(cache: AuthCacheWithoutForm, draftRetrievalMethod: DraftRetrievalMethod)(implicit
    request: Request[AnyContent],
    l: LangADT
  ) = {
    val userId = UserId(cache.retrievals)
    val formTemplateId = cache.formTemplate._id
    val formIdData = FormIdData.Plain(userId, formTemplateId)

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
        choice.bindFromRequest
          .fold(
            errorForm => {

              val continueFormPage = new ContinueFormPage(cache.formTemplate, errorForm)

              BadRequest(continue_form_page(frontendAppConfig, continueFormPage)).pure[Future]
            },
            {
              case "continue" =>
                fastForwardService.redirectFastForward[SectionSelectorType.Normal](cache, noAccessCode, formModelOptics)
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

  def newOrContinue(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
        val queryParams: QueryParams = QueryParams.fromRequest(request)

        for {
          formTemplate <- getLatestFormTemplate(formTemplateId, cache.retrievals)
          formIdData   <- Future.successful(FormIdData.Plain(UserId(cache.retrievals), formTemplate._id))
          res <-
            handleForm(formIdData, formTemplate)(
              newForm(formTemplate._id, cache.copy(formTemplate = formTemplate), queryParams)
            ) { form =>
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
                    noAccessCode
                  )
                case _ =>
                  auditService.sendFormResumeEvent(form, cache.retrievals)
                  val continueFormPage = new ContinueFormPage(formTemplate, choice)
                  Ok(continue_form_page(frontendAppConfig, continueFormPage)).pure[Future]
              }
            }
        } yield res
    }

  private def getLatestFormTemplate(
    formTemplateId: FormTemplateId,
    retrievals: MaterialisedRetrievals
  )(implicit hc: HeaderCarrier): Future[FormTemplate] =
    for {
      formTemplate <- gformConnector.getFormTemplate(formTemplateId)
      formIdData   <- Future.successful(FormIdData.Plain(UserId(retrievals), formTemplate.formTemplate._id))
      maybeForm    <- gformConnector.maybeForm(formIdData, formTemplate.formTemplate)
      res <- maybeForm match {
               case Some(_) => Future.successful(formTemplate.formTemplate)
               case None =>
                 formTemplate.redirect match {
                   case Some(ft) => getLatestFormTemplate(ft, retrievals)
                   case None     => Future.successful(formTemplate.formTemplate)
                 }
             }
    } yield res

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
               redirectContinue[SectionSelectorType.Normal](cache, form, formIdData.maybeAccessCode)
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
          redirectContinue[SectionSelectorType.Normal](cache, form, maybeAccessCode)
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
        .bindFromRequest
        .fold(
          (hasErrors: data.Form[AccessCodeForm]) => Future.successful(badRequest(cache.formTemplate, hasErrors)),
          accessCodeForm =>
            accessCodeForm.accessOption match {
              case AccessCodePage.optionNew =>
                for {
                  formIdData <- startFreshForm(formTemplateId, cache.retrievals, QueryParams.empty)
                  result     <- processNewFormData(formIdData, drm)
                } yield result
              case AccessCodePage.optionAccess =>
                optionAccess(accessCodeForm, cache)
            }
        )

    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit lang => cache =>
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals) match {
          case (BySubmissionReference, _)                    => processSubmittedData(cache, BySubmissionReference)
          case (drm @ FormAccessCodeForAgents(_), IsAgent()) => processSubmittedData(cache, drm)
          case otherwise =>
            Future.failed(
              new Exception(
                s"newFormPost endpoind called, but draftRetrievalMethod is not allowed for a user or formTemplate: $otherwise"
              )
            )
        }
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
          redirectContinue[SectionSelectorType.Normal](cache, form, Some(accessCode))
        }
    }

  private def getForm(formIdData: FormIdData, formTemplate: FormTemplate)(implicit
    hc: HeaderCarrier
  ): Future[Option[Form]] =
    for {
      maybeForm <- gformConnector.maybeForm(formIdData, formTemplate)
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

  def redirectContinue[U <: SectionSelectorType: SectionSelector](
    cache: AuthCacheWithoutForm,
    form: Form,
    accessCode: Option[AccessCode]
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
      res <- fastForwardService.redirectFastForward(cacheWithForm, accessCode, formModelOptics)
    } yield res

  }

}
