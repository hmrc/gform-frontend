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
import uk.gov.hmrc.gform.auth.models.{ IsAgent, MaterialisedRetrievals, OperationWithForm, OperationWithoutForm }
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers._
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.controllers.helpers._
import uk.gov.hmrc.gform.fileupload.{ Envelope, FileUploadService }
import uk.gov.hmrc.gform.gform.handlers.{ FormControllerRequestHandler, FormHandlerResult }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Data
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.models.ExpandUtils._
import uk.gov.hmrc.gform.models.gform.{ ForceReload, FormValidationOutcome, NoSpecificAction, ObligationsAction }
import uk.gov.hmrc.gform.models.{ AccessCodePage, ProcessData, ProcessDataService }
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

class NewFormController(
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions,
  fileUploadService: FileUploadService,
  gformConnector: GformConnector,
  fastForwardService: FastForwardService
) extends FrontendController {

  import i18nSupport._

  implicit val frontendConfig: FrontendAppConfig = frontendAppConfig

  private val noAccessCode = Option.empty[AccessCode]

  def dashboard(formTemplateId: FormTemplateId) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.ViewDashboard) {
      implicit request => implicit lang => cache =>
        import cache._

        val route = (formTemplate.draftRetrievalMethod, retrievals) match {
          case (Some(BySubmissionReference), _) =>
            Ok(access_code_start(formTemplate, AccessCodePage.form(BySubmissionReference), frontendAppConfig))
          case (Some(FormAccessCodeForAgents), IsAgent()) =>
            Ok(access_code_start(formTemplate, AccessCodePage.form(FormAccessCodeForAgents), frontendAppConfig))
          case _ => Redirect(routes.NewFormController.newForm(formTemplateId))
        }

        route.pure[Future]
    }

  def showAccessCode(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.ShowAccessCode) {
      implicit request => implicit l => cache =>
        Future.successful {
          val accessCode = request.flash.get(AccessCodePage.key)
          accessCode match {
            case Some(code) =>
              Ok(start_new_form(cache.formTemplate, AccessCodePage(code), frontendAppConfig))
            case None => Redirect(routes.NewFormController.dashboard(formTemplateId))
          }
        }
    }

  private val choice: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "decision" -> play.api.data.Forms.nonEmptyText
    ))

  def decision(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm(formTemplateId, noAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache =>
        choice.bindFromRequest
          .fold(
            _ =>
              BadRequest(
                continue_form_page(
                  cache.formTemplate,
                  choice.bindFromRequest().withError("decision", "error.required"),
                  noAccessCode,
                  frontendAppConfig)).pure[Future], {
              case "continue" =>
                val dataRaw = FormDataHelpers.formDataMap(cache.form.formData) + cache.form.visitsIndex.toVisitsTuple
                fastForwardService.redirectWithRecalculation(cache, dataRaw, noAccessCode, ForceReload)
              case "delete" =>
                fastForwardService.deleteForm(cache)
              case _ => Redirect(routes.NewFormController.newForm(formTemplateId)).pure[Future]
            }
          )
    }

  def newForm(formTemplateId: FormTemplateId) =
    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit l => cache =>
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
                                 fastForwardService.redirectFromEmpty(cache, form, noAccessCode, NoSpecificAction)
                               case None => Future.failed(new NotFoundException(s"Form with id $formId not found."))
                             }
                     } yield res
                   }
        } yield result
    }

  def newFormPost(formTemplateId: FormTemplateId): Action[AnyContent] = {
    def badRequest(formTemplate: FormTemplate, errors: play.api.data.Form[AccessCodeForm])(
      implicit request: Request[AnyContent],
      lang: LangADT) =
      BadRequest(access_code_start(formTemplate, errors, frontendAppConfig))

    def notFound(formTemplate: FormTemplate)(implicit request: Request[AnyContent], lang: LangADT) =
      badRequest(
        formTemplate,
        AccessCodePage
          .form(formTemplate.draftRetrievalMethod.getOrElse(OnePerUser))
          .bindFromRequest()
          .withError(AccessCodePage.key, "error.notfound")
      )

    def optionAccess(
      accessCodeForm: AccessCodeForm,
      cache: AuthCacheWithoutForm)(implicit hc: HeaderCarrier, request: Request[AnyContent], lang: LangADT) = {
      val maybeAccessCode: Option[AccessCode] = accessCodeForm.accessCode.map(a => AccessCode(a))
      maybeAccessCode.fold(BadRequest("hahaha").pure[Future]) { accessCode =>
        for {
          maybeForm <- getForm(FormId.withAccessCode(UserId(cache.retrievals), formTemplateId, accessCode))
          res <- maybeForm
                  .map(fastForwardService.redirectFromEmpty(cache, _, maybeAccessCode, NoSpecificAction))
                  .getOrElse(notFound(cache.formTemplate).pure[Future])
        } yield res
      }

    }

    def processNewFormData(newFormData: NewFormData, drm: DraftRetrievalMethod)(implicit request: Request[AnyContent]) =
      newFormData.formAccess match {
        case FormAccess.ByAccessCode(accessCode) =>
          Redirect(routes.NewFormController.showAccessCode(formTemplateId))
            .flashing(AccessCodePage.key -> accessCode.value)
            .pure[Future]
        case FormAccess.Direct =>
          Future.failed(
            new Exception(
              s"newFormPost endpoind for DraftRetrievalMethod: $drm is being seen as OnePerUser on the backend"))
      }

    def processSubmittedData(cache: AuthCacheWithoutForm, drm: DraftRetrievalMethod)(
      implicit request: Request[AnyContent],
      l: LangADT): Future[Result] =
      AccessCodePage
        .form(drm)
        .bindFromRequest
        .fold(
          (hasErrors: data.Form[AccessCodeForm]) => Future.successful(badRequest(cache.formTemplate, hasErrors)),
          accessCodeForm => {
            accessCodeForm.accessOption match {
              case AccessCodePage.optionNew =>
                for {
                  newFormData <- startFreshForm(formTemplateId, cache.retrievals)
                  result      <- processNewFormData(newFormData, drm)
                } yield result
              case AccessCodePage.optionAccess => optionAccess(accessCodeForm, cache)
            }
          }
        )

    auth.authWithoutRetrievingForm(formTemplateId, OperationWithoutForm.EditForm) {
      implicit request => implicit lang => cache =>
        (cache.formTemplate.draftRetrievalMethod, cache.retrievals) match {
          case (Some(BySubmissionReference), _) =>
            processSubmittedData(cache, BySubmissionReference)
          case (Some(FormAccessCodeForAgents), IsAgent()) =>
            processSubmittedData(cache, FormAccessCodeForAgents)
          case otherwise =>
            Future.failed(
              new Exception(
                s"newFormPost endpoind called, but draftRetrievalMethod is not allowed for a user or formTemplate: $otherwise")
            )
        }
    }
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

  private def startFreshForm(formTemplateId: FormTemplateId, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier): Future[NewFormData] =
    for {
      newFormData <- gformConnector
                      .newForm(formTemplateId, UserId(retrievals), AffinityGroupUtil.fromRetrievals(retrievals))
    } yield newFormData

  private def getOrStartForm(
    formTemplateId: FormTemplateId,
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode])(implicit hc: HeaderCarrier): Future[(FormId, Boolean)] =
    for {
      maybeFormExceptSubmitted <- getForm(FormId.direct(UserId(retrievals), formTemplateId))
      formId <- maybeFormExceptSubmitted.fold(startFreshForm(formTemplateId, retrievals).map(_.formId))(
                 _._id.pure[Future])
    } yield (formId, maybeFormExceptSubmitted.isDefined)
}
