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

import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, EmailAuthDetails, OperationWithForm }
import uk.gov.hmrc.gform.auth.models.OperationWithForm.ViewSaveAcknowledgement
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.GformSessionKeys.{ COMPOSITE_AUTH_DETAILS_SESSION_KEY, EMAIL_AUTH_DETAILS_SESSION_KEY }
import uk.gov.hmrc.gform.controllers.{ AuthCacheWithForm, AuthenticatedRequestActions }
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, Coordinates, EmailAuthConfig, FormTemplate, FormTemplateId, SectionNumber, SectionOrSummary, SuppressErrors }
import uk.gov.hmrc.gform.views.html.hardcoded.pages.{ save_acknowledgement, save_acknowledgement_email_auth, save_with_access_code }
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import cats.implicits._
import uk.gov.hmrc.gform._
import uk.gov.hmrc.gform.auditing.AuditService
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers.processResponseDataFromBody
import uk.gov.hmrc.gform.gform.processor.FormProcessor
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.gform.NoSpecificAction
import uk.gov.hmrc.gform.models.{ FastForward, ProcessData, ProcessDataService, SectionSelectorType }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeExpiryDate, FormIdData, UserData, Validated }
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthConfig.hmrcSimpleModule
import uk.gov.hmrc.gform.views.hardcoded.{ SaveAcknowledgement, SaveWithAccessCode }

import scala.concurrent.{ ExecutionContext, Future }

class SaveAcknowledgementController(
  i18nSupport: I18nSupport,
  frontendAppConfig: FrontendAppConfig,
  auth: AuthenticatedRequestActions,
  messagesControllerComponents: MessagesControllerComponents,
  processDataService: ProcessDataService[Future],
  gformConnector: GformConnector,
  auditService: AuditService,
  confirmationService: ConfirmationService,
  formProcessor: FormProcessor
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def saveAndExitWithEmailAuth(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, None, ViewSaveAcknowledgement) {
      implicit request => implicit lang => cache => _ => _ =>
        val formTemplateContext = cache.formTemplateContext
        val formTemplate = formTemplateContext.formTemplate
        formTemplate.authConfig match {
          case Composite(configs) =>
            val compositeAuthDetails =
              jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
                .get(formTemplateContext)
            val config = AuthConfig
              .getAuthConfig(compositeAuthDetails.getOrElse(hmrcSimpleModule), configs)
            saveAcknowledgementWithEmailAuth(cache, formTemplateId, config)
          case config =>
            saveAcknowledgementWithEmailAuth(cache, formTemplateId, Some(config))
        }
    }

  private def saveAcknowledgementWithEmailAuth(
    cache: AuthCacheWithForm,
    formTemplateId: FormTemplateId,
    config: Option[AuthConfig]
  )(implicit
    request: Request[AnyContent],
    l: LangADT
  ): Future[Result] = {
    val formTemplateContext = cache.formTemplateContext
    val formTemplate = formTemplateContext.formTemplate
    config match {
      case Some(EmailAuthConfig(_, _, _, _)) =>
        val emailAuthDetails: EmailAuthDetails =
          jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
        emailAuthDetails
          .get(formTemplateContext)
          .fold {
            throw new IllegalArgumentException(
              s"Email auth details missing for form template $formTemplateId"
            )
          } { emailAuthDetails =>
            Ok(
              save_acknowledgement_email_auth(
                formTemplate,
                uk.gov.hmrc.gform.models.EmailId(emailAuthDetails.email),
                frontendAppConfig
              )
            )
          }
          .pure[Future]
      case _ =>
        throw new IllegalArgumentException(s"Only forms with 'email' auth config allowed")
    }
  }

  def saveAndExit(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    browserSectionNumber: SectionNumber,
    fastForward: List[FastForward]
  ): Action[AnyContent] =
    auth.authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
      implicit request => implicit l => cache => implicit sse => formModelOptics =>
        val formModel = formModelOptics.formModelVisibilityOptics.formModel
        val sectionNumber: SectionNumber = formModel.visibleSectionNumber(browserSectionNumber)
        processResponseDataFromBody(request, formModelOptics.formModelRenderPageOptics, Some(browserSectionNumber)) {
          _ => variadicFormData => enteredVariadicFormData =>
            {

              def processSaveAndExit(processData: ProcessData): Future[Result] = {

                val purgeConfirmationData: PurgeConfirmationData =
                  confirmationService.purgeConfirmationData(sectionNumber, processData, enteredVariadicFormData)

                formProcessor.validateAndUpdateData(
                  cache,
                  purgeConfirmationData.f(processData),
                  sectionNumber,
                  sectionNumber,
                  maybeAccessCode,
                  fastForward,
                  formModelOptics,
                  purgeConfirmationData.enteredVariadicFormData,
                  false
                ) { _ => _ => maybeSn =>
                  val formTemplate = cache.formTemplate
                  maybeAccessCode match {
                    case Some(accessCode) =>
                      Redirect(
                        gform.routes.SaveAcknowledgementController
                          .saveAndExitWithAccessCode(cache.formTemplateId, accessCode)
                      )
                    case None =>
                      formTemplate.authConfig match {
                        case Composite(configs) =>
                          val compositeAuthDetails =
                            jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
                              .get(cache.formTemplateContext)
                          val config = AuthConfig
                            .getAuthConfig(compositeAuthDetails.getOrElse(hmrcSimpleModule), configs)
                          processSaveAndExitAcknowledgementPage(config, processData, maybeSn)
                        case config =>
                          processSaveAndExitAcknowledgementPage(Some(config), processData, maybeSn)
                      }
                  }

                }
              }

              def processSaveAndExitAcknowledgementPage(
                config: Option[AuthConfig],
                processData: ProcessData,
                maybeSn: SectionOrSummary
              ): Result = {
                val formTemplate = cache.formTemplate
                config match {
                  case Some(EmailAuthConfig(_, _, _, _)) =>
                    Redirect(gform.routes.SaveAcknowledgementController.saveAndExitWithEmailAuth(cache.formTemplateId))
                  case _ =>
                    showAcknowledgementPage(
                      cache.formTemplateId,
                      maybeAccessCode,
                      processData,
                      maybeSn,
                      formTemplate,
                      cache.form.envelopeExpiryDate
                    )
                }
              }

              def showAcknowledgementPage(
                formTemplateId: FormTemplateId,
                maybeAccessCode: Option[AccessCode],
                processData: ProcessData,
                maybeSn: SectionOrSummary,
                formTemplate: FormTemplate,
                envelopeExpiryDate: Option[EnvelopeExpiryDate]
              )(implicit request: Request[AnyContent]) = {
                val call = maybeSn match {
                  case SectionOrSummary.Section(sn) =>
                    val sectionTitle4Ga = formProcessor.getSectionTitle4Ga(processData, sn)
                    if (sn.isTaskList) {
                      uk.gov.hmrc.gform.tasklist.routes.TaskListController.landingPage(formTemplateId, maybeAccessCode)
                    } else {
                      routes.FormController
                        .form(formTemplateId, None, sn, sectionTitle4Ga, SuppressErrors.Yes, List(FastForward.Yes))
                    }
                  case _ =>
                    formTemplate.formKind.fold { _ =>
                      routes.SummaryController.summaryById(formTemplateId, maybeAccessCode, None, Some(true))
                    } { _ =>
                      uk.gov.hmrc.gform.tasklist.routes.TaskListController.landingPage(formTemplateId, maybeAccessCode)
                    }
                }
                val saveAcknowledgement = new SaveAcknowledgement(formTemplate, envelopeExpiryDate)
                Ok(save_acknowledgement(saveAcknowledgement, call, frontendAppConfig, maybeAccessCode))
              }

              for {
                processData <- processDataService
                                 .getProcessData[SectionSelectorType.Normal](
                                   variadicFormData,
                                   cache,
                                   formModelOptics,
                                   gformConnector.getAllTaxPeriods,
                                   NoSpecificAction
                                 )
                res <- processSaveAndExit(processData).map { result =>
                         auditService.formSavedEvent(
                           cache.form,
                           cache.retrievals
                         )
                         result
                       }

              } yield res

            }
        }
    }

  def saveAndExitWithAccessCode(formTemplateId: FormTemplateId, accessCode: AccessCode): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, Some(accessCode), OperationWithForm.EditForm) {
        implicit request => implicit l => cache => _ => _ =>
          val saveWithAccessCode = new SaveWithAccessCode(cache.formTemplate, accessCode)
          Ok(save_with_access_code(saveWithAccessCode, frontendAppConfig)).pure[Future]
      }

  def saveAndExitFromSummary(
    formTemplateId: FormTemplateId,
    maybeAccessCode: Option[AccessCode],
    maybeCoordinates: Option[Coordinates]
  ): Action[AnyContent] =
    auth
      .authAndRetrieveForm[SectionSelectorType.Normal](formTemplateId, maybeAccessCode, OperationWithForm.EditForm) {
        implicit request => implicit l => cache => _ => _ =>
          gformConnector
            .updateUserData(
              FormIdData(cache.retrievals, formTemplateId, maybeAccessCode),
              UserData(
                cache.form.formData,
                Validated,
                cache.form.visitsIndex,
                cache.form.thirdPartyData,
                cache.form.componentIdToFileId
              )
            )
            .flatMap { _ =>
              maybeAccessCode match {
                case Some(accessCode) =>
                  Redirect(
                    gform.routes.SaveAcknowledgementController
                      .saveAndExitWithAccessCode(cache.formTemplateId, accessCode)
                  ).pure[Future]
                case _ =>
                  val config: Option[AuthConfig] =
                    cache.formTemplate.authConfig match {
                      case Composite(configs) =>
                        val compositeAuthDetails =
                          jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
                            .get(cache.formTemplateContext)
                        AuthConfig
                          .getAuthConfig(compositeAuthDetails.getOrElse(hmrcSimpleModule), configs)
                      case config => Some(config)
                    }

                  config match {
                    case Some(c) if c.isEmailAuthConfig =>
                      Redirect(gform.routes.SaveAcknowledgementController.saveAndExitWithEmailAuth(formTemplateId))
                        .pure[Future]
                    case _ =>
                      val call = if (maybeCoordinates.isDefined) {
                        uk.gov.hmrc.gform.tasklist.routes.TaskListController
                          .landingPage(cache.formTemplateId, maybeAccessCode)
                      } else {
                        routes.SummaryController
                          .summaryById(
                            formTemplateId,
                            maybeAccessCode,
                            None,
                            None
                          ) // TODO JoVl why are Coordinates needed here?
                      }
                      val saveAcknowledgement =
                        new SaveAcknowledgement(cache.formTemplate, cache.form.envelopeExpiryDate)
                      Ok(save_acknowledgement(saveAcknowledgement, call, frontendAppConfig, maybeAccessCode))
                        .pure[Future]
                  }
              }
            }
      }
}
