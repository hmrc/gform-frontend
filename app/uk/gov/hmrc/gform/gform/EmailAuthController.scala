/*
 * Copyright 2021 HM Revenue & Customs
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

import cats.MonadError
import cats.implicits._
import org.slf4j.{ Logger, LoggerFactory }
import org.typelevel.ci.CIString
import play.api.data
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.{ CompositeAuthDetails, EmailAuthDetails, InvalidEmail, ValidEmail }
import uk.gov.hmrc.gform.commons.MarkDownUtil
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.GformSessionKeys.{ COMPOSITE_AUTH_DETAILS_SESSION_KEY, EMAIL_AUTH_DETAILS_SESSION_KEY }
import uk.gov.hmrc.gform.controllers.NonAuthenticatedRequestActions
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailConfirmationCode }
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.toJsonStr
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, EmailAuthConfig, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import org.typelevel.ci._
import uk.gov.hmrc.gform.typeclasses.Rnd

import scala.concurrent.{ ExecutionContext, Future }

class EmailAuthController(
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  nonAutheticatedRequestActions: NonAuthenticatedRequestActions,
  frontendAppConfig: FrontendAppConfig,
  gformConnector: GformConnector
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  private val logger: Logger = LoggerFactory.getLogger(getClass)

  import i18nSupport._

  private val emailForm: data.Form[String] = play.api.data.Form(
    play.api.data.Forms.single(
      "email" -> play.api.data.Forms.nonEmptyText
    )
  )

  private val confirmCodeForm: data.Form[(String, String)] = play.api.data.Form(
    play.api.data.Forms.tuple(
      "email" -> play.api.data.Forms.nonEmptyText,
      "code"  -> play.api.data.Forms.nonEmptyText
    )
  )

  def emailIdForm(formTemplateId: FormTemplateId, continue: String): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      val emailAuthDetails: EmailAuthDetails =
        jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
      val (pageErrors, maybeEmailFieldError, maybeEmailFieldValue) = emailAuthDetails.get(formTemplateId) match {
        case Some(InvalidEmail(EmailId(value), message)) =>
          (
            Errors(
              new components.govukErrorSummary()(
                ErrorSummary(
                  errorList = List(
                    ErrorLink(
                      href = Some("#email"),
                      content = content.Text(
                        request.messages
                          .messages(message, request.messages.messages("emailAuth.emailAddress"))
                      )
                    )
                  ),
                  title = content.Text(request.messages.messages("error.summary.heading"))
                )
              )
            ),
            Some(
              ErrorMessage(
                content = content.Text(
                  request.messages
                    .messages(message, request.messages.messages("emailAuth.emailAddress"))
                )
              )
            ),
            Some(value)
          )
        case _ => (NoErrors, None, None)
      }

      formTemplate.authConfig match {
        case EmailAuthConfig(_, emailUseInfo, _, _) =>
          Ok(
            html.auth.enter_email(
              formTemplate,
              frontendAppConfig,
              uk.gov.hmrc.gform.gform.routes.EmailAuthController.sendEmail(formTemplateId, continue),
              maybeEmailFieldValue.map(_.toString),
              emailUseInfo.map(MarkDownUtil.markDownParser _),
              pageErrors,
              maybeEmailFieldError
            )
          ).pure[Future]
        case Composite(configs) =>
          val compositeAuthDetails: CompositeAuthDetails =
            jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)

          compositeAuthDetails.get(formTemplate._id) match {
            case Some(selection) =>
              AuthConfig
                .getAuthConfig(selection, configs) match {
                case Some(EmailAuthConfig(_, emailUseInfo, _, _)) =>
                  Ok(
                    html.auth.enter_email(
                      formTemplate,
                      frontendAppConfig,
                      uk.gov.hmrc.gform.gform.routes.EmailAuthController.sendEmail(formTemplateId, continue),
                      maybeEmailFieldValue.map(_.toString),
                      emailUseInfo.map(MarkDownUtil.markDownParser _),
                      pageErrors,
                      maybeEmailFieldError
                    )
                  ).pure[Future]
                case _ =>
                  notSetEmailResult(formTemplateId)
              }
            case None =>
              notSetEmailResult(formTemplateId)
          }
        case _ =>
          notSetEmailResult(formTemplateId)
      }
    }

  def sendEmail(formTemplateId: FormTemplateId, continue: String): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit l =>
      val emailAuthDetails: EmailAuthDetails =
        jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
      emailForm
        .bindFromRequest()
        .fold(
          _ =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.EmailAuthController.emailIdForm(formTemplateId, continue)
            ).addingToSession(
              EMAIL_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
                emailAuthDetails + (formTemplateId -> InvalidEmail(EmailId(CIString.empty), "generic.error.required"))
              )
            ).pure[Future],
          { email =>
            val emailId = EmailId(CIString(email))
            val formTemplate = request.attrs(FormTemplateKey)
            EmailAddress.isValid(emailId.value.toString) match {
              case true =>
                sendEmailWithConfirmationCode(formTemplate, emailId).map { emailAndCode =>
                  Redirect(
                    uk.gov.hmrc.gform.gform.routes.EmailAuthController.confirmCodeForm(formTemplateId, None, continue)
                  ).addingToSession(
                    EMAIL_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
                      emailAuthDetails + (formTemplateId -> ValidEmail(emailAndCode))
                    )
                  )
                }
              case false =>
                Redirect(
                  uk.gov.hmrc.gform.gform.routes.EmailAuthController.emailIdForm(formTemplateId, continue)
                ).addingToSession(
                  EMAIL_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
                    emailAuthDetails + (formTemplateId -> InvalidEmail(emailId, "generic.error.invalid"))
                  )
                ).pure[Future]
            }

          }
        )

    }

  def confirmCodeForm(formTemplateId: FormTemplateId, error: Option[Boolean], continue: String): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      val emailAuthDetails: EmailAuthDetails =
        jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)

      val (pageErrors, maybeCodeFieldError) = error match {
        case Some(true) =>
          (
            Errors(
              new components.govukErrorSummary()(
                ErrorSummary(
                  errorList = List(
                    ErrorLink(
                      href = Some("#code"),
                      content = content.Text(request.messages.messages("emailAuth.confirmCodeError"))
                    )
                  ),
                  title = content.Text(request.messages.messages("error.summary.heading"))
                )
              )
            ),
            Some(
              ErrorMessage(
                content = content.Text(request.messages.messages("emailAuth.confirmCodeError"))
              )
            )
          )
        case _ => (NoErrors, None)
      }

      (emailAuthDetails.get(formTemplateId), formTemplate.authConfig) match {
        case (Some(emailAuthData), EmailAuthConfig(_, _, emailCodeHelp, _)) =>
          Ok(
            html.auth.confirm_code(
              formTemplate,
              frontendAppConfig,
              EmailId(emailAuthData.email),
              emailCodeHelp.map(_.value),
              uk.gov.hmrc.gform.gform.routes.EmailAuthController
                .confirmCode(formTemplateId, continue),
              continue,
              pageErrors,
              maybeCodeFieldError
            )
          ).pure[Future]

        case (Some(emailAuthData), Composite(configs)) =>
          val compositeAuthDetails =
            jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
              .get(formTemplate._id)

          val config = AuthConfig
            .getAuthConfig(compositeAuthDetails.get, configs)

          config match {
            case Some(EmailAuthConfig(_, _, emailCodeHelp, _)) =>
              Ok(
                html.auth.confirm_code(
                  formTemplate,
                  frontendAppConfig,
                  EmailId(emailAuthData.email),
                  emailCodeHelp.map(_.value),
                  uk.gov.hmrc.gform.gform.routes.EmailAuthController
                    .confirmCode(formTemplateId, continue),
                  continue,
                  pageErrors,
                  maybeCodeFieldError
                )
              ).pure[Future]

            case _ =>
              logger.warn(s"Could not find emailAuthDetails in session for formTemplate $formTemplateId")
              BadRequest(s"Unable to confirm code for formTemplate $formTemplateId").pure[Future]

          }

        case _ =>
          logger.warn(s"Could not find emailAuthDetails in session for formTemplate $formTemplateId")
          BadRequest(s"Unable to confirm code for formTemplate $formTemplateId").pure[Future]
      }
    }

  def confirmCode(
    formTemplateId: FormTemplateId,
    continue: String
  ): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => _ =>
      val formTemplate = request.attrs(FormTemplateKey)

      confirmCodeForm
        .bindFromRequest()
        .fold(
          _ =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.EmailAuthController
                .confirmCodeForm(formTemplateId, Some(true), continue)
            ).pure[Future],
          { case (email: String, code: String) =>
            val emailAuthDetails: EmailAuthDetails =
              jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
            emailAuthDetails
              .checkCodeAndConfirm(formTemplateId, EmailAndCode(CIString(email), EmailConfirmationCode(CIString(code))))
              .fold {
                Redirect(
                  uk.gov.hmrc.gform.gform.routes.EmailAuthController
                    .confirmCodeForm(formTemplateId, Some(true), continue)
                )
              } { confirmedEmailAuthDetails =>
                val confirmedEmailAuthDetailsStr = toJsonStr(confirmedEmailAuthDetails)

                formTemplate.authConfig match {
                  case EmailAuthConfig(_, _, _, Some(_)) =>
                    Redirect(
                      uk.gov.hmrc.gform.gform.routes.EmailAuthController.emailConfirmedForm(formTemplateId, continue)
                    ).addingToSession(
                      EMAIL_AUTH_DETAILS_SESSION_KEY -> confirmedEmailAuthDetailsStr
                    )
                  case _ =>
                    Redirect(continue).addingToSession(
                      EMAIL_AUTH_DETAILS_SESSION_KEY -> confirmedEmailAuthDetailsStr
                    )
                }

              }
              .pure[Future]
          }
        )

    }

  def emailConfirmedForm(formTemplateId: FormTemplateId, continue: String): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      formTemplate.authConfig match {
        case EmailAuthConfig(_, _, _, Some(emailConfirmation)) =>
          Ok(
            html.auth.email_confirmation(
              formTemplate,
              frontendAppConfig,
              uk.gov.hmrc.gform.gform.routes.EmailAuthController
                .emailConfirmedContinue(continue),
              MarkDownUtil.markDownParser(emailConfirmation)
            )
          ).pure[Future]
        case _ =>
          logger.warn(s"AuthModule for formTemplate $formTemplateId doesn't have email confirmation")
          BadRequest(s"Unable to provide email confirmation for formTemplate $formTemplateId").pure[Future]
      }
    }

  def emailConfirmedContinue(
    continue: String
  ): Action[AnyContent] =
    nonAutheticatedRequestActions.async { _ => _ =>
      Redirect(continue)
        .pure[Future]
    }

  private def sendEmailWithConfirmationCode[D <: DataOrigin](
    formTemplate: FormTemplate,
    emailId: EmailId
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    me: MonadError[Future, Throwable],
    l: LangADT
  ): Future[EmailAndCode] = {
    val isStaticCodeEmail = frontendAppConfig.emailAuthStaticCodeEmails.fold(false) {
      _.exists(_ === ci"${emailId.value.toString}")
    }

    implicit val rnd = if (isStaticCodeEmail) {
      Rnd.ConstantInt
    } else {
      Rnd.RandomInt
    }

    val emailAndCode = EmailAndCode.emailVerificationCode(emailId.value.toString)

    formTemplate.authConfig match {
      case emailAuthConfig: EmailAuthConfig =>
        gformConnector
          .sendEmail(
            ConfirmationCodeWithEmailService(
              NotifierEmailAddress(emailId.value.toString),
              emailAndCode.code,
              emailAuthConfig.service,
              l
            )
          )
          .map(_ => emailAndCode)

      case composite: Composite =>
        val compositeAuthDetails =
          jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
            .get(formTemplate._id)

        val config = AuthConfig
          .getAuthConfig(compositeAuthDetails.get, composite.configs)

        config match {
          case Some(EmailAuthConfig(service, _, _, _)) =>
            gformConnector
              .sendEmail(
                ConfirmationCodeWithEmailService(
                  NotifierEmailAddress(emailId.value.toString),
                  emailAndCode.code,
                  service,
                  l
                )
              )
              .map(_ => emailAndCode)

          case _ =>
            me.raiseError(new IllegalArgumentException(s"Unsupported auth config ${formTemplate.authConfig}"))
        }

      case _ => me.raiseError(new IllegalArgumentException(s"Unsupported auth config ${formTemplate.authConfig}"))
    }
  }

  private def notSetEmailResult(formTemplateId: FormTemplateId): Future[Result] = {
    logger.warn(s"AuthModule for formTemplate $formTemplateId is not set to email")
    BadRequest(s"Unable to accept Email for formTemplate $formTemplateId").pure[Future]
  }
}
