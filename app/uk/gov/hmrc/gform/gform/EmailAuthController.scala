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

import cats.MonadError
import cats.implicits._
import org.slf4j.{ Logger, LoggerFactory }
import org.typelevel.ci.CIString
import play.api.data
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents, Request, Result }
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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, Composite, EmailAuthConfig, FormTemplateContext, FormTemplateId, HasEmailConfirmation }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailConfirmationCode }
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.toJsonStr
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.validation.{ EmailAddress, ValidationValues }
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
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      val emailAuthDetails: EmailAuthDetails =
        jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
      val (pageErrors, maybeEmailFieldError, maybeEmailFieldValue) =
        emailAuthDetails.get(formTemplateContext) match {
          case Some(InvalidEmail(EmailId(value), message, unprocessedParams)) =>
            val params = unprocessedParams.map(param => request.messages.messages(param))
            (
              Errors(
                new components.GovukErrorSummary()(
                  ErrorSummary(
                    errorList = List(
                      ErrorLink(
                        href = Some("#email"),
                        content = content.Text(
                          request.messages
                            .messages(message, params: _*)
                        )
                      )
                    ),
                    title = content.Text(request.messages.messages("error.summary.heading"))
                  )
                )
              ),
              Some(
                ErrorMessage.errorMessageWithDefaultStringsTranslated(
                  content = content.Text(
                    request.messages
                      .messages(message, params: _*)
                  )
                )
              ),
              Some(value)
            )
          case _ => (NoErrors, None, None)
        }

      def showEnterEmailPage(emailUseInfo: Option[LocalisedString]) = Ok(
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
      formTemplate.authConfig match {
        case EmailAuthConfig(_, emailUseInfo, _, _) => showEnterEmailPage(emailUseInfo)
        case Composite(configs) =>
          val compositeAuthDetails: CompositeAuthDetails =
            jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)

          compositeAuthDetails.get(formTemplateContext).flatMap { selection =>
            AuthConfig.getAuthConfig(selection, configs)
          } match {
            case Some(EmailAuthConfig(_, emailUseInfo, _, _)) => showEnterEmailPage(emailUseInfo)
            case _                                            => notSetEmailResult(formTemplateId)
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
                emailAuthDetails + (formTemplateId -> InvalidEmail(
                  EmailId(CIString.empty),
                  "generic.auth.email.error.required",
                  Nil
                ))
              )
            ).pure[Future],
          { email =>
            val emailId = EmailId(CIString(email))
            val formTemplateContext = request.attrs(FormTemplateKey)
            EmailAddress.isValid(emailId.value.toString.trim) match {
              case true if emailId.value.toString.trim.size > ValidationValues.emailLimit =>
                Redirect(
                  uk.gov.hmrc.gform.gform.routes.EmailAuthController.emailIdForm(formTemplateId, continue)
                ).addingToSession(
                  EMAIL_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
                    emailAuthDetails + (formTemplateId -> InvalidEmail(
                      emailId,
                      "generic.error.text.maxLength",
                      List("", "emailAuth.email", ValidationValues.emailLimit.toString)
                    ))
                  )
                ).pure[Future]
              case true =>
                sendEmailWithConfirmationCode(formTemplateContext, emailId).map {
                  case None => // User has lost his session, so we start from the beginning
                    Redirect(uk.gov.hmrc.gform.gform.routes.NewFormController.dashboard(formTemplateId))
                  case Some(emailAndCode) =>
                    Redirect(
                      uk.gov.hmrc.gform.gform.routes.EmailAuthController
                        .confirmCodeForm(formTemplateId, None, continue, None)
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
                    emailAuthDetails + (formTemplateId -> InvalidEmail(
                      emailId,
                      "generic.auth.email.error.pattern",
                      Nil
                    ))
                  )
                ).pure[Future]
            }

          }
        )

    }

  def confirmCodeForm(
    formTemplateId: FormTemplateId,
    error: Option[Boolean],
    continue: String,
    maybeCodeLength: Option[Int]
  ): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      val emailAuthDetails: EmailAuthDetails =
        jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)

      val (pageErrors, maybeCodeFieldError) = error match {
        case Some(true) =>
          val errorMessage = maybeCodeLength match {
            case Some(4) => "emailAuth.error.invalidCode"
            case _       => "emailAuth.error.invalidCodeLength"
          }
          (
            Errors(
              new components.GovukErrorSummary()(
                ErrorSummary(
                  errorList = List(
                    ErrorLink(
                      href = Some("#code"),
                      content = content.Text(request.messages.messages(errorMessage))
                    )
                  ),
                  title = content.Text(request.messages.messages("error.summary.heading"))
                )
              )
            ),
            Some(
              ErrorMessage.errorMessageWithDefaultStringsTranslated(
                content = content.Text(request.messages.messages(errorMessage))
              )
            )
          )
        case _ => (NoErrors, None)
      }

      (emailAuthDetails.get(formTemplateContext), formTemplate.authConfig) match {
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
              .get(formTemplateContext)

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
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate

      confirmCodeForm
        .bindFromRequest()
        .fold(
          _ =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.EmailAuthController
                .confirmCodeForm(formTemplateId, Some(true), continue, None)
            ).pure[Future],
          { case (email: String, code: String) =>
            val emailAuthDetails: EmailAuthDetails =
              jsonFromSession(request, EMAIL_AUTH_DETAILS_SESSION_KEY, EmailAuthDetails.empty)
            emailAuthDetails
              .checkCodeAndConfirm(
                formTemplateId,
                formTemplate,
                EmailAndCode(CIString(email), EmailConfirmationCode(CIString(code.trim)))
              )
              .fold {
                Redirect(
                  uk.gov.hmrc.gform.gform.routes.EmailAuthController
                    .confirmCodeForm(formTemplateId, Some(true), continue, Some(code.length))
                )
              } { confirmedEmailAuthDetails =>
                val confirmedEmailAuthDetailsStr = toJsonStr(confirmedEmailAuthDetails)

                formTemplate.authConfig match {
                  case HasEmailConfirmation(_) =>
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
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate

      formTemplate.authConfig match {
        case HasEmailConfirmation(emailConfirmation) =>
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
    formTemplateContext: FormTemplateContext,
    emailId: EmailId
  )(implicit
    request: Request[AnyContent],
    hc: HeaderCarrier,
    me: MonadError[Future, Throwable],
    l: LangADT
  ): Future[Option[EmailAndCode]] = {
    val formTemplate = formTemplateContext.formTemplate

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
              l,
              formTemplate._id
            )
          )
          .map(_ => Some(emailAndCode))

      case composite: Composite =>
        val maybeCompositeAuthDetails: Option[String] =
          jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)
            .get(formTemplateContext)

        maybeCompositeAuthDetails.traverse { compositeAuthDetails =>
          val config = AuthConfig
            .getAuthConfig(compositeAuthDetails, composite.configs)

          config match {
            case Some(EmailAuthConfig(service, _, _, _)) =>
              gformConnector
                .sendEmail(
                  ConfirmationCodeWithEmailService(
                    NotifierEmailAddress(emailId.value.toString),
                    emailAndCode.code,
                    service,
                    l,
                    formTemplate._id
                  )
                )
                .map(_ => emailAndCode)

            case _ =>
              me.raiseError(new IllegalArgumentException(s"Unsupported auth config ${formTemplate.authConfig}"))
          }
        }

      case _ => me.raiseError(new IllegalArgumentException(s"Unsupported auth config ${formTemplate.authConfig}"))
    }
  }

  private def notSetEmailResult(formTemplateId: FormTemplateId): Future[Result] = {
    logger.warn(s"AuthModule for formTemplate $formTemplateId is not set to email")
    BadRequest(s"Unable to accept Email for formTemplate $formTemplateId").pure[Future]
  }
}
