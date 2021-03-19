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
import play.api.data
import play.api.i18n.I18nSupport
import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.{ EmailAuthDetails, EmailCodeConfirmation }
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.NonAuthenticatedRequestActions
import uk.gov.hmrc.gform.gform.EmailAuthUtils.{ EMAIL_CODES_SESSION_KEY, fromSession }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.EmailId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService.digitalContact
import uk.gov.hmrc.gform.sharedmodel.email.{ ConfirmationCodeWithEmailService, EmailConfirmationCode, EmailTemplateId }
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils.toJsonStr
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ EmailAuthConfig, FormTemplate, FormTemplateId }
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.govukfrontend.views.html.components
import uk.gov.hmrc.govukfrontend.views.viewmodels.content
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage
import uk.gov.hmrc.govukfrontend.views.viewmodels.errorsummary.{ ErrorLink, ErrorSummary }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

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

  def emailIdForm(formTemplateId: FormTemplateId): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      Ok(
        html.auth.enter_email(
          formTemplate,
          frontendAppConfig,
          uk.gov.hmrc.gform.gform.routes.EmailAuthController.sendEmail(formTemplateId)
        )
      ).pure[Future]
    }

  def sendEmail(formTemplateId: FormTemplateId): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => _ =>
      val emailId = EmailId(emailForm.bindFromRequest().get)
      val formTemplate = request.attrs(FormTemplateKey)
      sendEmailWithConfirmationCode(formTemplate, emailId).map { emailAndCode =>
        val emailAuthDetails: EmailAuthDetails = fromSession(request, EMAIL_CODES_SESSION_KEY, EmailAuthDetails())
        Redirect(
          uk.gov.hmrc.gform.gform.routes.EmailAuthController.confirmCodeForm(formTemplateId)
        ).addingToSession(
          EMAIL_CODES_SESSION_KEY -> toJsonStr(
            emailAuthDetails + (formTemplateId -> EmailCodeConfirmation(emailAndCode))
          )
        )
      }
    }

  def confirmCodeForm(formTemplateId: FormTemplateId): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      val emailAuthDetails: EmailAuthDetails = fromSession(request, EMAIL_CODES_SESSION_KEY, EmailAuthDetails())

      emailAuthDetails.get(formTemplateId) match {
        case Some(authDetails) =>
          Ok(
            html.auth.confirm_code(
              formTemplate,
              frontendAppConfig,
              EmailId(authDetails.emailAndCode.email),
              uk.gov.hmrc.gform.gform.routes.EmailAuthController.confirmCode(formTemplateId)
            )
          ).pure[Future]
        case None =>
          logger.warn(s"Could not find emailAuthDetails in session for formTemplate $formTemplateId")
          BadRequest(s"Unable to confirm code for formTemplate $formTemplateId").pure[Future]
      }
    }

  def confirmCode(formTemplateId: FormTemplateId): Action[AnyContent] =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      val (email, code) = confirmCodeForm.bindFromRequest().get
      val emailAuthDetails: EmailAuthDetails = fromSession(request, EMAIL_CODES_SESSION_KEY, EmailAuthDetails())
      emailAuthDetails
        .checkCodeAndConfirm(formTemplateId, EmailAndCode(email, EmailConfirmationCode(code)))
        .fold {
          val pageErrors: HasErrors = Errors(
            new components.govukErrorSummary()(
              ErrorSummary(
                errorList = List(
                  ErrorLink(
                    href = Some("#code"),
                    content = content.Text("Enter the code we emailed you. This is 4 letters, like DNLC")
                  )
                ),
                title = content.Text(request.messages.messages("error.summary.heading"))
              )
            )
          )
          val maybeCodeFieldError = Some(
            ErrorMessage(
              content = content.Text("Enter the code we emailed you. This is 4 letters, like DNLC")
            )
          )
          Ok(
            html.auth.confirm_code(
              formTemplate,
              frontendAppConfig,
              EmailId(email),
              uk.gov.hmrc.gform.gform.routes.EmailAuthController.confirmCode(formTemplateId),
              pageErrors,
              maybeCodeFieldError
            )
          )
        } { confirmedEmailAuthDetails =>
          Redirect(
            uk.gov.hmrc.gform.gform.routes.NewFormController.dashboard(formTemplateId)
          ).addingToSession(
            EMAIL_CODES_SESSION_KEY -> toJsonStr(
              confirmedEmailAuthDetails
            )
          )
        }
        .pure[Future]
    }

  private def sendEmailWithConfirmationCode[D <: DataOrigin](
    formTemplate: FormTemplate,
    emailId: EmailId
  )(implicit
    hc: HeaderCarrier,
    me: MonadError[Future, Throwable]
  ): Future[EmailAndCode] =
    formTemplate.authConfig match {
      case EmailAuthConfig(emailCodeTemplate) =>
        val emailAndCode = EmailAndCode.emailVerificationCode(emailId.value)
        gformConnector
          .sendEmail(
            ConfirmationCodeWithEmailService(
              NotifierEmailAddress(emailId.value),
              emailAndCode.code,
              digitalContact(EmailTemplateId(emailCodeTemplate.value))
            )
          )
          .map(_ => emailAndCode)
      case _ => me.raiseError(new IllegalArgumentException(s"Unsupported auth config ${formTemplate.authConfig}"))
    }
}
