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

import cats.implicits._
import org.apache.commons.codec.net.URLCodec
import org.slf4j.LoggerFactory
import uk.gov.hmrc.gform.sharedmodel.formtemplate.SuppressErrors
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.FormTemplateKey
import uk.gov.hmrc.gform.auth.models.CompositeAuthDetails
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.GformSessionKeys.COMPOSITE_AUTH_DETAILS_SESSION_KEY
import uk.gov.hmrc.gform.controllers.NonAuthenticatedRequestActions
import uk.gov.hmrc.gform.gform.SessionUtil.jsonFromSession
import uk.gov.hmrc.gform.sharedmodel.form.EmailAndCode.toJsonStr
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AuthConfig, FormTemplateId }
import uk.gov.hmrc.gform.views.hardcoded.CompositeAuthFormPage
import uk.gov.hmrc.gform.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ ExecutionContext, Future }

class CompositeAuthController(
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  nonAutheticatedRequestActions: NonAuthenticatedRequestActions,
  frontendAppConfig: FrontendAppConfig
)(implicit ec: ExecutionContext)
    extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  private val choice = play.api.data.Form(
    play.api.data.Forms.single(
      "compositeAuthSelection" -> play.api.data.Forms.nonEmptyText
    )
  )

  private val logger = LoggerFactory.getLogger(getClass)

  def authSelectionForm(
    formTemplateId: FormTemplateId,
    ggId: Option[String],
    continue: String,
    se: SuppressErrors
  ) =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate
      val compositeAuthFormPage =
        choice
          .bindFromRequest()
          .fold(
            errorForm => new CompositeAuthFormPage(formTemplate, errorForm, ggId, se),
            _ => new CompositeAuthFormPage(formTemplate, choice, ggId, se)
          )

      Ok(
        html.auth.auth_selection(
          frontendAppConfig,
          compositeAuthFormPage,
          ggId,
          continue
        )
      ).pure[Future]
    }

  def selectedForm(formTemplateId: FormTemplateId, ggId: Option[String], continue: String) =
    nonAutheticatedRequestActions.async { implicit request => _ =>
      val compositeAuthDetails: CompositeAuthDetails =
        jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)

      val formTemplateContext = request.attrs(FormTemplateKey)
      val formTemplate = formTemplateContext.formTemplate

      choice
        .bindFromRequest()
        .fold(
          _ =>
            Redirect(
              uk.gov.hmrc.gform.gform.routes.CompositeAuthController
                .authSelectionForm(formTemplateId, ggId, continue, SuppressErrors.No)
            )
              .pure[Future],
          {
            case AuthConfig.hmrcSimpleModule =>
              val oQueryString: Option[String] =
                request.queryString.get("continue").flatMap(_.headOption).map(_.split("\\?")) map { v =>
                  if (v.length > 1)
                    v.last
                  else ""
                }
              val continueUrlBase =
                frontendAppConfig.gformFrontendBaseUrl + uk.gov.hmrc.gform.gform.routes.NewFormController
                  .dashboardWithCompositeAuth(formTemplate._id)
                  .url
              val continueUrl = oQueryString match {
                case Some(qs) if qs.nonEmpty => continueUrlBase + "?" + qs
                case _                       => continueUrlBase
              }
              val ggLoginUrl = frontendAppConfig.governmentGatewaySignInUrl
              val codec = new URLCodec("UTF-8")
              val url = s"$ggLoginUrl?continue=${codec.encode(continueUrl)}"
              logger.info(
                s"For a template, ${formTemplate._id.value} user has selected ${AuthConfig.authConfigNameInLogs(AuthConfig.hmrcSimpleModule)} config"
              )

              Redirect(url)
                .pure[Future]

            case selectedConfig =>
              logger.info(
                s"For a template, ${formTemplate._id.value} user has selected ${AuthConfig.authConfigNameInLogs(selectedConfig)} config"
              )
              Redirect(continue)
                .addingToSession(
                  COMPOSITE_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
                    compositeAuthDetails.add(formTemplate, selectedConfig)
                  )
                )
                .pure[Future]
          }
        )
    }

}
