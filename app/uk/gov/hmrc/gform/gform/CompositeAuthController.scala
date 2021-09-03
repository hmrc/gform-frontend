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

import cats.implicits._
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
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers

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

  def authSelectionForm(formTemplateId: FormTemplateId, ggId: Option[String]) =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      val compositeAuthFormPage =
        new CompositeAuthFormPage(formTemplate, choice, ggId)

      Ok(
        html.auth.auth_selection(
          frontendAppConfig,
          compositeAuthFormPage,
          ggId.map(FormDataHelpers.addSpacesInGovermentGatewayId)
        )
      ).pure[Future]
    }

  def selectedForm(formTemplateId: FormTemplateId) =
    nonAutheticatedRequestActions.async { implicit request => implicit lang =>
      val formTemplate = request.attrs(FormTemplateKey)
      val compositeAuthDetails: CompositeAuthDetails =
        jsonFromSession(request, COMPOSITE_AUTH_DETAILS_SESSION_KEY, CompositeAuthDetails.empty)

      choice
        .bindFromRequest()
        .fold(
          errorForm => {
            val compositeAuthFormPage = new CompositeAuthFormPage(formTemplate, errorForm, None)
            BadRequest(html.auth.auth_selection(frontendAppConfig, compositeAuthFormPage, None)).pure[Future]
          },
          {
            case AuthConfig.hmrcSimpleModule =>
              val formTemplate = request.attrs(FormTemplateKey)
              val continueUrl =
                frontendAppConfig.gformFrontendBaseUrl + uk.gov.hmrc.gform.gform.routes.NewFormController
                  .dashboardWithCompositeAuth(formTemplate._id)
                  .url
              val ggLoginUrl = frontendAppConfig.governmentGatewaySignInUrl
              val url = s"$ggLoginUrl?continue=$continueUrl"
              Redirect(url)
                .pure[Future]

            case selectedConfig =>
              Redirect(uk.gov.hmrc.gform.gform.routes.NewFormController.dashboard(formTemplateId))
                .addingToSession(
                  COMPOSITE_AUTH_DETAILS_SESSION_KEY -> toJsonStr(
                    compositeAuthDetails.add(formTemplateId -> selectedConfig)
                  )
                )
                .pure[Future]
          }
        )
    }

}
