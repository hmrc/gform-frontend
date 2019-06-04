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

package uk.gov.hmrc.gform.auth

import play.api.i18n.I18nSupport
import play.api.mvc.Action
import uk.gov.hmrc.gform.config.{ AppConfig, FrontendAppConfig }
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

class AgentEnrolmentController(
  appConfig: AppConfig,
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  auth: AuthenticatedRequestActions
) extends FrontendController {

  import i18nSupport._

  private def agentSubscribeUrl(continueUrl: String): String = {
    val encodedContinueUrl = java.net.URLEncoder.encode(continueUrl, "UTF-8")
    val baseUrl = appConfig.`agent-subscription-frontend-base-url`
    s"$baseUrl/agent-subscription/business-type?continue=$encodedContinueUrl"
  }

  def prologue(formTemplateId: FormTemplateId, formName: String) =
    auth.asyncGGAuth(formTemplateId) { implicit request => implicit l => formWithoutData =>
      val continueUrl = uk.gov.hmrc.gform.gform.routes.FormController.dashboard(formTemplateId).url
      Future.successful(
        Ok(
          uk.gov.hmrc.gform.views.html.hardcoded.pages
            .agent_enrolment_prologue(formWithoutData.formTemplate, frontendAppConfig, agentSubscribeUrl(continueUrl)))
      )
    }
}
