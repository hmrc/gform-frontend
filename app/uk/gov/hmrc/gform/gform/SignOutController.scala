/*
 * Copyright 2020 HM Revenue & Customs
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

import play.api.mvc.{ Action, AnyContent, MessagesControllerComponents }
import scala.concurrent.Future
import uk.gov.hmrc.gform.config.FrontendAppConfig
import play.api.i18n.I18nSupport
import uk.gov.hmrc.gform.controllers.NonAuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.views.html.hardcoded.pages.signed_out
import uk.gov.hmrc.play.bootstrap.controller.FrontendController

class SignOutController(
  frontendConfig: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents,
  nonAuth: NonAuthenticatedRequestActionsAlgebra[Future]
) extends FrontendController(messagesControllerComponents) with I18nSupport {

  def signOut(formTemplateId: FormTemplateId): Action[AnyContent] = nonAuth { request => l =>
    val signBackInURL = routes.NewFormController.dashboard(formTemplateId).url
    Redirect(routes.SignOutController.showSignedOutPage(signBackInURL)).withNewSession
  }

  def showSignedOutPage(signBackInUrl: String): Action[AnyContent] = nonAuth { implicit request => implicit l =>
    Ok(signed_out(signBackInUrl, frontendConfig))
  }
}
