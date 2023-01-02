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

package uk.gov.hmrc.gform
package auth

import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.NonAuthenticatedRequestActionsAlgebra
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.Future

class ErrorController(
  frontendAppConfig: FrontendAppConfig,
  i18nSupport: I18nSupport,
  messagesControllerComponents: MessagesControllerComponents,
  nonAuth: NonAuthenticatedRequestActionsAlgebra[Future]
) extends FrontendController(messagesControllerComponents) {

  import i18nSupport._

  def insufficientEnrolments(formTemplateId: FormTemplateId) = nonAuth { implicit request => implicit l =>
    val formTemplateWithRedirects = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateWithRedirects.formTemplate
    val pageTitle = request.flash.get("formTitle").getOrElse("")
    Ok(views.html.hardcoded.pages.insufficient_enrolments(formTemplate, pageTitle, frontendAppConfig))
  }

  def browserForbidden(formTemplateId: FormTemplateId) = nonAuth { implicit request => implicit l =>
    val formTemplateWithRedirects = request.attrs(FormTemplateKey)
    val formTemplate = formTemplateWithRedirects.formTemplate
    val pageTitle = request.flash.get("formTitle").getOrElse("")
    Ok(views.html.hardcoded.pages.browser_forbidden(formTemplate, pageTitle, frontendAppConfig))
  }
}
