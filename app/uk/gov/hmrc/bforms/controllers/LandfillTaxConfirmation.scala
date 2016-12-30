/*
 * Copyright 2016 HM Revenue & Customs
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

package uk.gov.hmrc.bforms.controllers

import javax.inject.{Inject, Singleton}

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Action
import uk.gov.hmrc.bforms.FrontendAuthConnector
import uk.gov.hmrc.bforms.controllers.auth.BFormsAuth
import uk.gov.hmrc.play.frontend.auth.Actions
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LandfillTaxConfirmation @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport with Actions {
  self: BFormsAuth =>

  override lazy val authConnector: AuthConnector = FrontendAuthConnector

  def landfillTaxConfirmationDisplay(registrationNumber: String, submissionAcknowledgement: String) = Action.async { implicit request =>
    val filteredRegistrationNumber = registrationNumber.filter(Character.isLetterOrDigit)
    val landfill_tax_confirmation = uk.gov.hmrc.bforms.views.html.landfill_tax_confirmation(filteredRegistrationNumber, submissionAcknowledgement)
    Future.successful(Ok(landfill_tax_confirmation))
  }
}
