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

import play.api.Play.current
import play.api.i18n.Messages.Implicits._
import play.api.mvc._
import uk.gov.hmrc.bforms.FrontendAuthConnector
import uk.gov.hmrc.bforms.controllers.auth.BFormsAuth
import uk.gov.hmrc.play.frontend.controller.FrontendController
import uk.gov.hmrc.play.frontend.auth.Actions
import uk.gov.hmrc.play.frontend.auth.connectors.AuthConnector

import scala.concurrent.Future

@Singleton
class LandfillTax @Inject() extends FrontendController with Actions { self: BFormsAuth =>

  override lazy val authConnector: AuthConnector = FrontendAuthConnector

  def landfillTaxDisplay (registrationNumber : String) = AsyncAuthenticatedAction { implicit authContext => implicit request =>
		val filteredRegistrationNumber = registrationNumber.filter(Character.isLetterOrDigit)
    val landfill_tax = uk.gov.hmrc.bforms.views.html.landfill_tax(filteredRegistrationNumber)
    Future.successful(Ok(landfill_tax))
  }
  def landfillTaxSubmitContinue(registrationNumber : String) = AsyncAuthenticatedAction { implicit authContext => implicit request =>
    Future.successful(Redirect(routes.LandfillTaxForm.landfillTaxFormDisplay(registrationNumber))) }
}
