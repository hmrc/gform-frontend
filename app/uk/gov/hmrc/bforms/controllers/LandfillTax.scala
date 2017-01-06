/*
 * Copyright 2017 HM Revenue & Customs
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
import uk.gov.hmrc.bforms.models.LandfillTaxDetails
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LandfillTax @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

  def landfillTaxDisplay(registrationNumber: String) = Action.async { implicit request =>
    Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax(registrationNumber.filter(Character.isLetterOrDigit))))
  }


  def landfillTaxSubmitContinue(registrationNumber: String) = Action.async {
    Future.successful(Redirect(routes.LandfillTaxForm.landfillTaxFormDisplay(registrationNumber)))
  }
}
