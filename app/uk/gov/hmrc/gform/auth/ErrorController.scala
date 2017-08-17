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

package uk.gov.hmrc.gform.auth

import play.api.mvc.Action
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.Future

object ErrorController extends FrontendController {

  def insufficientEnrolments = Action.async { implicit request =>
    val pageTitle = request.flash.get("formTitle").getOrElse("")
    Future.successful(
      Ok(uk.gov.hmrc.gform.views.html.hardcoded.pages.insufficient_enrolments(pageTitle))
    )
  }
}
