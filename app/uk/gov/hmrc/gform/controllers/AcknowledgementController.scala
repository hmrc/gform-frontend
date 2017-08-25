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

package uk.gov.hmrc.gform.controllers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import javax.inject.{ Inject, Singleton }
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AckSection, Default }
import uk.gov.hmrc.play.frontend.controller.FrontendController
import scala.concurrent.Future

@Singleton
class AcknowledgementController @Inject() (controllersModule: ControllersModule) extends FrontendController {

  import AuthenticatedRequest._
  import controllersModule.i18nSupport._

  def showAcknowledgement(formId: FormId) = auth.async(formId) { implicit authRequest =>
    val content = authRequest.formTemplate.acknowledgementSection.map((ackSection: AckSection) =>
      uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement_content_partial(ackSection))
    val timeFormat = DateTimeFormatter.ofPattern("HH:mm")
    val dateFormat = DateTimeFormatter.ofPattern("dd MMM yyyy")
    val now = LocalDateTime.now()

    val timeMessage = s""" at ${now.format(timeFormat)} on ${now.format(dateFormat)}"""
    Future.successful(
      Ok(
        uk.gov.hmrc.gform.views.html.hardcoded.pages.partials.acknowledgement(
          timeMessage, content, authRequest.formTemplate.formCategory.getOrElse(Default)
        )
      )
    )
  }

  private lazy val auth = controllersModule.authenticatedRequestActions
}
