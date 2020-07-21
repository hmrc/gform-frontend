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

import play.api.Logger
import play.api.i18n.{ I18nSupport, Messages }
import play.api.mvc._
import uk.gov.hmrc.gform.commons.MarkDownUtil
import uk.gov.hmrc.gform.config.FrontendAppConfig
import uk.gov.hmrc.gform.controllers.AuthenticatedRequestActions
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.sharedmodel.LocalisedString
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, HmrcVerified }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationPrint
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, LangADT }
import uk.gov.hmrc.gform.summary.SummaryRenderingService
import uk.gov.hmrc.gform.summarypdf.PdfGeneratorService
import uk.gov.hmrc.http.BadRequestException
import uk.gov.hmrc.play.bootstrap.controller.FrontendController
import uk.gov.hmrc.gform.views.html.hardcoded.pages._

import scala.concurrent.{ ExecutionContext, Future }

class IdentityVerificationController(
  auth: AuthenticatedRequestActions,
  i18nSupport: I18nSupport,
  frontendAppConfig: FrontendAppConfig,
  messagesControllerComponents: MessagesControllerComponents
) extends FrontendController(messagesControllerComponents) {

  def failure(formTemplateId: FormTemplateId): Action[AnyContent] = pageWithContent(_.ivFailure)(formTemplateId)

  def enrolmentsNeeded(formTemplateId: FormTemplateId): Action[AnyContent] =
    pageWithContent(_.notAllowedIn)(formTemplateId)

  private def pageWithContent(f: HmrcVerified => LocalisedString)(formTemplateId: FormTemplateId): Action[AnyContent] =
    auth.asyncNoAuth(formTemplateId) { implicit request => implicit l => formTemplate =>
      import i18nSupport._

      formTemplate.authConfig match {
        case hmrcVerified @ HmrcVerified(_, _) =>
          Future.successful(
            Ok(
              pure(
                MarkDownUtil.markDownParser(f(hmrcVerified)),
                "identity.verification",
                formTemplate,
                frontendAppConfig)))
        case otherwise =>
          Logger.warn(
            s"Illegal page access with $formTemplateId. It has $otherwise, but 'HmrcVerified(...)' was expected instead")
          Future.failed(new BadRequestException(s"HmrcVerified is not defined for $formTemplateId"))
      }
    }
}
