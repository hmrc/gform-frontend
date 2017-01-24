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

import uk.gov.hmrc.bforms.models._
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Action

@Singleton
class LandfillTaxForm @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

  def landfillTaxFormDisplay(registrationNumber : String) = Action.async { implicit request =>
    val form = LandfillTaxDetails.form
    RetrieveService.retrieveFromBackEnd(registrationNumber).map {
      case Left(x) =>
        val filledForm = form.fill(x)
        Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(filledForm, registrationNumber.filter(Character.isLetterOrDigit)))
      case Right(_) =>
        Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit)))
    }
  }

  def landfillTaxForms(registrationNumber : String) = Action.async { implicit request =>
    LandfillTaxDetails.form.bindFromRequest.fold(
      error => {
        SaveService.save(Right(error.data), registrationNumber)
        Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(error, registrationNumber)))
      },
      content => {
        content.save match {
          case "Exit" =>
            SaveService.save(Left(content), registrationNumber) map {
              case VerificationResult(Some(errorMsg)) => Ok("failed")
              case VerificationResult(noErrors) =>  Ok("Worked")
            }
          case "Continue" =>
            SaveService.save(Left(content), registrationNumber)
              TaxFormSubmission.submit(registrationNumber).map {
                case SubmissionResult(Some(errorMessage), _) =>
                  val formWithErrors = LandfillTaxDetails.form.withGlobalError(errorMessage)
                  BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(formWithErrors, registrationNumber))
                case SubmissionResult(noErrors, Some(submissionAcknowledgement)) =>
                  Redirect(routes.LandfillTaxConfirmation.landfillTaxConfirmationDisplay(registrationNumber, submissionAcknowledgement))
              }
          case _ => Future.successful(Ok("Failed"))
        }
      }
    )
  }
}