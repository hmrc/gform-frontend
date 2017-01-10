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

import uk.gov.hmrc.bforms.models.LandfillTaxDetails
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Action
<<<<<<< HEAD
=======
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository


>>>>>>> 1b8975c1390fb523a221795dd3f8a6fd040db992

@Singleton
class LandfillTaxForm @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

  implicit val x : TaxFormSaveExit[LandfillTaxDetails] = TaxFormSaveExit.nameLater

  def landfillTaxFormDisplay(registrationNumber : String) = Action.async { implicit request =>
    val form = LandfillTaxDetails.form
    if (TaxFormRetrieve.) {
      form.fill(object?)
    }
    //Need a check of the database before continuing.
    Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(LandfillTaxDetails.form, registrationNumber.filter(Character.isLetterOrDigit))))
  }

//  def saveAndExit(rn: String) = landfillTaxSaveAndExit[LandfillTaxDetails](rn)

  def landfillTaxForms(rn: String) = landfillTax(rn)

  private def landfillTax[A](registrationNumber : String)(implicit taxFormSaveExit:TaxFormSaveExit[A]) = Action.async { implicit request =>
      LandfillTaxDetails.form.bindFromRequest.fold(
        error => {
          Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(error, registrationNumber)))
        },
        content => {
          if (content.save.equals("Exit")) {
            SaveExit.SaveForm(content) map {
              case false => Ok("Failed")
              case true => Ok("Worked")
            }
          } else if(content.save.equals("Continue")) {
            TaxFormSubmission.submitTaxForm(content).map {
              case SubmissionResult(Some(errorMessage), _) =>
                val formWithErrors = LandfillTaxDetails.form.withGlobalError(errorMessage)
                BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(formWithErrors, registrationNumber))
              case SubmissionResult(noErrors, Some(submissionAcknowledgement)) =>
                Redirect(routes.LandfillTaxConfirmation.landfillTaxConfirmationDisplay(registrationNumber, submissionAcknowledgement))
            }
          } else {
            Future.successful(Ok("Failed"))
          }
        }
      )
  }

//  private def landfillTaxSaveAndExit[A](registrationNumber : String)(implicit taxFormSaveExit:TaxFormSaveExit[A]) = Action.async { implicit request =>
//    LandfillTaxDetails.form.bindFromRequest.fold(
//      error => {
//        println("inside Error")
//        val errors = error
////        Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(error, registrationNumber)))
//        SaveExit.SaveForm(error.get)(x).map {
//          case false => Ok("Failed")
//          case true => Ok("Worked")
//        }
//      },
//        content => {
//          println("inside content")
//          Future.successful(Ok("Failed"))
//  }
//    )
//  }
//
//  def landfillTaxFormSubmitContinue(registrationNumber: String) = Action.async { implicit request =>
//    LandfillTaxDetails.form.bindFromRequest().fold(
//      formWithErrors =>
//        Future.successful(
//          BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(formWithErrors, registrationNumber))
//        ),
//      formData =>
//        TaxFormSubmission.submitTaxForm(formData).map {
//          case SubmissionResult(Some(errorMessage), _) =>
//            val formWithErrors = LandfillTaxDetails.form.withGlobalError(errorMessage)
//            BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(formWithErrors, registrationNumber))
//          case SubmissionResult(noErrors, Some(submissionAcknowledgement)) =>
//            Redirect(routes.LandfillTaxConfirmation.landfillTaxConfirmationDisplay(registrationNumber, submissionAcknowledgement))
//        }
//    )
//  }
}