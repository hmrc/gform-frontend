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

import java.text.Format
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale
import javax.inject.{Inject, Singleton}

import play.api.Logger
import uk.gov.hmrc.bforms.models._
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.libs.json.{Format, _}
import reactivemongo.api.DB
import uk.gov.hmrc.bforms.connectors.{BformsConnector, VerificationResult}
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import play.api.mvc.Action

case class KeyPair(id: String, value: String)

object KeyPair {

  implicit val keyPairReader = Json.reads[KeyPair]
}

@Singleton
class LandfillTaxForm @Inject()(val messagesApi: MessagesApi, repository: LandFillTaxRepository)(implicit ec: ExecutionContext, db : DB)
  extends FrontendController with I18nSupport {

  implicit val y : TaxFormRetrieve[String, LandFillTaxDetailsPersistence, Map[String, String]] = TaxFormRetrieve.somethingElse(repository)
  implicit val x : TaxFormSaveExit[Either[LandfillTaxDetails, Map[String, String]]] = TaxFormSaveExit.nameLater(repository)


  def landfillTaxFormDisplay(registrationNumber : String) = Action.async { implicit request =>
    val form = LandfillTaxDetails.form
    RetrieveService.retrieveFromBackEnd(registrationNumber).flatMap {
      case Left(x) =>
        x.\("fields").validate[List[KeyPair]] match {
          case JsSuccess(js, _) =>
            val filledForm = form.fill(listKeyPairToLandFillTaxDetails(js).validate[LandfillTaxDetails].get)
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(filledForm, registrationNumber.filter(Character.isLetterOrDigit))))
          case JsError(err) =>
            Logger.warn(s"$err")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
        }
      case Right(_) =>
        Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
    }
  }

  private def listKeyPairToLandFillTaxDetails(json: List[KeyPair]) = {
    val obj= json.foldRight(Json.obj()) { (keypair, acc) =>
      val something = if (keypair.id == "environmentalBodies") {
        keypair.id -> Json.parse(keypair.value)
      } else {
        keypair.id -> JsString(keypair.value)
      }
      acc + something
    }
    obj
  }
  def landfillTaxForms(rn: String) = landfillTax(rn)(x)

  private def landfillTax[A](registrationNumber : String)(implicit taxFormSaveExit:TaxFormSaveExit[A]) = Action.async { implicit request =>
      LandfillTaxDetails.form.bindFromRequest.fold(
        error => {

          repository.store(Right(error.data))
          Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(error, registrationNumber)))
        },
        content => {
          println(content)
          if (content.save.equals("Exit")) {
            SaveExit.saveToBackEnd(content) map {
              case VerificationResult(Some(errorMsg)) => Ok("failed")
              case VerificationResult(noErrors) =>  Ok("Worked")
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