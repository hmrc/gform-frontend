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
import uk.gov.hmrc.bforms.connectors.VerificationResult
import play.api.libs.json._
import play.api.mvc.Action

@Singleton
class LandfillTaxForm @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

  def landfillTaxFormDisplay(registrationNumber : String) = Action.async { implicit request =>
    val form = LandfillTaxDetails.form
    RetrieveService.retrieveFromBackEnd(registrationNumber).flatMap {
      case Left(x) =>
        println("Left(x)")
        x.\("fields").validate[List[KeyPair]] match {
          case JsSuccess(js, _) =>
            println("JsSucess(js,_)")
            println(listKeyPairToLandFillTaxDetails(js).validate[LandfillTaxDetails])
            val filledForm = form.fill(listKeyPairToLandFillTaxDetails(js).validate[LandfillTaxDetails] match {
              case JsSuccess(jss,_) => jss
              case JsError(err) =>
                createfilledObject(js)
            })
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(filledForm, registrationNumber.filter(Character.isLetterOrDigit))))
          case JsError(err) =>
            print("JsError(err)")
            Logger.warn(s"$err")

            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
        }
      case Right(_) =>
        println("Right(_)")
        Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
    }
  }

  def landfillTaxForms(rn: String) = landfillTax(rn)

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

  private def landfillTax(registrationNumber : String) = Action.async { implicit request =>
      LandfillTaxDetails.form.bindFromRequest.fold(
        error => {
          SaveService.saveToBackEndFormWithErrors(error.data, registrationNumber)
          Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(error, registrationNumber)))
        },
        content => {
          println(content)
          if (content.save.equals("Exit")) {
            SaveService.saveToBackEnd(content, registrationNumber) map {
              case VerificationResult(Some(errorMsg)) => Ok("failed")
              case VerificationResult(noErrors) =>  Ok("Worked")
            }
          } else if(content.save.equals("Continue")) {
            SaveService.saveToBackEnd(content, registrationNumber)
            TaxFormSubmission.submit(registrationNumber).map {
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

  private def createfilledObject(js : List[KeyPair]) ={
    val date = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    val dateformatter = date.withLocale(Locale.UK)
    val mapOfValues = js.map(f => f.id -> f.value).toMap
    new LandfillTaxDetails(
      mapOfValues("registrationNumber"),
      mapOfValues("save"),
      mapOfValues("firstName"),
      mapOfValues("lastName"),
      mapOfValues("telephoneNumber"),
      mapOfValues("status"),
      mapOfValues("nameOfBusiness"),
      if(mapOfValues("accountingPeriodStartDate") != ""){
        LocalDate.parse(mapOfValues("accountingPeriodStartDate"), dateformatter)
      } else {LocalDate.MIN},
      if(mapOfValues("accountingPeriodEndDate") != ""){
        LocalDate.parse(mapOfValues("accountingPeriodEndDate"), dateformatter)
      } else {LocalDate.MIN},
      mapOfValues("taxDueForThisPeriod"),
      mapOfValues("underDeclarationsFromPreviousPeriod"),
      mapOfValues("overDeclarationsForThisPeriod"),
      if(mapOfValues("taxCreditClaimedForEnvironment") != ""){
        BigDecimal(mapOfValues("taxCreditClaimedForEnvironment"))
      } else {BigDecimal(-1)},
      mapOfValues("badDebtReliefClaimed"),
      mapOfValues("otherCredits"),
      mapOfValues("standardRateWaste"),
      mapOfValues("lowerRateWaste"),
      mapOfValues("exemptWaste"),
      if(mapOfValues("environmentalBodies") != "[{\"bodyName\":\"\",\"amount\":\"\"}]") {
        Json.obj("environmentalBodies" -> mapOfValues("environmentalBodies")).validate[Seq[EnvironmentalBody]].get
      } else {
        Seq(EnvironmentalBody("", -1))
      },
      Some(mapOfValues("emailAddress")),
      Some(mapOfValues("confirmEmailAddress"))
    )
  }
}