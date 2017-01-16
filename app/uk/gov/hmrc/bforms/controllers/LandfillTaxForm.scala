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

import uk.gov.hmrc.bforms.models.{EnvironmentalBody, Error, LandFillTaxDetailsPersistence, LandfillTaxDetails}
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Action
import reactivemongo.api.DB
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository



@Singleton
class LandfillTaxForm @Inject()(val messagesApi: MessagesApi, repository: LandFillTaxRepository)(implicit ec: ExecutionContext, db : DB)
  extends FrontendController with I18nSupport {

//  implicit val repo : LandFillTaxRepository = LandFillTaxRepository.apply(db

  implicit val y : TaxFormRetrieve[String, LandFillTaxDetailsPersistence, Map[String, String]] = TaxFormRetrieve.somethingElse(repository)
  implicit val x : TaxFormSaveExit[Either[LandfillTaxDetails, Map[String, String]]] = TaxFormSaveExit.nameLater(repository)

  def landfillTaxFormDisplay(registrationNumber : String) = Action.async { implicit request =>
    val form = LandfillTaxDetails.form
    RetrieveService.retrieve(registrationNumber).flatMap {
      case x : Either[Unit, Either[LandFillTaxDetailsPersistence, Map[String, String]]] => {
        x match {
          case Right(Left(obj)) => {
            println("Right(list)")
            val formData : LandFillTaxDetailsPersistence = obj
            val filledForm = new LandfillTaxDetails(formData.registrationNumber.value,
              "",
              formData.firstName.value,
              formData.lastName.value,
              formData.telephoneNumber.value,
              formData.status.value,
              formData.nameOfBusiness.value,
              formData.accountingPeriodStartDate,
              formData.accountingPeriodEndDate,
              formData.taxDueForThisPeriod.value,
              formData.underDeclarationsFromPreviousPeriod.value,
              formData.overDeclarationsForThisPeriod.value,
              formData.taxCreditClaimedForEnvironment.value,
              formData.badDebtReliefClaimed.value,
              formData.otherCredits.value,
              formData.standardRateWaste.value,
              formData.lowerRateWaste.value,
              formData.exemptWaste.value,
              formData.environmentalBody1,
              formData.emailAddress.value, formData.confirmEmailAddress.value)
            val formFilled = form.fill(filledForm)
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(formFilled, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case Right(Right(obj)) => {
            val localDateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy").withLocale(Locale.UK)
            println("Right(obj)")
            val formData : Map[String, String] = obj
            val accountPeriodStartDate =
              formData("accountingPeriodStartDate") match {
                case "" => LocalDate.MIN
                case s => LocalDate.parse(s,localDateFormatter)
            }

            val accountPeriodEndDate = {
              formData("accountingPeriodEndDate") match {
                case "" => LocalDate.MIN
                case s => LocalDate.parse(s,localDateFormatter)
              }
            }

            val taxCreditClaimedForEnvironment:BigDecimal = {
              formData("taxCreditClaimedForEnvironment") match {
                case "" => -1
                case s => BigDecimal(s)
              }
            }

            val environmentalBodyAmount: BigDecimal = {
              formData("environmentalBody1[1].amount") match {
                case "" => BigDecimal(-1)
                case s => BigDecimal(s)
              }
            }

            val filledForm = new LandfillTaxDetails(formData("registrationNumber"),
              "",
              formData("firstName"),
              formData("lastName"),
              formData("telephoneNumber"),
              formData("status"),
              formData("nameOfBusiness"),
              accountPeriodStartDate,
              accountPeriodEndDate,
              formData("taxDueForThisPeriod"),
              formData("underDeclarationsFromPreviousPeriod"),
              formData("overDeclarationsForThisPeriod"),
              taxCreditClaimedForEnvironment,
              formData("badDebtReliefClaimed"),
              formData("otherCredits"),
              formData("standardRateWaste"),
              formData("lowerRateWaste"),
              formData("exemptWaste"),
              Seq(EnvironmentalBody(formData("environmentalBody1[1].bodyName"),environmentalBodyAmount)),
              Some(formData("emailAddress")),
              Some(formData("confirmEmailAddress")))
            val formFilled = form.fill(filledForm)
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(formFilled, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case Left(()) => {
            println("Unit")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case _ => {
            println("Blank")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
          }
        }
      }
      case _ => {
        println("Unit")
        Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form(form, registrationNumber.filter(Character.isLetterOrDigit))))
      }
    }
  }

  def landfillTaxForms(rn: String) = landfillTax(rn)(x)

  private def landfillTax[A](registrationNumber : String)(implicit taxFormSaveExit:TaxFormSaveExit[A]) = Action.async { implicit request =>
      LandfillTaxDetails.form.bindFromRequest.fold(
        error => {
          println(error.data)
          repository.store(Right(error.data))
          Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form(error, registrationNumber)))
        },
        content => {
          println(content)
          if (content.save.equals("Exit")) {
            SaveExit.SaveForm(Left(content))(x) map {
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