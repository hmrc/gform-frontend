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

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Action
import uk.gov.hmrc.bforms.models.persistence.LandfillTaxDetailsDeclarationPersistence
import uk.gov.hmrc.bforms.models.{EnvironmentalBody, LandfillTaxDetailsDeclaration}
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class LandfillTaxFormDeclaration @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

  //  implicit val repo : LandfillTaxRepository = LandfillTaxRepository.apply(db

//  implicit val y: TaxFormDeclarationRetrieve[String, LandfillTaxDetailsDeclarationPersistence, Map[String, String]] = TaxFormDeclarationRetrieve.somethingElse(declarationRepository)
//  implicit val x: TaxFormDeclarationSaveExit[Either[LandfillTaxDetailsDeclaration, Map[String, String]]] = TaxFormDeclarationSaveExit.nameLater(declarationRepository)

  def landfillTaxFormDeclarationDisplay(registrationNumber: String) = Action.async { implicit request =>
    val form = LandfillTaxDetailsDeclaration.form
    // Stub
      Future.successful[Either[Unit, Either[LandfillTaxDetailsDeclarationPersistence, Map[String, String]]]](Left(():Unit)).flatMap {
//    RetrieveDeclarationService.retrieve(registrationNumber).flatMap {
      case x: Either[Unit, Either[LandfillTaxDetailsDeclarationPersistence, Map[String, String]]] => {
        x match {
          case Right(Left(obj)) => {
            println("Right(list)")
            val formData: LandfillTaxDetailsDeclarationPersistence = obj
            val filledForm = new LandfillTaxDetailsDeclaration(formData.registrationNumber.value,
              "",
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
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(formFilled, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case Right(Right(obj)) => {
            val localDateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy").withLocale(Locale.UK)
            println("Right(obj)")
            val formData: Map[String, String] = obj
            val accountPeriodStartDate =
              formData("accountingPeriodStartDate") match {
                case "" => LocalDate.MIN
                case s => LocalDate.parse(s, localDateFormatter)
              }

            val accountPeriodEndDate = {
              formData("accountingPeriodEndDate") match {
                case "" => LocalDate.MIN
                case s => LocalDate.parse(s, localDateFormatter)
              }
            }

            val taxCreditClaimedForEnvironment: BigDecimal = {
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

            val filledForm = new LandfillTaxDetailsDeclaration(formData("registrationNumber"),
              "",
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
              Seq(EnvironmentalBody(formData("environmentalBody1[1].bodyName"), environmentalBodyAmount)),
              Some(formData("emailAddress")),
              Some(formData("confirmEmailAddress")))
            val formFilled = form.fill(filledForm)
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(formFilled, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case Left(()) => {
            println("Unit")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(form, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case _ => {
            println("Blank")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(form, registrationNumber.filter(Character.isLetterOrDigit))))
          }
        }
      }
      case _ => {
        println("Unit")
        Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(form, registrationNumber.filter(Character.isLetterOrDigit))))
      }
    }
  }

  def landfillTaxFormDeclaration(rn: String) = landfillTaxDeclaration(rn)

  private def landfillTaxDeclaration[A](registrationNumber: String) = Action.async { implicit request =>
    LandfillTaxDetailsDeclaration.form.bindFromRequest.fold(
      error => {
        println(error.data)
        val right: Right[LandfillTaxDetailsDeclaration, Map[String, String]] = Right(error.data)
        // Stub
//        declarationRepository.store(right)
        Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(error, registrationNumber)))
      },
      content => {
        println(content)
        if (content.save.equals("Exit")) {
          // Stub
//          DeclarationSaveExit.declarationSaveForm(Left(content))(x) map {
//            case false => Ok("Failed")
//            case true => Redirect(routes.LandfillTax.landfillTaxDisplay(registrationNumber))
//          }
          Future.successful(Redirect(routes.LandfillTax.landfillTaxDisplay(registrationNumber)))
        } else if (content.save.equals("Continue")) {
          // Stub
//          TaxFormDeclarationSubmission.submitTaxForm(content).flatMap {
//            case DeclarationSubmissionResult(Some(errorMessage), _) =>
//              val formWithErrors = LandfillTaxDetailsDeclaration.form.withGlobalError(errorMessage)
//              Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form_declaration(formWithErrors, registrationNumber)))
//            case DeclarationSubmissionResult(noErrors, Some(submissionAcknowledgement)) =>
//              DeclarationSaveExit.declarationSaveForm(Left(content))(x) map {
//                case false => Ok("Failed")
//                case true => Redirect(routes.LandfillTaxConfirmation.landfillTaxConfirmationDisplay(registrationNumber, submissionAcknowledgement))
//              }
//          }
          Future.successful(Redirect(routes.LandfillTaxConfirmation.landfillTaxConfirmationDisplay(registrationNumber, "XXX")))
        } else {
          Future.successful(Ok("Failed"))
        }
      }
    )
  }

}
