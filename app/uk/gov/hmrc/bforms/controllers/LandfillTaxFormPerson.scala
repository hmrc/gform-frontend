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

import java.time.format.DateTimeFormatter
import java.util.Locale
import javax.inject.{Inject, Singleton}

import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.Action
import uk.gov.hmrc.bforms.models.persistence.LandfillTaxDetailsPersonPersistence
import uk.gov.hmrc.bforms.models.LandfillTaxDetailsPerson
import uk.gov.hmrc.bforms.service._
import uk.gov.hmrc.play.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}


@Singleton
class LandfillTaxFormPerson @Inject()(val messagesApi: MessagesApi)(implicit ec: ExecutionContext)
  extends FrontendController with I18nSupport {

//  implicit val repo : LandfillTaxRepository = LandfillTaxRepository.apply(db

//  implicit val y : TaxFormPersonRetrieve[String, LandfillTaxDetailsPersonPersistence, Map[String, String]] = TaxFormPersonRetrieve.somethingElse(personRepository)
//  implicit val x : TaxFormPersonSaveExit[Either[LandfillTaxDetailsPerson, Map[String, String]]] = TaxFormPersonSaveExit.nameLater(personRepository)

  def landfillTaxFormPersonDisplay(registrationNumber : String) = Action.async { implicit request =>
    val form = LandfillTaxDetailsPerson.form
    Future.successful[Either[Unit, Either[LandfillTaxDetailsPersonPersistence, Map[String, String]]]](Left(():Unit)).flatMap {
      // STub
//    RetrievePersonService.retrieve(registrationNumber).flatMap {
      case x : Either[Unit, Either[LandfillTaxDetailsPersonPersistence, Map[String, String]]] => {
        x match {
          case Right(Left(obj)) => {
            println("Right(list)")
            val formData : LandfillTaxDetailsPersonPersistence = obj
            val filledForm = new LandfillTaxDetailsPerson(formData.registrationNumber.value,
              "",
              formData.firstName.value,
              formData.lastName.value,
              formData.telephoneNumber.value,
              formData.status.value)
            val formFilled = form.fill(filledForm)
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(formFilled, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case Right(Right(obj)) => {
            val localDateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy").withLocale(Locale.UK)
            println("Right(obj)")
            val formData : Map[String, String] = obj
            val filledForm = new LandfillTaxDetailsPerson(formData("registrationNumber"),
              "",
              formData("firstName"),
              formData("lastName"),
              formData("telephoneNumber"),
              formData("status"))
            val formFilled = form.fill(filledForm)
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(formFilled, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case Left(()) => {
            println("Unit")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(form, registrationNumber.filter(Character.isLetterOrDigit))))
          }
          case _ => {
            println("Blank")
            Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(form, registrationNumber.filter(Character.isLetterOrDigit))))
          }
        }
      }
      case _ => {
        println("Unit")
        Future.successful(Ok(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(form, registrationNumber.filter(Character.isLetterOrDigit))))
      }
    }
  }

  def landfillTaxFormPerson(rn: String) = landfillTaxPerson(rn)

  private def landfillTaxPerson[A](registrationNumber : String) = Action.async { implicit request =>
      LandfillTaxDetailsPerson.form.bindFromRequest.fold(
        error => {
          println(error.data)
          val right: Right[LandfillTaxDetailsPerson, Map[String, String]] = Right(error.data)
          // Stub
//          personRepository.store(right)
          Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(error, registrationNumber)))
        },
        content => {
          println(content)
          if (content.save.equals("Exit")) {
            // Stub
//            PersonSaveExit.personSaveForm(Left(content))(x) map {
//              case false => Ok("Failed")
//              case true => Redirect(routes.LandfillTax.landfillTaxDisplay(registrationNumber))
//            }
            Future.successful(Redirect(routes.LandfillTax.landfillTaxDisplay(registrationNumber)))

        } else if(content.save.equals("Continue")) {
            // Stub
//            TaxFormPersonSubmission.submitTaxForm(content).flatMap {
//              case PersonSubmissionResult(Some(errorMessage), _) =>
//                val formWithErrors = LandfillTaxDetailsPerson.form.withGlobalError(errorMessage)
//                Future.successful(BadRequest(uk.gov.hmrc.bforms.views.html.landfill_tax_form_person(formWithErrors, registrationNumber)))
//              case PersonSubmissionResult(noErrors, Some(submissionAcknowledgement)) =>
//                PersonSaveExit.personSaveForm(Left(content))(x) map {
//                  case false => Ok("Failed")
//                  case true => Redirect(routes.LandfillTaxFormDeclaration.landfillTaxFormDeclarationDisplay(registrationNumber))
//                }
//            }
    Future.successful(Redirect(routes.LandfillTaxFormDeclaration.landfillTaxFormDeclarationDisplay(registrationNumber)))

  } else {
            Future.successful(Ok("Failed"))
          }
        }
      )
  }

}
