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

package uk.gov.hmrc.bforms.service

import java.time.LocalDate

import play.api.libs.json._
import play.api.mvc.Action
import uk.gov.hmrc.bforms.connectors.{BformsConnector, VerificationResult}
import uk.gov.hmrc.bforms.repositories.LandFillTaxRepository
import uk.gov.hmrc.bforms.models.{EnvironmentalBody, LandfillTaxDetails}
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.parsing.json.JSONObject

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

/**
  * Created by daniel-connelly on 06/01/17.
  */
trait TaxFormSaveExit[A] {
  def apply(a: A): Future[Either[String,Unit]]
}

object TaxFormSaveExit {

  private def getTaxFormSaveExit[A](f: A => Future[Either[String, Unit]]) : TaxFormSaveExit[A] = {
    new TaxFormSaveExit[A] {
      def apply(params: A) : Future[Either[String, Unit]] = f(params)
    }
  }

  implicit def nameLater(implicit repository: LandFillTaxRepository): TaxFormSaveExit[Either[LandfillTaxDetails, Map[String, String]]] = {
    getTaxFormSaveExit((r : Either[LandfillTaxDetails, Map[String, String]]) =>  repository.store(r))
  }
}

object SaveExit {

  def bformsConnector : BformsConnector = BformsConnector

  implicit val environmentalBodyWriter : Writes[EnvironmentalBody] = (
    (__ \ "name").write[String] and
      (__ \ "amount").write[BigDecimal]
    )(unlift(EnvironmentalBody.unapply))

  val landfillTaxDetailsWriter : Writes[LandfillTaxDetails] = (
      (JsPath \ (0) \ "value").write[String] and
      (JsPath \ (1) \ "value").write[String] and
      (JsPath \ (2) \ "value").write[String] and
      (JsPath \ (3) \ "value").write[String] and
      (JsPath \ (4) \ "value").write[String] and
      (JsPath \ (5) \ "value").write[String] and
      ((JsPath \ "fields") (6) \ "value").write[String] and
      ((JsPath \ "fields") (7) \ "value").write[LocalDate] and
      ((JsPath \ "fields") (8) \ "value").write[LocalDate] and
      ((JsPath \ "fields") (9) \ "value").write[String] and
      ((JsPath \ "fields") (10) \ "value").write[String] and
      ((JsPath \ "fields") (11) \ "value").write[String] and
      ((JsPath \ "fields") (12) \ "value").write[BigDecimal] and
      ((JsPath \ "fields") (13) \ "value").write[String] and
      ((JsPath \ "fields") (14) \ "value").write[String] and
      ((JsPath \ "fields") (15) \ "value").write[String] and
      ((JsPath \ "fields") (16) \ "value").write[String] and
      ((JsPath \ "fields") (17) \ "value").write[String] and
      ((JsPath \ "fields") (18) \ "value").write[Seq[EnvironmentalBody]] and
      ((JsPath \ "fields") (19) \ "value").writeNullable[String] and
      ((JsPath \ "fields") (20) \ "value").writeNullable[String]
    )(unlift(LandfillTaxDetails.unapply))

//  implicit val schemaFormat = (
//    (JsPath \ "formTypeId").json.put(JsString("LF100")) and
//    (JsPath \ "version").json.put(JsString("0.1.0")) and
//    (JsPath \ "characterSet").json.put(JsString("UTF-8")) and
//      (JsPath \ "fields").write[LandfillTaxDetails]
//    )


  def saveToBackEndFormWithErrors(formDetails : Map[String, String])(implicit hc : HeaderCarrier) : Future[VerificationResult] = {
    val jsonobject = Json.obj("formTypeId" -> "LF100",
      "version" -> "0.1.0",
      "characterSet" -> "UTF-8",
      "fields" -> Json.arr(
        Json.obj("id" -> "registrationNumber", "value" -> formDetails("registratioNumber")),
        Json.obj("id" -> "firstName", "value" -> formDetails("firstName")),
        Json.obj("id" -> "secondName", "value" -> formDetails("lastName")),
        Json.obj("id" -> "telephoneNumber", "value" -> formDetails("telephoneNumber")),
        Json.obj("id" -> "status", "value" -> formDetails("status")),
        Json.obj("id" -> "NameOfBusiness", "value" -> formDetails("nameOfBusiness")),
        Json.obj("id" -> "accoutingPeriodStartDate", "value" -> formDetails("accountingPeriodStartDate")),
        Json.obj("id" -> "accoutingPeriodEndDate", "value" -> formDetails("accountingPeriodEndDate")),
        Json.obj("id" -> "taxDueForThisPeriod", "value" -> formDetails("taxDueForThisPeriod")),
        Json.obj("id" -> "underDeclarationsFromPreviousPeriod", "value" -> formDetails("underDeclarationsFromPreviousPeriod")),
        Json.obj("id" -> "overDeclarationsForThisPeriod", "value" -> formDetails("overDeclarationsForThisPeriod")),
        Json.obj("id" -> "taxCreditClaimedForEnvironment", "value" -> formDetails("taxCreditClaimedForEnvironment")),
        Json.obj("id" -> "badDebtReliefClaimed", "value" -> formDetails("badDebtReliefClaimed")),
        Json.obj("id" -> "otherCredits", "value" -> formDetails("otherCredits")),
        Json.obj("id" -> "standardRateWaste", "value" -> formDetails("standardRateWaste")),
        Json.obj("id" -> "LowerRateWaste", "value" -> formDetails("lowerRateWaste")),
        Json.obj("id" -> "exemptWaste", "value" -> formDetails("exemptWaste")),
        Json.obj("id" -> "environmentalBodies", "value" -> Json.obj(
          "name" -> formDetails("environmentalBody1[0].bodyName"),
          "amount" -> formDetails("environmentalBody1[0].amount"))),
        Json.obj("id" -> "emailAddress", "value" -> formDetails("emailAddress")),
        Json.obj("id" -> "confirmEmailAddress", "value" -> formDetails("confirmEmailAddress"))
      )
    )
    bformsConnector.saveForm(jsonobject)
  }

  def saveToBackEnd[A](formDetails: LandfillTaxDetails)(implicit hc : HeaderCarrier) : Future[VerificationResult] = {
    val formInfo = implicitly[OWrites[LandfillTaxDetails]].writes(formDetails).value.map {case(key, jsvalue) =>
      val json = jsvalue match {
        case JsString(str) => str
        case JsNumber(num) => num.toString
        case JsObject(obj) => obj.toString
        case others => others.toString
      }

      Json.obj("id" -> key, "value" -> json)}.toList

    val jsonobject : JsObject = Json.obj("formTypeId" -> "LF100",
      "version" -> "0.1.0",
      "characterSet" -> "UTF-8",
      "fields" -> Json.toJson(formInfo
//        Json.obj("id" -> , "value" -> formDetails.registrationNumber),
//        Json.obj("id" -> "firstName", "value" -> formDetails.firstName),
//        Json.obj("id" -> "secondName", "value" -> formDetails.lastName),
//        Json.obj("id" -> "telephoneNumber", "value" -> formDetails.telephoneNumber),
//        Json.obj("id" -> "status", "value" -> formDetails.status),
//        Json.obj("id" -> "NameOfBusiness", "value" -> formDetails.nameOfBusiness),
//        Json.obj("id" -> "accoutingPeriodStartDate", "value" -> formDetails.accountingPeriodStartDate),
//        Json.obj("id" -> "accoutingPeriodEndDate", "value" -> formDetails.accountingPeriodEndDate),
//        Json.obj("id" -> "taxDueForThisPeriod", "value" -> formDetails.taxDueForThisPeriod),
//        Json.obj("id" -> "underDeclarationsFromPreviousPeriod", "value" -> formDetails.underDeclarationsFromPreviousPeriod),
//        Json.obj("id" -> "overDeclarationsForThisPeriod", "value" -> formDetails.overDeclarationsForThisPeriod),
//        Json.obj("id" -> "taxCreditClaimedForEnvironment", "value" -> formDetails.taxCreditClaimedForEnvironment),
//        Json.obj("id" -> "badDebtReliefClaimed", "value" -> formDetails.badDebtReliefClaimed),
//        Json.obj("id" -> "otherCredits", "value" -> formDetails.otherCredits),
//        Json.obj("id" -> "standardRateWaste", "value" -> formDetails.standardRateWaste),
//        Json.obj("id" -> "LowerRateWaste", "value" -> formDetails.lowerRateWaste),
//        Json.obj("id" -> "exemptWaste", "value" -> formDetails.exemptWaste),
//        Json.obj("id" -> "environmentalBodies", "value" -> Json.obj(
//          "name" -> formDetails.environmentalBodies(0).bodyName,
//          "amount" -> formDetails.environmentalBodies(0).amount)),
//        Json.obj("id" -> "emailAddress", "value" -> formDetails.emailAddress),
//        Json.obj("id" -> "confirmEmailAddress", "value" -> formDetails.confirmEmailAddress))
    ))
    bformsConnector.saveForm(jsonobject)
  }

  def SaveForm[A, B](formDetails:Either[A, B])(implicit taxFormSaveExit:TaxFormSaveExit[Either[A, B]]):Future[Boolean] = {
    taxFormSaveExit(formDetails).map {
      case _ => true
      case x => false
    }
  }
}
