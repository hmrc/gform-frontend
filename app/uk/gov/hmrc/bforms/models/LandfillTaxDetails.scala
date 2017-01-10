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

package uk.gov.hmrc.bforms.models

import java.time.LocalDate

import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json

case class environmentalBody(bodyName: String, amount: String)

object environmentalBody{
  implicit val formats = Json.format[environmentalBody]
}

case class LandfillTaxDetails(
                              save:String,
                               firstName: String,
                               lastName: String,
                               telephoneNumber: String,
                               status: String,
                               nameOfBusiness: String,
                               accountingPeriodStartDate: LocalDate,
                               accountingPeriodEndDate: LocalDate,
                               taxDueForThisPeriod: String,
                               underDeclarationsFromPreviousPeriod: String,
                               overDeclarationsForThisPeriod: String,
                               taxCreditClaimedForEnvironment: String,
                               badDebtReliefClaimed: String,
                               otherCredits: String,
                               standardRateWaste: String,
                               lowerRateWaste: String,
                               exemptWaste: String,
                               environmentalBody1: Seq[environmentalBody],
                               emailAddress: Option[String],
                               confirmEmailAddress: Option[String]
                             )

object LandfillTaxDetails {
  implicit val formats = Json.format[LandfillTaxDetails]

  val form = Form(mapping(
    "save" -> nonEmptyText,
    "firstName" -> nonEmptyText,
    "lastName" -> nonEmptyText,
    "telephoneNumber" -> nonEmptyText(minLength = 3),
    "status" -> nonEmptyText,
    "nameOfBusiness" -> nonEmptyText,
    "accountingPeriodStartDate" -> localDate("dd/MM/yyyy"),
    "accountingPeriodEndDate" -> localDate("dd/MM/yyyy"),
    "taxDueForThisPeriod" -> nonEmptyText,
    "underDeclarationsFromPreviousPeriod" -> nonEmptyText,
    "overDeclarationsForThisPeriod" -> nonEmptyText,
    "taxCreditClaimedForEnvironment" -> nonEmptyText,
    "badDebtReliefClaimed" -> nonEmptyText,
    "otherCredits" -> nonEmptyText,
    "standardRateWaste" -> nonEmptyText,
    "lowerRateWaste" -> nonEmptyText,
    "exemptWaste" -> nonEmptyText,
    "environmentalBody1" -> seq(
      mapping(
      "bodyName" -> nonEmptyText,
        "amount" -> nonEmptyText
    )(environmentalBody.apply)(environmentalBody.unapply)),
    "emailAddress" -> optional(text),
    "confirmEmailAddress" -> optional(text)
  )(LandfillTaxDetails.apply)(LandfillTaxDetails.unapply))
}
