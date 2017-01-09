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

package uk.gov.hmrc.bforms.repositories

import java.time.LocalDate
import javax.inject.Inject

import com.google.inject.Singleton
import play.api.libs.json.{Format, Json}
import reactivemongo.api.DB
import uk.gov.hmrc.bforms.models.{AccountingPeriodEndDate, AccountingPeriodStartDate, BadDebtReliefClaimed, ConfirmEmailAddress, EmailAddress, EnvironmentalBody1, EnvironmentalBody2, ExemptWaste, FirstName, LandFillTaxDetailsPersistence, LandfillTaxDetails, LastName, LowerRateWaste, NameOfBusiness, OtherCredits, OverDeclarationsForThisPeriod, StandardRateWaste, Status, TaxCreditClaimedForEnvironment, TaxDueForThisPeriod, TelephoneNumber, UnderDeclarationsFromPreviousPeriod}
import uk.gov.hmrc.mongo.ReactiveRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by daniel-connelly on 22/12/16.
  */
@Singleton
class LandFillTaxRepository @Inject()(db:DB) extends ReactiveRepository[LandFillTaxDetailsPersistence, String]("formData", () => db, LandFillTaxDetailsPersistence.mongoFormat, implicitly[Format[String]]) with LandFillTaxRepo {

  def store(form: LandfillTaxDetails) = {
    val store = LandFillTaxDetailsPersistence("Something" , FirstName(form.firstName), LastName(form.lastName), TelephoneNumber(form.telephoneNumber),
      Status(form.status),
      NameOfBusiness(form.nameOfBusiness),
      AccountingPeriodStartDate(form.accountingPeriodStartDate),
      AccountingPeriodEndDate(form.accountingPeriodEndDate),
      TaxDueForThisPeriod(form.taxDueForThisPeriod),
      UnderDeclarationsFromPreviousPeriod(form.underDeclarationsFromPreviousPeriod),
      OverDeclarationsForThisPeriod(form.overDeclarationsForThisPeriod),
      TaxCreditClaimedForEnvironment(form.taxCreditClaimedForEnvironment),
      BadDebtReliefClaimed(form.badDebtReliefClaimed),
      OtherCredits(form.otherCredits),
      StandardRateWaste(form.standardRateWaste),
      lowerRateWaste: String,
      exemptWaste: String,
      environmentalBody1: String,
      environmentalBody2: Option[String],
      emailAddress: Option[String],
      confirmEmailAddress: Option[String])
       insert(store) map {
         case r if r.ok =>
           logger.info(s"form with details of '${form.firstName}' & '${form.lastName}' was successfully stored")
           Right(())
         case r =>
           logger.error(s"form with details of '${form.firstName}' & '${form.lastName}' was not successfully stored")
           Left(r.message)
       }
  }

  def get() = { ??? }
}

object LandFillTaxDetailsPersistence {

  def apply(bleh : String,
            firstName:FirstName,
            lastName:LastName,
            telephoneNumber:TelephoneNumber,
            status: Status,
            nameOfBusiness: NameOfBusiness,
            accountingPeriodStartDate: AccountingPeriodStartDate,
            accountingPeriodEndDate: AccountingPeriodEndDate,
            taxDueForThisPeriod: TaxDueForThisPeriod,
            underDeclarationsFromPreviousPeriod: UnderDeclarationsFromPreviousPeriod,
            overDeclarationsForThisPeriod: OverDeclarationsForThisPeriod,
            taxCreditClaimedForEnvironment: TaxCreditClaimedForEnvironment,
            badDebtReliefClaimed: BadDebtReliefClaimed,
            otherCredits: OtherCredits,
            standardRateWaste: StandardRateWaste,
            lowerRateWaste: LowerRateWaste,
            exemptWaste: ExemptWaste,
            environmentalBody1: EnvironmentalBody1,
            environmentalBody2: EnvironmentalBody2,
            emailAddress: EmailAddress,
            confirmEmailAddress: ConfirmEmailAddress) = {
    new LandFillTaxDetailsPersistence(bleh,
      firstName,
      lastName,
      telephoneNumber,
      status,
      nameOfBusiness,
      accountingPeriodStartDate,
      accountingPeriodEndDate,
      taxDueForThisPeriod,
      underDeclarationsFromPreviousPeriod,
      overDeclarationsForThisPeriod,
      taxCreditClaimedForEnvironment,
      badDebtReliefClaimed,
      otherCredits,
      standardRateWaste,
      lowerRateWaste,
      exemptWaste,
      environmentalBody1,
      environmentalBody2,
      emailAddress,
      confirmEmailAddress)
  }

  val mongoFormat = Json.format[LandFillTaxDetailsPersistence]
}

trait LandFillTaxRepo {

  def store(form : LandfillTaxDetails) : Future[Either[String, Unit]]

  def get() : Future[Either[String, Unit]]
}

