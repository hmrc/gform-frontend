package uk.gov.hmrc.bforms.models


import org.apache.commons.lang3.RandomStringUtils
import org.joda.time.LocalDate

/**
  * Created by daniel-connelly on 05/01/17.
  */
case class LandFillTaxDetailsPersistence(ID : String = RandomStringUtils.random(4),
                                         firstName : FirstName = new FirstName(""),
                                         lastName : LastName = new LastName(""),
                                         telephoneNumber: TelephoneNumber = new TelephoneNumber(""),
                                         status: Status = new Status(""),
                                         nameOfBusiness: NameOfBusiness = new NameOfBusiness(""),
                                         accountingPeriodStartDate: AccountingPeriodStartDate = new AccountingPeriodStartDate(new LocalDate()),
                                         accountingPeriodEndDate: AccountingPeriodEndDate = new AccountingPeriodEndDate(new LocalDate()),
                                         taxDueForThisPeriod: TaxDueForThisPeriod = new TaxDueForThisPeriod(""),
                                         underDeclarationsFromPreviousPeriod: UnderDeclarationsFromPreviousPeriod = new UnderDeclarationsFromPreviousPeriod(""),
                                         overDeclarationsForThisPeriod: OverDeclarationsForThisPeriod = new OverDeclarationsForThisPeriod(""),
                                         taxCreditClaimedForEnvironment: TaxCreditClaimedForEnvironment = new TaxCreditClaimedForEnvironment(""),
                                         badDebtReliefClaimed: BadDebtReliefClaimed = new BadDebtReliefClaimed(""),
                                         otherCredits: OtherCredits = new OtherCredits(""),
                                         standardRateWaste: StandardRateWaste = new StandardRateWaste(""),
                                         lowerRateWaste: LowerRateWaste = new LowerRateWaste(""),
                                         exemptWaste: ExemptWaste = new ExemptWaste(""),
                                         environmentalBody1: EnvironmentalBody1 = new EnvironmentalBody1(""),
                                         environmentalBody2: EnvironmentalBody2 = new EnvironmentalBody2(Some("")),
                                         emailAddress: EmailAddress = new EmailAddress(Some("")),
                                         confirmEmailAddress: ConfirmEmailAddress = new ConfirmEmailAddress(Some("")),
                                         datePersisted : LocalDate = new LocalDate
                                        ){

}

class FirstName(val value:String) extends AnyVal
class LastName(val value:String) extends AnyVal
class TelephoneNumber(val value:String) extends AnyVal
class Status (val value:String) extends AnyVal
class NameOfBusiness(val value:String) extends AnyVal
class AccountingPeriodStartDate(val value:LocalDate) extends AnyVal
class AccountingPeriodEndDate(val value:LocalDate) extends AnyVal
class TaxDueForThisPeriod(val value:String) extends AnyVal
class UnderDeclarationsFromPreviousPeriod(val value:String) extends AnyVal
class OverDeclarationsForThisPeriod(val value:String) extends AnyVal
class TaxCreditClaimedForEnvironment(val value:String) extends AnyVal
class BadDebtReliefClaimed(val value:String) extends AnyVal
class OtherCredits(val value:String) extends AnyVal
class StandardRateWaste(val value:String) extends AnyVal
class LowerRateWaste(val value:String) extends AnyVal
class ExemptWaste(val value:String) extends AnyVal
class EnvironmentalBody1(val value:String) extends AnyVal
class EnvironmentalBody2(val value:Option[String]) extends AnyVal
class EmailAddress(val value:Option[String]) extends AnyVal
class ConfirmEmailAddress(val value:Option[String]) extends AnyVal