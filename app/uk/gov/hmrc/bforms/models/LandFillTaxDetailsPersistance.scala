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

import play.api.libs.json.{Format, JsError, JsResult, JsString, JsSuccess, JsValue, Json, OFormat, _}
import org.apache.commons.lang3.RandomStringUtils
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import play.data.format.Formats.DateFormatter

/**
  * Created by daniel-connelly on 05/01/17.
  */
case class LandFillTaxDetailsPersistence(ID : String = RandomStringUtils.random(4),
                                         firstName : FirstName = new FirstName(""),
                                         lastName : LastName = new LastName(""),
                                         telephoneNumber: TelephoneNumber = new TelephoneNumber(""),
                                         status: Status = new Status(""),
                                         nameOfBusiness: NameOfBusiness = new NameOfBusiness(""),
                                         accountingPeriodStartDate: LocalDate = LocalDate.now,
                                         accountingPeriodEndDate: LocalDate = LocalDate.now,
                                         taxDueForThisPeriod: TaxDueForThisPeriod = new TaxDueForThisPeriod(""),
                                         underDeclarationsFromPreviousPeriod: UnderDeclarationsFromPreviousPeriod = new UnderDeclarationsFromPreviousPeriod(""),
                                         overDeclarationsForThisPeriod: OverDeclarationsForThisPeriod = new OverDeclarationsForThisPeriod(""),
                                         taxCreditClaimedForEnvironment: TaxCreditClaimedForEnvironment = new TaxCreditClaimedForEnvironment(""),
                                         badDebtReliefClaimed: BadDebtReliefClaimed = new BadDebtReliefClaimed(""),
                                         otherCredits: OtherCredits = new OtherCredits(""),
                                         standardRateWaste: StandardRateWaste = new StandardRateWaste(""),
                                         lowerRateWaste: LowerRateWaste = new LowerRateWaste(""),
                                         exemptWaste: ExemptWaste = new ExemptWaste(""),
                                         environmentalBody1: Seq[environmentalBody] =  Seq(new environmentalBody("" , "")),
                                         emailAddress: EmailAddress = new EmailAddress(Some("")),
                                         confirmEmailAddress: ConfirmEmailAddress = new ConfirmEmailAddress(Some("")),
                                         datePersisted : LocalDate = LocalDate.now
                                        ){
}

class FirstName(val value:String) extends AnyVal
class LastName(val value:String) extends AnyVal
class TelephoneNumber(val value:String) extends AnyVal
class Status (val value:String) extends AnyVal
class NameOfBusiness(val value:String) extends AnyVal
//class AccountingPeriodStartDate(val value:LocalDate) extends AnyVal
//class AccountingPeriodEndDate(val value:LocalDate) extends AnyVal
class TaxDueForThisPeriod(val value:String) extends AnyVal
class UnderDeclarationsFromPreviousPeriod(val value:String) extends AnyVal
class OverDeclarationsForThisPeriod(val value:String) extends AnyVal
class TaxCreditClaimedForEnvironment(val value:String) extends AnyVal
class BadDebtReliefClaimed(val value:String) extends AnyVal
class OtherCredits(val value:String) extends AnyVal
class StandardRateWaste(val value:String) extends AnyVal
class LowerRateWaste(val value:String) extends AnyVal
class ExemptWaste(val value:String) extends AnyVal
class EmailAddress(val value:Option[String]) extends AnyVal
class ConfirmEmailAddress(val value:Option[String]) extends AnyVal

object FirstName {
  def apply(value: String) = new FirstName(value)

  implicit val format : Format[FirstName] = ValueClassFormat.format(FirstName.apply)(_.value)
}

object LastName {
  def apply(value:String) = new LastName(value)

  implicit val format : Format[LastName] = ValueClassFormat.format(LastName.apply)(_.value)

}

object TelephoneNumber {
  def apply(value: String) = new TelephoneNumber(value)

  implicit val format : Format[TelephoneNumber] = ValueClassFormat.format(TelephoneNumber.apply)(_.value)
}

object Status {
  def apply(value: String) = new Status(value)

  implicit val format : Format[Status] = ValueClassFormat.format(Status.apply)(_.value)
}

object NameOfBusiness {
  def apply(value: String) = new NameOfBusiness(value)

  implicit val format : Format[NameOfBusiness] = ValueClassFormat.format(NameOfBusiness.apply)(_.value)
}

//object AccountingPeriodStartDate {
//  def apply(value: LocalDate) = new AccountingPeriodStartDate(value)
//
//  implicit val format : Format[AccountingPeriodStartDate] = ValueClassFormatLocalDate.format(AccountingPeriodStartDate.apply)(_.value)
//}
//
//object AccountingPeriodEndDate {
//  def apply(value: LocalDate) = new AccountingPeriodEndDate(value)
//
//  implicit val format : Format[AccountingPeriodEndDate] =  ValueClassFormatLocalDate.format(AccountingPeriodEndDate.apply)(_.value)
//}

object TaxDueForThisPeriod {
  def apply(value: String) = new TaxDueForThisPeriod(value)

  implicit val format : Format[TaxDueForThisPeriod] = ValueClassFormat.format(TaxDueForThisPeriod.apply)(_.value)
}

object UnderDeclarationsFromPreviousPeriod {
  def apply(value: String) = new UnderDeclarationsFromPreviousPeriod(value)

  implicit val format : Format[UnderDeclarationsFromPreviousPeriod] = ValueClassFormat.format(UnderDeclarationsFromPreviousPeriod.apply)(_.value)
}

object OverDeclarationsForThisPeriod {
  def apply(value: String) = new OverDeclarationsForThisPeriod(value)

  implicit val format : Format[OverDeclarationsForThisPeriod] = ValueClassFormat.format(OverDeclarationsForThisPeriod.apply)(_.value)
}

object TaxCreditClaimedForEnvironment {
  def apply(value: String) = new TaxCreditClaimedForEnvironment(value)

  implicit val format : Format[TaxCreditClaimedForEnvironment] = ValueClassFormat.format(TaxCreditClaimedForEnvironment.apply)(_.value)
}

object BadDebtReliefClaimed {
  def apply(value: String) = new BadDebtReliefClaimed(value)

  implicit val format : Format[BadDebtReliefClaimed] = ValueClassFormat.format(BadDebtReliefClaimed.apply)(_.value)
}

object OtherCredits {
  def apply(value: String) = new OtherCredits(value)

  implicit val format : Format[OtherCredits] = ValueClassFormat.format(OtherCredits.apply)(_.value)
}

object StandardRateWaste {
  def apply(value: String) = new StandardRateWaste(value)

  implicit val format : Format[StandardRateWaste] = ValueClassFormat.format(StandardRateWaste.apply)(_.value)
}

object LowerRateWaste {
  def apply(value: String) = new LowerRateWaste(value)

  implicit val format : Format[LowerRateWaste] = ValueClassFormat.format(LowerRateWaste.apply)(_.value)
}

object ExemptWaste {
  def apply(value: String) = new ExemptWaste(value)

  implicit val format : Format[ExemptWaste] = ValueClassFormat.format(ExemptWaste.apply)(_.value)
}




object EmailAddress {
  def apply(value: String) = new EmailAddress(Some(value))

  implicit val format : Format[EmailAddress] = ValueClassFormat.format(EmailAddress.apply)(_.value.getOrElse("None"))
}

object ConfirmEmailAddress {
  def apply(value: String) = new ConfirmEmailAddress(Some(value))

  implicit val format : Format[ConfirmEmailAddress] = ValueClassFormat.format(ConfirmEmailAddress.apply)(_.value.getOrElse("None"))
}

object ValueClassFormat {
  def format[A: Format](fromStringToA: String => A)(fromAToString: A => String) = {
    new Format[A] {
      def reads(json: JsValue): JsResult[A] = {
        json match {
          case JsString(str) => JsSuccess(fromStringToA(str))
          case unknown => JsError(s"JsString value expected, got: $unknown")
        }
      }
      def writes(a: A): JsValue = JsString(fromAToString(a))
    }
  }
}

object ValueClassFormatLocalDate {
  def format[A: Format](fromDateToA: LocalDate => A)(fromAToDate: A => LocalDate) = {
    new Format[LocalDate] {
      override def reads(json: JsValue) : JsResult[LocalDate]= json.validate[String].map(LocalDate.parse)

      override def writes(a: LocalDate): JsValue = Json.toJson(a.toString)
    }
  }
}