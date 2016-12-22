/*
 * Copyright 2016 HM Revenue & Customs
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

import play.api.libs.json._

/**
  * Created by daniel-connelly on 22/12/16.
  */

case class LandfillTaxDetailsForm(firstName:FirstName, secondName : SecondName)

// does this just turn the case class into a json format ?
object LandFillTaxDetailsParser {

//  def splitUpFormData[A](something:Seq[A]): LandfillTaxDetailsForm = {
//  }

}
object LandfillTaxDetailsForm {

  implicit val oFormat : OFormat[LandfillTaxDetailsForm] = Json.format[LandfillTaxDetailsForm]
}

class FirstName(val value:String) extends AnyVal
class SecondName(val value:String) extends AnyVal
class RegistrationNumber(val value:String) extends AnyVal
class TelephoneNumber(val value:String) extends AnyVal
class Status(val value:String) extends AnyVal
class NameOfBusiness (val value:String) extends AnyVal
class AccountingPeriodStartDate(val value:LocalDate) extends AnyVal
class AccountPeriodEndDate(val value:LocalDate) extends AnyVal
class TaxDueForThisPeriod(val value:String) extends AnyVal
class UnderDeclarationForThisPeriod(val value:String) extends AnyVal
class OverDeclarationForThisPeriod(val value:String) extends AnyVal
class TaxCreditClaimedForEnvironment(val value:String) extends AnyVal

class badDebtReliefClaimed(val value:String) extends AnyVal
class otherCredits(val value:String) extends AnyVal
class standardRateWaste(val value:String) extends AnyVal
class lowerRateWaste(val value:String) extends AnyVal
class exemptWaste(val value:String) extends AnyVal
class environmentalBody1(val value:String) extends AnyVal
class environmentalBody2(val value:Option[String]) extends AnyVal
class emailAddress(val value:Option[String]) extends AnyVal
class confirmEmailAddress(val value:Option[String]) extends AnyVal

object FirstName {

  def apply(value:String) = new FirstName(value)

  implicit val format:Format[FirstName] = ValueClassFormat.format(FirstName.apply)(_.value)
}

object SecondName {

}

object RegistrationNumber {

  def apply(value:String) = new RegistrationNumber(value)

  implicit val format:Format[RegistrationNumber] = ValueClassFormat.format(RegistrationNumber.apply)(_.value)
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
