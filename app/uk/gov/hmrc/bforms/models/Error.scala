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

import uk.gov.hmrc.bforms.models.persistence._

class Error() {

}

object Error {
  def apply(errorData :Map[String, String]) = {
    val bob = errorData.map{
      case x  => x._1 match {
        case "firstName" => FirstName(x._2)
        case "lastName" => LastName(x._2)
        case "telephoneNumber" => TelephoneNumber(x._2)
        case "status" => Status(x._2)
        case "nameOfBuissness" => NameOfBusiness(x._2)
        case "accountingPeriodStartDate" => (x._2)
        case "accountingPeriodEndDate" => (x._2)
        case "taxDueForThisPeriod" => TaxDueForThisPeriod(x._2)
        case "underDeclarationForThisPeriod" => UnderDeclarationsFromPreviousPeriod(x._2)
        case "overDeclarationForThisPeriod" => OverDeclarationsForThisPeriod(x._2)
        case "taxCreditClaimedForEnvironment" => TaxCreditClaimedForEnvironment(0)
        case "badDebtReliefClaimed" => BadDebtReliefClaimed(x._2)
        case "otherCredits" => println(x._2)
        case "standardRateWaste" => println(x._2)
        case "lowerRateWaste" => println(x._2)
        case "exemptWaste" => println(x._2)
        case "environmentalBody1" => println(x._2)
        case "environmentalBody2" => println(x._2)
        case "emailAddress" => println(x._2)
        case "confirmEmailAddress" => println(x._2)
        case "registrationNumber" => println(x._2)
        case _ => println(x._1+"->"+x._2)
      }
      case _ => println("Nothing")
    }
    println(bob)
  }
}
