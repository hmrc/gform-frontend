/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.gform.bars

import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson

trait BankAccountReputationConnector[F[_]] {
  def validateBankDetails(account: ValidateBankDetails.Request)(implicit
    hc: HeaderCarrier
  ): F[ValidateBankDetails.Response]
  def businessBankAccountExistence(account: BusinessBankAccountExistence.Request)(implicit
    hc: HeaderCarrier
  ): F[BusinessBankAccountExistence.Response]
}

class BankAccountReputationAsyncConnector(ws: WSHttp, baseUrl: String)(implicit ex: ExecutionContext)
    extends BankAccountReputationConnector[Future] {
  override def validateBankDetails(request: ValidateBankDetails.Request)(implicit
    hc: HeaderCarrier
  ): Future[ValidateBankDetails.Response] =
    ws.POST[ValidateBankDetails.Request, ValidateBankDetails.Response](
      baseUrl + "/v2/validateBankDetails",
      request
    )

  override def businessBankAccountExistence(request: BusinessBankAccountExistence.Request)(implicit
    hc: HeaderCarrier
  ): Future[BusinessBankAccountExistence.Response] =
    ws.POST[BusinessBankAccountExistence.Request, BusinessBankAccountExistence.Response](
      baseUrl + "/verify/business",
      request
    )
}

object ValidateBankDetails {

  def create(
    sortCode: String,
    accountNumber: String
  ) = Request(Account(sortCode, accountNumber))

  case class Account(
    sortCode: String, // The bank sort code, 6 characters long (whitespace and/or dashes should be removed)
    accountNumber: String // The bank account number, 8 characters long
  )

  object Account {
    implicit val format: Format[Account] = Json.format[Account]
  }

  case class Request(
    account: Account
  )

  object Request {
    implicit val format: Format[Request] = Json.format[Request]
  }

  case class Response(
    accountNumberWithSortCodeIsValid: String,
    nonStandardAccountDetailsRequiredForBacs: String,
    sortCodeIsPresentOnEISCD: String,
    supportsBACS: Option[String],
    ddiVoucherFlag: Option[String],
    directDebitsDisallowed: Option[String],
    directDebitInstructionsDisallowed: Option[String],
    iban: Option[String],
    sortCodeBankName: Option[String]
  )

  object Response {
    implicit val format: Format[Response] = Json.format[Response]
  }
}

object BusinessBankAccountExistence {

  def create(
    sortCode: String,
    accountNumber: String,
    companyName: String
  ) = Request(Account(sortCode, accountNumber), Some(Business(companyName, None, None)))

  case class Account(
    sortCode: String, // The bank sort code, 6 characters long (whitespace and/or dashes should be removed)
    accountNumber: String // The bank account number, 8 characters long
  )

  object Account {
    implicit val format: Format[Account] = Json.format[Account]
  }

  case class Address(
    lines: List[String], // One to four lines; cumulative length must be between 1 and 140 characters.
    town: Option[String], // Must be between 1 and 35 characters long
    postcode: Option[String]
  )

  object Address {
    implicit val format: Format[Address] = Json.format[Address]
  }

  case class Business(
    companyName: String, // Must be between 1 and 70 characters long
    companyRegistrationNumber: Option[String],
    address: Option[Address]
  )

  object Business {
    implicit val format: Format[Business] = Json.format[Business]
  }

  case class Request(
    account: Account,
    business: Option[Business]
  )

  object Request {
    implicit val format: Format[Request] = Json.format[Request]
  }
  case class Response(
    accountNumberIsWellFormatted: String,
    sortCodeIsPresentOnEISCD: String,
    sortCodeBankName: Option[String],
    nonStandardAccountDetailsRequiredForBacs: String,
    accountExists: String,
    nameMatches: String,
    sortCodeSupportsDirectDebit: String,
    sortCodeSupportsDirectCredit: String
  )

  object Response {
    implicit val format: Format[Response] = Json.format[Response]
  }
}
