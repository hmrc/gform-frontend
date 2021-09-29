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
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector.{ ValidateBankDetailsRequest, ValidateBankDetailsResponse }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.JsonUtils
import uk.gov.hmrc.gform.wshttp.WSHttp
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds
import uk.gov.hmrc.http.HttpReads.Implicits.readFromJson

trait BankAccountReputationConnector[F[_]] {
  def validateBankDetails(account: ValidateBankDetailsRequest)(implicit
    hc: HeaderCarrier
  ): F[ValidateBankDetailsResponse]
}

class BankAccountReputationAsyncConnector(ws: WSHttp, baseUrl: String)(implicit ex: ExecutionContext)
    extends BankAccountReputationConnector[Future] {
  override def validateBankDetails(request: ValidateBankDetailsRequest)(implicit
    hc: HeaderCarrier
  ): Future[ValidateBankDetailsResponse] =
    ws.POST[ValidateBankDetailsRequest, ValidateBankDetailsResponse](
      baseUrl + s"/validateBankDetails",
      request,
      Seq(("User-Agent", "gforms"), ("Content-Type", "application/json")) ++ hc.requestId.map(r =>
        ("X-Tracking-Id", r.value)
      )
    )
}

object BankAccountReputationConnector {

  case class AccountNumber(value: String) extends AnyVal
  object AccountNumber {
    implicit val format: Format[AccountNumber] =
      JsonUtils.valueClassFormat[AccountNumber, String](AccountNumber.apply, _.value)
  }

  case class SortCode(value: String) extends AnyVal
  object SortCode {
    implicit val format: Format[SortCode] = JsonUtils.valueClassFormat[SortCode, String](SortCode.apply, _.value)
  }

  case class Account(
    sortCode: SortCode, // The bank sort code, 6 characters long (whitespace and/or dashes should be removed)
    accountNumber: AccountNumber // The bank account number, 8 characters long
  )

  object Account {
    implicit val format: Format[Account] = Json.format[Account]
  }

  case class ValidateBankDetailsRequest(
    account: Account
  )

  object ValidateBankDetailsRequest {
    implicit val format: Format[ValidateBankDetailsRequest] = Json.format[ValidateBankDetailsRequest]
  }

  case class ValidateBankDetailsResponse(
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

  object ValidateBankDetailsResponse {
    implicit val format: Format[ValidateBankDetailsResponse] = Json.format[ValidateBankDetailsResponse]
  }
}
