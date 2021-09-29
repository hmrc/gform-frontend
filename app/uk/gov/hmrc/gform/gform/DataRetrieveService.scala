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

package uk.gov.hmrc.gform.gform

import org.slf4j.{ Logger, LoggerFactory }
import play.api.i18n.Messages
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector.{ Account, AccountNumber, SortCode, ValidateBankDetailsRequest }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds

sealed trait DataRetrieveService[T <: DataRetrieve, F[_]] {
  def retrieve(dataRetrieve: T, formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser])(implicit
    hc: HeaderCarrier,
    messages: Messages,
    ex: ExecutionContext
  ): F[DataRetrieveResult]
}

object DataRetrieveService {
  implicit def validateBankDetailsInstant(implicit
    bankAccountReputationConnector: BankAccountReputationConnector[Future]
  ): DataRetrieveService[ValidateBank, Future] = new DataRetrieveService[ValidateBank, Future] {
    private val logger: Logger = LoggerFactory.getLogger(getClass)

    override def retrieve(
      validateBankDetails: ValidateBank,
      formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
    )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[DataRetrieveResult] = {
      val accNumber =
        formModelVisibilityOptics.evalAndApplyTypeInfoFirst(validateBankDetails.accountNumber)
      val sortCode =
        formModelVisibilityOptics.evalAndApplyTypeInfoFirst(validateBankDetails.sortCode)

      if (accNumber.isEmpty || sortCode.isEmpty) {
        Future.successful(DataRetrieveMissingInput)
      } else {
        bankAccountReputationConnector
          .validateBankDetails(
            ValidateBankDetailsRequest(
              Account(SortCode(sortCode.stringRepresentation), AccountNumber(accNumber.stringRepresentation))
            )
          )
          .map { validateResult =>
            DataRetrieveSuccess(
              validateBankDetails.id,
              Map(
                DataRetrieveIsValid -> validateResult.accountNumberWithSortCodeIsValid
              )
            )
          }
          .recover { case e =>
            logger.error(s"Failed to retrieve data for validateBankDetails, with id ${validateBankDetails.id}", e)
            DataRetrieveFailed
          }
      }
    }
  }

  def apply[T <: DataRetrieve, F[_]](implicit d: DataRetrieveService[T, F]) = d
}
