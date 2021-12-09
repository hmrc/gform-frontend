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
import uk.gov.hmrc.gform.bars
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, ValidateBankDetails }
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
  ): DataRetrieveService[ValidateBankDetails, Future] = new DataRetrieveService[ValidateBankDetails, Future] {
    private val logger: Logger = LoggerFactory.getLogger(getClass)

    override def retrieve(
      validateBankDetails: ValidateBankDetails,
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
            bars.ValidateBankDetails.create(
              sortCode.stringRepresentation,
              accNumber.stringRepresentation
            )
          )
          .map { validateResult =>
            DataRetrieveSuccess(
              validateBankDetails.id,
              Map(
                DataRetrieveAttribute.IsValid -> validateResult.accountNumberWithSortCodeIsValid
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

  implicit def businessBankAccountExistence(implicit
    bankAccountReputationConnector: BankAccountReputationConnector[Future]
  ): DataRetrieveService[BusinessBankAccountExistence, Future] =
    new DataRetrieveService[BusinessBankAccountExistence, Future] {
      private val logger: Logger = LoggerFactory.getLogger(getClass)

      override def retrieve(
        businessBankAccountExistence: BusinessBankAccountExistence,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[DataRetrieveResult] = {
        val accNumber =
          formModelVisibilityOptics.evalAndApplyTypeInfoFirst(businessBankAccountExistence.accountNumber)
        val sortCode =
          formModelVisibilityOptics.evalAndApplyTypeInfoFirst(businessBankAccountExistence.sortCode)
        val companyName =
          formModelVisibilityOptics.evalAndApplyTypeInfoFirst(businessBankAccountExistence.companyName)

        if (accNumber.isEmpty || sortCode.isEmpty || companyName.isEmpty) {
          Future.successful(DataRetrieveMissingInput)
        } else {
          bankAccountReputationConnector
            .businessBankAccountExistence(
              bars.BusinessBankAccountExistence.create(
                sortCode.stringRepresentation,
                accNumber.stringRepresentation,
                companyName.stringRepresentation
              )
            )
            .map { result =>
              DataRetrieveSuccess(
                businessBankAccountExistence.id,
                Map(
                  DataRetrieveAttribute.AccountNumberIsWellFormatted             -> result.accountNumberIsWellFormatted,
                  DataRetrieveAttribute.SortCodeIsPresentOnEISCD                 -> result.sortCodeIsPresentOnEISCD,
                  DataRetrieveAttribute.SortCodeBankName                         -> result.sortCodeBankName.getOrElse(""),
                  DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs -> result.nonStandardAccountDetailsRequiredForBacs,
                  DataRetrieveAttribute.AccountExists                            -> result.accountExists,
                  DataRetrieveAttribute.NameMatches                              -> result.nameMatches,
                  DataRetrieveAttribute.SortCodeSupportsDirectDebit              -> result.sortCodeSupportsDirectDebit,
                  DataRetrieveAttribute.SortCodeSupportsDirectCredit             -> result.sortCodeSupportsDirectCredit
                )
              )
            }
            .recover { case e =>
              logger.error(
                s"Failed to retrieve data for businessBankAccountExistence, with id ${businessBankAccountExistence.id}",
                e
              )
              DataRetrieveFailed
            }
        }
      }
    }

  def apply[T <: DataRetrieve, F[_]](implicit d: DataRetrieveService[T, F]) = d
}
