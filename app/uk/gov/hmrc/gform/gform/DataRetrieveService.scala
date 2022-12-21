/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.i18n.Messages
import play.api.libs.json.{ JsValue, Json }
import uk.gov.hmrc.gform.api.{ CompanyInformationConnector, CompanyProfile }
import uk.gov.hmrc.gform.bars
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BusinessBankAccountExistence, CompanyRegistrationNumber, ValidateBankDetails }
import uk.gov.hmrc.gform.sharedmodel._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds

sealed trait DataRetrieveService[T <: DataRetrieve, F[_]] {
  def retrieve(
    dataRetrieve: T,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
    maybeRequestParams: Option[JsValue]
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    ex: ExecutionContext
  ): F[Option[DataRetrieveResult]]
}

object DataRetrieveService {
  implicit def validateBankDetailsInstant(implicit
    bankAccountReputationConnector: BankAccountReputationConnector[Future]
  ): DataRetrieveService[ValidateBankDetails, Future] = new DataRetrieveService[ValidateBankDetails, Future] {

    override def retrieve(
      validateBankDetails: ValidateBankDetails,
      formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
      maybeRequestParams: Option[JsValue]
    )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
      val accNumber =
        formModelVisibilityOptics.evalAndApplyTypeInfoFirst(validateBankDetails.accountNumber).stringRepresentation
      val sortCode =
        formModelVisibilityOptics.evalAndApplyTypeInfoFirst(validateBankDetails.sortCode).stringRepresentation

      val requestParams = Json.obj(
        "accountNumber" -> accNumber,
        "sortCode"      -> sortCode
      )
      if (accNumber.isEmpty || sortCode.isEmpty || maybeRequestParams.contains(requestParams)) {
        Future.successful(None)
      } else {
        bankAccountReputationConnector
          .validateBankDetails(
            bars.ValidateBankDetails.create(sortCode, accNumber)
          )
          .map {
            case ServiceResponse(validateResult) =>
              Some(
                DataRetrieveResult(
                  validateBankDetails.id,
                  Map(
                    DataRetrieveAttribute.IsValid                                  -> validateResult.accountNumberIsWellFormatted,
                    DataRetrieveAttribute.SortCodeIsPresentOnEISCD                 -> validateResult.sortCodeIsPresentOnEISCD,
                    DataRetrieveAttribute.SortCodeBankName                         -> validateResult.sortCodeBankName.getOrElse(""),
                    DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs -> validateResult.nonStandardAccountDetailsRequiredForBacs,
                    DataRetrieveAttribute.SortCodeSupportsDirectDebit -> validateResult.sortCodeSupportsDirectDebit
                      .getOrElse(""),
                    DataRetrieveAttribute.SortCodeSupportsDirectCredit -> validateResult.sortCodeSupportsDirectCredit
                      .getOrElse(""),
                    DataRetrieveAttribute.Iban -> validateResult.iban.getOrElse("")
                  ),
                  requestParams
                )
              )
            // We need to fail the journey here, otherwise expressions like
            // ${dataRetrieve.bankDetails.isValid='no'} etc.
            // will always evaluate to false
            case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve ValidateBankDetails data")
          }
      }
    }
  }

  implicit def businessBankAccountExistence(implicit
    bankAccountReputationConnector: BankAccountReputationConnector[Future]
  ): DataRetrieveService[BusinessBankAccountExistence, Future] =
    new DataRetrieveService[BusinessBankAccountExistence, Future] {

      override def retrieve(
        businessBankAccountExistence: BusinessBankAccountExistence,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val accNumber =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(businessBankAccountExistence.accountNumber)
            .stringRepresentation
        val sortCode =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(businessBankAccountExistence.sortCode)
            .stringRepresentation
        val companyName =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(businessBankAccountExistence.companyName)
            .stringRepresentation

        val requestParams = Json.obj(
          "accountNumber" -> accNumber,
          "sortCode"      -> sortCode,
          "companyName"   -> companyName
        )

        if (
          accNumber.isEmpty || sortCode.isEmpty || companyName.isEmpty || maybeRequestParams.contains(requestParams)
        ) {
          Future.successful(None)
        } else {
          bankAccountReputationConnector
            .businessBankAccountExistence(
              bars.BusinessBankAccountExistence.create(sortCode, accNumber, companyName)
            )
            .map {
              case ServiceResponse(result) =>
                Some(
                  DataRetrieveResult(
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
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound =>
                // We need to fail the journey here, otherwise expressions like
                // ${dataRetrieve.businessBankDetails.accountNumberIsWellFormatted='no'} etc.
                // will always evaluate to false
                throw new Exception("Cannot retrieve BusinessBankAccountExistence data")
            }
        }
      }
    }

  implicit def companyProfileInstant(implicit
    companyInformationConnector: CompanyInformationConnector[Future]
  ): DataRetrieveService[CompanyRegistrationNumber, Future] =
    new DataRetrieveService[CompanyRegistrationNumber, Future] {

      override def retrieve(
        companyRegistrationNumber: CompanyRegistrationNumber,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val companyNumber =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(companyRegistrationNumber.companyNumber)
            .stringRepresentation

        val requestParams = Json.obj(
          "companyNumber" -> companyNumber
        )

        if (companyNumber.isEmpty) {
          Future.successful(None)
        } else {
          companyInformationConnector
            .companyProfile(
              CompanyProfile.create(companyNumber)
            )
            .map {
              case ServiceResponse(result) =>
                Some(
                  DataRetrieveResult(
                    companyRegistrationNumber.id,
                    Map(
                      DataRetrieveAttribute.Name   -> result.name,
                      DataRetrieveAttribute.Status -> result.status.getOrElse(""),
                      DataRetrieveAttribute.RegisteredAddress -> CompanyProfile
                        .createFullRegisteredAddress(result.registeredAddress)
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve CompanyProfile data")
            }
        }
      }
    }

  def apply[T <: DataRetrieve, F[_]](implicit d: DataRetrieveService[T, F]) = d
}
