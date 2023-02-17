/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.gform.api.{ BankAccountInsightCheck, BankAccountInsightsConnector, CompanyInformationConnector, CompanyProfile, NinoInsightCheck, NinoInsightsConnector }
import uk.gov.hmrc.gform.bars
import uk.gov.hmrc.gform.bars.BankAccountReputationConnector
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.{ BankAccountInsights, BusinessBankAccountExistence, CompanyRegistrationNumber, Employments, NinoInsights, PersonalBankAccountExistence, PersonalBankAccountExistenceWithName, ValidateBankDetails }
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
                  RetrieveDataType.ObjectType(
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
                    )
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
                    RetrieveDataType.ObjectType(
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

  implicit def personalBankAccountExistence(implicit
    bankAccountReputationConnector: BankAccountReputationConnector[Future]
  ): DataRetrieveService[PersonalBankAccountExistence, Future] =
    new DataRetrieveService[PersonalBankAccountExistence, Future] {

      override def retrieve(
        personalBankAccountExistence: PersonalBankAccountExistence,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val accNumber =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.accountNumber)
            .stringRepresentation
        val sortCode =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.sortCode)
            .stringRepresentation
        val firstName =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.firstName)
            .stringRepresentation
        val lastName =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.lastName)
            .stringRepresentation

        val requestParams = Json.obj(
          "accountNumber" -> accNumber,
          "sortCode"      -> sortCode,
          "firstName"     -> firstName,
          "lastName"      -> lastName
        )

        if (
          accNumber.isEmpty || sortCode.isEmpty || firstName.isEmpty || lastName.isEmpty || maybeRequestParams
            .contains(requestParams)
        ) {
          Future.successful(None)
        } else {
          bankAccountReputationConnector
            .personalBankAccountExistence(
              bars.PersonalBankAccountExistence.create(sortCode, accNumber, firstName, lastName)
            )
            .map {
              case ServiceResponse(result) =>
                Some(
                  DataRetrieveResult(
                    personalBankAccountExistence.id,
                    RetrieveDataType.ObjectType(
                      Map(
                        DataRetrieveAttribute.AccountNumberIsWellFormatted             -> result.accountNumberIsWellFormatted,
                        DataRetrieveAttribute.AccountExists                            -> result.accountExists,
                        DataRetrieveAttribute.NameMatches                              -> result.nameMatches,
                        DataRetrieveAttribute.AccountName                              -> result.accountName.getOrElse(""),
                        DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs -> result.nonStandardAccountDetailsRequiredForBacs,
                        DataRetrieveAttribute.SortCodeIsPresentOnEISCD                 -> result.sortCodeIsPresentOnEISCD,
                        DataRetrieveAttribute.SortCodeSupportsDirectDebit              -> result.sortCodeSupportsDirectDebit,
                        DataRetrieveAttribute.SortCodeSupportsDirectCredit             -> result.sortCodeSupportsDirectCredit,
                        DataRetrieveAttribute.SortCodeBankName                         -> result.sortCodeBankName.getOrElse(""),
                        DataRetrieveAttribute.Iban                                     -> result.iban.getOrElse("")
                      )
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound =>
                // We need to fail the journey here, otherwise expressions like
                // ${dataRetrieve.personalBankAccountExistence.accountNumberIsWellFormatted='no'} etc.
                // will always evaluate to false
                throw new Exception("Cannot retrieve PersonalBankAccountExistence data")
            }
        }
      }
    }

  implicit def personalBankAccountExistenceWithName(implicit
    bankAccountReputationConnector: BankAccountReputationConnector[Future]
  ): DataRetrieveService[PersonalBankAccountExistenceWithName, Future] =
    new DataRetrieveService[PersonalBankAccountExistenceWithName, Future] {

      override def retrieve(
        personalBankAccountExistence: PersonalBankAccountExistenceWithName,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val accNumber =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.accountNumber)
            .stringRepresentation
        val sortCode =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.sortCode)
            .stringRepresentation
        val name =
          formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(personalBankAccountExistence.name)
            .stringRepresentation
        val requestParams = Json.obj("accountNumber" -> accNumber, "sortCode" -> sortCode, "name" -> name)

        if (
          accNumber.isEmpty || sortCode.isEmpty || name.isEmpty || maybeRequestParams
            .contains(requestParams)
        ) {
          Future.successful(None)
        } else {
          bankAccountReputationConnector
            .personalBankAccountExistence(
              bars.PersonalBankAccountExistence.createWithName(sortCode, accNumber, name)
            )
            .map {
              case ServiceResponse(result) =>
                Some(
                  DataRetrieveResult(
                    personalBankAccountExistence.id,
                    RetrieveDataType.ObjectType(
                      Map(
                        DataRetrieveAttribute.AccountNumberIsWellFormatted             -> result.accountNumberIsWellFormatted,
                        DataRetrieveAttribute.AccountExists                            -> result.accountExists,
                        DataRetrieveAttribute.NameMatches                              -> result.nameMatches,
                        DataRetrieveAttribute.AccountName                              -> result.accountName.getOrElse(""),
                        DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs -> result.nonStandardAccountDetailsRequiredForBacs,
                        DataRetrieveAttribute.SortCodeIsPresentOnEISCD                 -> result.sortCodeIsPresentOnEISCD,
                        DataRetrieveAttribute.SortCodeSupportsDirectDebit              -> result.sortCodeSupportsDirectDebit,
                        DataRetrieveAttribute.SortCodeSupportsDirectCredit             -> result.sortCodeSupportsDirectCredit,
                        DataRetrieveAttribute.SortCodeBankName                         -> result.sortCodeBankName.getOrElse(""),
                        DataRetrieveAttribute.Iban                                     -> result.iban.getOrElse("")
                      )
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound =>
                // We need to fail the journey here, otherwise expressions like
                // ${dataRetrieve.personalBankAccountExistence.accountNumberIsWellFormatted='no'} etc.
                // will always evaluate to false
                throw new Exception("Cannot retrieve PersonalBankAccountExistenceWithName data")
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
                    RetrieveDataType.ObjectType(
                      Map(
                        DataRetrieveAttribute.Name   -> result.name,
                        DataRetrieveAttribute.Status -> result.status.getOrElse(""),
                        DataRetrieveAttribute.RegisteredAddress -> CompanyProfile
                          .createFullRegisteredAddress(result.registeredAddress)
                      )
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve CompanyProfile data")
            }
        }
      }
    }

  implicit def ninoInsights(implicit
    ninoInsightsConnector: NinoInsightsConnector[Future]
  ): DataRetrieveService[NinoInsights, Future] =
    new DataRetrieveService[NinoInsights, Future] {

      override def retrieve(
        ninoInsights: NinoInsights,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val nino = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(ninoInsights.nino).stringRepresentation

        val requestParams = Json.obj(
          "nino" -> nino
        )

        if (nino.isEmpty) {
          Future.successful(None)
        } else {
          ninoInsightsConnector
            .insights(
              NinoInsightCheck.create(nino)
            )
            .map {
              case ServiceResponse(result) =>
                Some(
                  DataRetrieveResult(
                    ninoInsights.id,
                    RetrieveDataType.ObjectType(
                      Map(
                        DataRetrieveAttribute.RiskScore -> result.riskScore.toString,
                        DataRetrieveAttribute.Reason    -> result.reason
                      )
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve nino insights data")
            }
        }
      }
    }

  implicit def bankAccountInsights(implicit
    bankAccountInsightsConnector: BankAccountInsightsConnector[Future]
  ): DataRetrieveService[BankAccountInsights, Future] =
    new DataRetrieveService[BankAccountInsights, Future] {

      override def retrieve(
        bankAccountInsights: BankAccountInsights,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val sortCode =
          formModelVisibilityOptics.evalAndApplyTypeInfoFirst(bankAccountInsights.sortCode).stringRepresentation.trim
        val accountNumber = formModelVisibilityOptics
          .evalAndApplyTypeInfoFirst(bankAccountInsights.accountNumber)
          .stringRepresentation
          .trim

        val requestParams = Json.obj(
          "sortCode"      -> sortCode,
          "accountNumber" -> accountNumber
        )

        if (sortCode.isEmpty || accountNumber.isEmpty) {
          Future.successful(None)
        } else {
          bankAccountInsightsConnector
            .insights(
              BankAccountInsightCheck.create(sortCode, accountNumber)
            )
            .map {
              case ServiceResponse(result) =>
                Some(
                  DataRetrieveResult(
                    bankAccountInsights.id,
                    RetrieveDataType.ObjectType(
                      Map(
                        DataRetrieveAttribute.RiskScore -> result.riskScore.toString,
                        DataRetrieveAttribute.Reason    -> result.reason
                      )
                    ),
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve bankAccount insights data")
            }
        }
      }
    }

  implicit def employments(implicit
    gformConnector: GformConnector
  ): DataRetrieveService[Employments, Future] =
    new DataRetrieveService[Employments, Future] {

      override def retrieve(
        employments: Employments,
        formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser],
        maybeRequestParams: Option[JsValue]
      )(implicit hc: HeaderCarrier, messages: Messages, ex: ExecutionContext): Future[Option[DataRetrieveResult]] = {
        val nino: String = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(employments.nino).stringRepresentation
        val taxYear = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(employments.taxYear).numberRepresentation

        val requestParams = Json.obj(
          "nino"    -> nino,
          "taxYear" -> taxYear
        )

        if (nino.isEmpty || taxYear.isEmpty) {
          Future.successful(None)
        } else {
          gformConnector
            .getEmployments(nino, taxYear.get.toInt)
            .map {
              case ServiceResponse(result) =>
                val retrieveDataType = RetrieveDataType.ListType(result.map { r =>
                  Map(
                    DataRetrieveAttribute.SequenceNumber    -> r.sequenceNumber.toString,
                    DataRetrieveAttribute.EmployerName      -> r.employerName.toString,
                    DataRetrieveAttribute.WorksNumber       -> r.worksNumber.toString,
                    DataRetrieveAttribute.TaxDistrictNumber -> r.taxDistrictNumber.toString,
                    DataRetrieveAttribute.PayeNumber        -> r.payeNumber.toString,
                    DataRetrieveAttribute.Director          -> r.director.toString
                  )
                })
                Some(
                  DataRetrieveResult(
                    employments.id,
                    retrieveDataType,
                    requestParams
                  )
                )
              case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve employments data")
            }
        }
      }
    }

  def apply[T <: DataRetrieve, F[_]](implicit d: DataRetrieveService[T, F]) = d
}
