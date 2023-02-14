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

package uk.gov.hmrc.gform.sharedmodel

import julienrf.json.derived
import play.api.libs.json.{ Format, JsArray, JsValue, Json, OFormat, Reads, Writes }
import uk.gov.hmrc.gform.eval.ExprType
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, JsonUtils }

case class DataRetrieveId(value: String) extends AnyVal {
  def withIndex(index: Int): DataRetrieveId =
    DataRetrieveId(index + "_" + value)
}

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)
}

sealed trait DataRetrieveAttribute extends Product with Serializable {
  def name: String

  def `type`: ExprType
}

case object DataRetrieveAttribute {
  import uk.gov.hmrc.gform.eval.ExprType.{ number, string }
  case object IsValid extends DataRetrieveAttribute {
    override def name: String = "isValid"
    override def `type`: ExprType = string
  }

  case object Iban extends DataRetrieveAttribute {
    override def name: String = "iban"
    override def `type`: ExprType = string
  }

  case object AccountNumberIsWellFormatted extends DataRetrieveAttribute {
    override def name: String = "accountNumberIsWellFormatted"
    override def `type`: ExprType = string
  }

  case object SortCodeIsPresentOnEISCD extends DataRetrieveAttribute {
    override def name: String = "sortCodeIsPresentOnEISCD"
    override def `type`: ExprType = string
  }

  case object SortCodeBankName extends DataRetrieveAttribute {
    override def name: String = "sortCodeBankName"
    override def `type`: ExprType = string
  }

  case object NonStandardAccountDetailsRequiredForBacs extends DataRetrieveAttribute {
    override def name: String = "nonStandardAccountDetailsRequiredForBacs"
    override def `type`: ExprType = string
  }

  case object AccountExists extends DataRetrieveAttribute {
    override def name: String = "accountExists"
    override def `type`: ExprType = string
  }

  case object NameMatches extends DataRetrieveAttribute {
    override def name: String = "nameMatches"
    override def `type`: ExprType = ExprType.string
  }

  case object SortCodeSupportsDirectDebit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectDebit"
    override def `type`: ExprType = string
  }

  case object SortCodeSupportsDirectCredit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectCredit"
    override def `type`: ExprType = string
  }

  case object Name extends DataRetrieveAttribute {
    override def name: String = "name"
    override def `type`: ExprType = string
  }

  case object Status extends DataRetrieveAttribute {
    override def name: String = "status"
    override def `type`: ExprType = string
  }

  case object RegisteredAddress extends DataRetrieveAttribute {
    override def name: String = "registeredAddress"
    override def `type`: ExprType = string
  }

  case object RiskScore extends DataRetrieveAttribute {
    override def name: String = "riskScore"
    override def `type`: ExprType = number
  }

  case object Reason extends DataRetrieveAttribute {
    override def name: String = "reason"
    override def `type`: ExprType = string
  }

  case object AccountName extends DataRetrieveAttribute {
    override def name: String = "accountName"
    override def `type`: ExprType = string
  }

  case object EmployerName extends DataRetrieveAttribute {
    override def name: String = "employerName"
    override def `type`: ExprType = string
  }

  case object SequenceNumber extends DataRetrieveAttribute {
    override def name: String = "sequenceNumber"
    override def `type`: ExprType = number
  }

  case object WorksNumber extends DataRetrieveAttribute {
    override def name: String = "worksNumber"
    override def `type`: ExprType = string
  }

  case object TaxDistrictNumber extends DataRetrieveAttribute {
    override def name: String = "taxDistrictNumber"
    override def `type`: ExprType = string
  }

  case object PayeNumber extends DataRetrieveAttribute {
    override def name: String = "payeNumber"
    override def `type`: ExprType = string
  }

  case object Director extends DataRetrieveAttribute {
    override def name: String = "director"
    override def `type`: ExprType = string // Boolean
  }

  implicit val format: OFormat[DataRetrieveAttribute] = derived.oformat()

  def fromName(name: String): DataRetrieveAttribute = name match {
    case "isValid"                                  => IsValid
    case "accountNumberIsWellFormatted"             => AccountNumberIsWellFormatted
    case "sortCodeIsPresentOnEISCD"                 => SortCodeIsPresentOnEISCD
    case "sortCodeBankName"                         => SortCodeBankName
    case "nonStandardAccountDetailsRequiredForBacs" => NonStandardAccountDetailsRequiredForBacs
    case "accountExists"                            => AccountExists
    case "nameMatches"                              => NameMatches
    case "sortCodeSupportsDirectDebit"              => SortCodeSupportsDirectDebit
    case "sortCodeSupportsDirectCredit"             => SortCodeSupportsDirectCredit
    case "iban"                                     => Iban
    case "name"                                     => Name
    case "status"                                   => Status
    case "registeredAddress"                        => RegisteredAddress
    case "riskScore"                                => RiskScore
    case "reason"                                   => Reason
    case "accountName"                              => AccountName
    case "employerName"                             => EmployerName
    case "sequenceNumber"                           => SequenceNumber
    case "worksNumber"                              => WorksNumber
    case "taxDistrictNumber"                        => TaxDistrictNumber
    case "payeNumber"                               => PayeNumber
    case "director"                                 => Director
    case other                                      => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute name: $other")
  }
}

sealed trait DataRetrieve {
  def id: DataRetrieveId
  def attributes: List[DataRetrieveAttribute]
}

object DataRetrieve {

  def requestParamsFromCache(
    form: Form,
    dataRetrieveId: DataRetrieveId
  ): Option[JsValue] = form.thirdPartyData.dataRetrieve.flatMap(
    _.get(dataRetrieveId).map(_.requestParams)
  )

  final case class ValidateBankDetails(override val id: DataRetrieveId, sortCode: Expr, accountNumber: Expr)
      extends DataRetrieve {
    override def attributes: List[DataRetrieveAttribute] = List(
      DataRetrieveAttribute.IsValid,
      DataRetrieveAttribute.SortCodeIsPresentOnEISCD,
      DataRetrieveAttribute.SortCodeBankName,
      DataRetrieveAttribute.NonStandardAccountDetailsRequiredForBacs
    )
  }

  final case class BusinessBankAccountExistence(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    companyName: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      SortCodeIsPresentOnEISCD,
      SortCodeBankName,
      NonStandardAccountDetailsRequiredForBacs,
      AccountExists,
      NameMatches,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit
    )
  }

  final case class CompanyRegistrationNumber(
    override val id: DataRetrieveId,
    companyNumber: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      Name,
      Status,
      RegisteredAddress
    )
  }

  final case class NinoInsights(
    override val id: DataRetrieveId,
    nino: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._

    override def attributes: List[DataRetrieveAttribute] = List(
      RiskScore,
      Reason
    )
  }

  final case class BankAccountInsights(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._
    override def attributes: List[DataRetrieveAttribute] = List(
      RiskScore,
      Reason
    )
  }

  final case class PersonalBankAccountExistenceWithName(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    name: Expr
  ) extends DataRetrieve {
    import DataRetrieveAttribute._

    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      AccountExists,
      NameMatches,
      AccountName,
      NonStandardAccountDetailsRequiredForBacs,
      SortCodeIsPresentOnEISCD,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit,
      SortCodeBankName,
      Iban
    )
  }

  final case class PersonalBankAccountExistence(
    override val id: DataRetrieveId,
    sortCode: Expr,
    accountNumber: Expr,
    firstName: Expr,
    lastName: Expr
  ) extends DataRetrieve {

    import DataRetrieveAttribute._

    override def attributes: List[DataRetrieveAttribute] = List(
      AccountNumberIsWellFormatted,
      AccountExists,
      NameMatches,
      AccountName,
      NonStandardAccountDetailsRequiredForBacs,
      SortCodeIsPresentOnEISCD,
      SortCodeSupportsDirectDebit,
      SortCodeSupportsDirectCredit,
      SortCodeBankName,
      Iban
    )
  }

  final case class Employments(
    override val id: DataRetrieveId,
    nino: Expr,
    taxYear: Expr
  ) extends DataRetrieve {

    import DataRetrieveAttribute._

    override def attributes: List[DataRetrieveAttribute] = List(
      EmployerName,
      SequenceNumber,
      WorksNumber,
      TaxDistrictNumber,
      PayeNumber,
      Director
    )
  }

  implicit val format: OFormat[DataRetrieve] = derived.oformat()
}

sealed trait RetrieveDataType extends Product with Serializable {
  def size: Int = this match {
    case RetrieveDataType.ObjectType(_) => 1
    case RetrieveDataType.ListType(xs)  => xs.size
  }
}

object RetrieveDataType {
  case class ObjectType(data: Map[DataRetrieveAttribute, String]) extends RetrieveDataType
  case class ListType(data: List[Map[DataRetrieveAttribute, String]]) extends RetrieveDataType
}

case class DataRetrieveResult(
  id: DataRetrieveId,
  data: RetrieveDataType,
  requestParams: JsValue // Request data used to decide if new call to the API is need when input data are changing
)

object DataRetrieveResult {

  implicit val dataRetrieveSuccessDataFormat: OFormat[Map[DataRetrieveAttribute, String]] =
    implicitly[OFormat[Map[String, String]]]
      .bimap[Map[DataRetrieveAttribute, String]](
        _.map { case (key, value) =>
          DataRetrieveAttribute.fromName(key) -> value
        },
        _.map { case (key, value) =>
          key.name -> value
        }
      )
  implicit val retrieveDataTypeFormat: Format[RetrieveDataType] = {

    val reads: Reads[RetrieveDataType] = Reads {
      case a: JsArray =>
        implicitly[Reads[List[Map[DataRetrieveAttribute, String]]]].reads(a).map(RetrieveDataType.ListType)
      case other => implicitly[Reads[Map[DataRetrieveAttribute, String]]].reads(other).map(RetrieveDataType.ObjectType)
    }

    val writes: Writes[RetrieveDataType] = Writes[RetrieveDataType] {

      case RetrieveDataType.ObjectType(data) => Json.toJson(data)
      case RetrieveDataType.ListType(data)   => Json.toJson(data)
    }

    Format[RetrieveDataType](reads, writes)
  }
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
