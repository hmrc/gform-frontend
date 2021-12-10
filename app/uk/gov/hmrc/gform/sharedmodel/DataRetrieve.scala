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

package uk.gov.hmrc.gform.sharedmodel

import julienrf.json.derived
import play.api.libs.json.{ Format, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Expr, JsonUtils }

case class DataRetrieveId(value: String) extends AnyVal {
  def withIndex(index: Int): DataRetrieveId =
    DataRetrieveId(index + "_" + value)
}

object DataRetrieveId {
  implicit val format: Format[DataRetrieveId] =
    JsonUtils.valueClassFormat[DataRetrieveId, String](DataRetrieveId.apply, _.value)
}

sealed trait DataRetrieveAttribute {
  def name: String
}

case object DataRetrieveAttribute {

  case object IsValid extends DataRetrieveAttribute {
    override def name: String = "isValid"
  }

  case object AccountNumberIsWellFormatted extends DataRetrieveAttribute {
    override def name: String = "accountNumberIsWellFormatted"
  }

  case object SortCodeIsPresentOnEISCD extends DataRetrieveAttribute {
    override def name: String = "sortCodeIsPresentOnEISCD"
  }

  case object SortCodeBankName extends DataRetrieveAttribute {
    override def name: String = "sortCodeBankName"
  }

  case object NonStandardAccountDetailsRequiredForBacs extends DataRetrieveAttribute {
    override def name: String = "nonStandardAccountDetailsRequiredForBacs"
  }

  case object AccountExists extends DataRetrieveAttribute {
    override def name: String = "accountExists"
  }

  case object NameMatches extends DataRetrieveAttribute {
    override def name: String = "nameMatches"
  }

  case object SortCodeSupportsDirectDebit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectDebit"
  }

  case object SortCodeSupportsDirectCredit extends DataRetrieveAttribute {
    override def name: String = "sortCodeSupportsDirectCredit"
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
    case other                                      => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute name: $other")
  }
}

sealed trait DataRetrieve {
  def id: DataRetrieveId
  def attributes: List[DataRetrieveAttribute]
}

object DataRetrieve {

  final case class ValidateBankDetails(override val id: DataRetrieveId, sortCode: Expr, accountNumber: Expr)
      extends DataRetrieve {
    override def attributes: List[DataRetrieveAttribute] = List(DataRetrieveAttribute.IsValid)
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

  implicit val format: OFormat[DataRetrieve] = derived.oformat()
}

sealed trait DataRetrieveResult
case object DataRetrieveNotRequired extends DataRetrieveResult
case class DataRetrieveSuccess(id: DataRetrieveId, data: Map[DataRetrieveAttribute, String]) extends DataRetrieveResult
case object DataRetrieveMissingInput extends DataRetrieveResult

object DataRetrieveResult {
  implicit val dataRetrieveSuccessDataFormat: Format[Map[DataRetrieveAttribute, String]] =
    implicitly[Format[Map[String, String]]]
      .bimap[Map[DataRetrieveAttribute, String]](
        _.map { case (key, value) =>
          DataRetrieveAttribute.fromName(key) -> value
        },
        _.map { case (key, value) =>
          key.name -> value
        }
      )
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
