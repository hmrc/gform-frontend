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
  def exprId: String
}
case object DataRetrieveIsValid extends DataRetrieveAttribute {
  override def exprId: String = "isValid"
}

case object DataRetrieveAttribute {
  implicit val format: OFormat[DataRetrieveAttribute] = derived.oformat()

  def fromString(value: String): DataRetrieveAttribute = value match {
    case "DataRetrieveIsValid" => DataRetrieveIsValid
    case other                 => throw new IllegalArgumentException(s"Unknown DataRetrieveAttribute value $other")
  }
}

sealed trait DataRetrieve {
  def id: DataRetrieveId
  def attributes: List[DataRetrieveAttribute]
}

case class ValidateBank(override val id: DataRetrieveId, sortCode: Expr, accountNumber: Expr) extends DataRetrieve {
  override def attributes: List[DataRetrieveAttribute] = List(DataRetrieveIsValid)
}

object DataRetrieve {
  implicit val format: OFormat[DataRetrieve] = derived.oformat()
}

sealed trait DataRetrieveResult
case object DataRetrieveNotRequired extends DataRetrieveResult
case class DataRetrieveSuccess(id: DataRetrieveId, data: Map[DataRetrieveAttribute, String]) extends DataRetrieveResult
case object DataRetrieveFailed extends DataRetrieveResult
case object DataRetrieveMissingInput extends DataRetrieveResult

object DataRetrieveResult {
  implicit val dataRetrieveSuccessDataFormat: Format[Map[DataRetrieveAttribute, String]] =
    implicitly[Format[Map[String, String]]]
      .bimap[Map[DataRetrieveAttribute, String]](
        _.map { case (key, value) =>
          DataRetrieveAttribute.fromString(key) -> value
        },
        _.map { case (key, value) =>
          key.toString -> value
        }
      )
  implicit val format: Format[DataRetrieveResult] = derived.oformat()
}
