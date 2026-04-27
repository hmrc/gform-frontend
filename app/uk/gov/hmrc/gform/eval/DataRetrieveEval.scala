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

package uk.gov.hmrc.gform.eval

import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.recalculation.{ DateResultFlag, EvaluationStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, DataRetrieveCtx }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.Attribute
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveId, DataRetrieveResult, RetrieveDataType }
import uk.gov.hmrc.gform.typeclasses.Now

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object DataRetrieveEval {
  def getDataRetrieveAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): Option[List[String]] = {
    def getAttributes(id: DataRetrieveId) =
      if (dataRetrieve.contains(id)) {
        dataRetrieve
          .get(id)
          .flatMap(dataRetrieveResult => attributeValues(dataRetrieveResult, dataRetrieveCtx))
      } else {
        // Possible reference from outside ATL to dataRetrieve inside ATL
        val dataRetrieveResults: List[DataRetrieveResult] =
          dataRetrieve.view.filterKeys(key => key.modelPageId.id === id.modelPageId.id).values.toList

        val results: List[List[String]] =
          dataRetrieveResults.flatMap(dataRetrieveResult => attributeValues(dataRetrieveResult, dataRetrieveCtx))
        Some(results.flatten)
      }

    getAttributes(dataRetrieveCtx.id)
  }

  private[eval] def attributeValues(
    dataRetrieveResult: DataRetrieveResult,
    dataRetrieveCtx: DataRetrieveCtx
  ): Option[List[String]] =
    dataRetrieveResult.data match {
      case RetrieveDataType.ObjectType(map) => map.get(dataRetrieveCtx.attribute).map(List(_))
      case RetrieveDataType.ListType(xs)    => xs.traverse(_.get(dataRetrieveCtx.attribute))
    }

  def getDataRetrieveAddressAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): EvaluationStatus = {
    def getAddressResult(row: Map[Attribute, String]): EvaluationStatus = {
      def getAttr(attr: String): Option[String] = row.get(DataRetrieve.Attribute(attr))

      val lines: List[(Atom, String)] = List(
        getAttr("address_line_1").map(Address.street1                             -> _),
        getAttr("address_line_2").map(Address.street2                             -> _),
        getAttr("locality").map(Address.street3                                   -> _),
        getAttr("region").map(Address.street4                                     -> _),
        getAttr("postal_code").map(Address.postcode                               -> _),
        getAttr("country").map(c => if (isInUK(c)) "" else c).map(Address.country -> _)
      ).collect {
        case Some((atom, value)) if value.trim.nonEmpty => atom -> value
      }

      EvaluationStatus.AddressResult(lines)
    }

    def getResult(id: DataRetrieveId): Option[EvaluationStatus] = dataRetrieve
      .get(id)
      .map { case DataRetrieveResult(_, data, _, _, _, _) =>
        data match {
          case RetrieveDataType.ObjectType(row) => getAddressResult(row)
          case RetrieveDataType.ListType(xs) =>
            EvaluationStatus.ListResult(xs.map(row => getAddressResult(row)))
        }
      }

    getResult(dataRetrieveCtx.id).getOrElse(
      getResult(dataRetrieveCtx.id.modelPageId.baseId).getOrElse(EvaluationStatus.Empty)
    )
  }

  def getDataRetrieveAddressMap(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveId: DataRetrieveId,
    isUkAddress: Boolean
  ): Map[String, String] = {
    def getAttr(attr: String) =
      getDataRetrieveAttribute(dataRetrieve, DataRetrieveCtx(dataRetrieveId, DataRetrieve.Attribute(attr)))
        .flatMap(_.headOption)

    val atomList =
      if (isUkAddress)
        List("street1", "street2", "street3", "street4", "postcode", "uk", "country")
      else
        List("line1", "line2", "line3", "city", "postcode", "uk", "country")

    val maybeCountry = getAttr("country")
    val valuesList = List(
      getAttr("address_line_1"),
      getAttr("address_line_2"),
      getAttr("locality"),
      getAttr("region"),
      getAttr("postal_code"),
      if (isUkAddress) Some(maybeCountry.filter(_.trim.nonEmpty).fold(true)(_ => false).toString) else None,
      maybeCountry.map(country => if (isInUK(country)) "" else country)
    )

    val addressMap = atomList.zip(valuesList).toMap

    addressMap.collect {
      case (key, Some(value)) if !value.isBlank => key -> value
    }
  }

  def getFailureCount(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  )(implicit now: Now[LocalDateTime]): EvaluationStatus = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .map { dr =>
          val reset = dr.failureCountResetTime.map(_.isBefore(now.apply()))
          reset match {
            case Some(false) => dr.failureCount.getOrElse(0)
            case _           => 0
          }
        }

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold[EvaluationStatus](EvaluationStatus.Empty)(EvaluationStatus.NumberResult(_))
  }

  def getIsBlocked(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): EvaluationStatus = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .map(_.isBlocked)

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold[EvaluationStatus](EvaluationStatus.Empty)(blocked => EvaluationStatus.StringResult(blocked.toString))
  }

  def getFailureResetTime(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): EvaluationStatus = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .flatMap(_.failureCountResetTime.map(_.format(DateTimeFormatter.ofPattern("h:mma")).toLowerCase))

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold[EvaluationStatus](EvaluationStatus.Empty)(EvaluationStatus.StringResult(_))
  }

  def getFailureResetDate(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): EvaluationStatus = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .flatMap(_.failureCountResetTime.map(_.toLocalDate))

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold[EvaluationStatus](EvaluationStatus.Empty)(ld => EvaluationStatus.DateResult(ld, DateResultFlag.Date))
  }

  private def isInUK(country: String): Boolean = ukParts(country.toUpperCase)

  private val ukParts = Set("ENGLAND", "SCOTLAND", "WALES", "NORTHERN IRELAND", "GREAT BRITAIN", "UNITED KINGDOM")
}
