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
import uk.gov.hmrc.gform.eval.ExpressionResult.{ AddressResult, DateResult, Empty, ListResult, NumberResult, StringResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataRetrieveCtx
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve.Attribute
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveId, DataRetrieveResult, RetrieveDataType }
import uk.gov.hmrc.gform.typeclasses.Now

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

object DataRetrieveEval {
  private[eval] def getDataRetrieveAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): Option[List[String]] = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .flatMap { case DataRetrieveResult(_, data, _, _, _, _) =>
          data match {
            case RetrieveDataType.ObjectType(map) => map.get(dataRetrieveCtx.attribute).map(List(_))
            case RetrieveDataType.ListType(xs)    => xs.traverse(_.get(dataRetrieveCtx.attribute))
          }
        }

    getAttributes(dataRetrieveCtx.id).orElse(
      getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
    )
  }

  private[eval] def getDataRetrieveAddressAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): ExpressionResult = {
    def getAddressResult(row: Map[Attribute, String]) = {
      def getAttr(attr: String) = row.get(DataRetrieve.Attribute(attr))

      AddressResult(
        List(
          getAttr("address_line_1"),
          getAttr("address_line_2"),
          getAttr("po_box"),
          getAttr("locality"),
          getAttr("region"),
          getAttr("postal_code"),
          getAttr("country").map(c => if (isInUK(c)) "" else c)
        )
          .map(_.getOrElse(""))
          .filter(!_.isBlank)
      )
    }

    def getResult(id: DataRetrieveId) = dataRetrieve
      .get(id)
      .map { case DataRetrieveResult(_, data, _, _, _, _) =>
        data match {
          case RetrieveDataType.ObjectType(row) => getAddressResult(row)
          case RetrieveDataType.ListType(xs)    => ListResult(xs.map(row => getAddressResult(row)))
        }
      }

    getResult(dataRetrieveCtx.id).getOrElse(
      getResult(dataRetrieveCtx.id.modelPageId.baseId).getOrElse(Empty)
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

  private[eval] def getFailureCount(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  )(implicit now: Now[LocalDateTime]): ExpressionResult = {
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
      .fold(ExpressionResult.empty)(NumberResult(_))
  }

  private[eval] def getIsBlocked(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): ExpressionResult = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .map(_.isBlocked)

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold(ExpressionResult.empty)(blocked => StringResult(blocked.toString))
  }

  private[eval] def getFailureResetTime(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): ExpressionResult = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .flatMap(_.failureCountResetTime.map(_.format(DateTimeFormatter.ofPattern("HH:mm"))))

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold(ExpressionResult.empty)(StringResult(_))
  }

  private[eval] def getFailureResetDate(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): ExpressionResult = {
    def getAttributes(id: DataRetrieveId) =
      dataRetrieve
        .get(id)
        .flatMap(_.failureCountResetTime.map(_.toLocalDate))

    getAttributes(dataRetrieveCtx.id)
      .orElse(
        getAttributes(dataRetrieveCtx.id.modelPageId.baseId)
      )
      .fold(ExpressionResult.empty)(DateResult(_))
  }

  private def isInUK(country: String): Boolean = ukParts(country.toUpperCase)

  private val ukParts = Set("ENGLAND", "SCOTLAND", "WALES", "NORTHERN IRELAND", "GREAT BRITAIN", "UNITED KINGDOM")
}
