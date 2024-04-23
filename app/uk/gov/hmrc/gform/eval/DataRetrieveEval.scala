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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.DataRetrieveCtx
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieveId, DataRetrieveResult, RetrieveDataType }

object DataRetrieveEval {
  private[eval] def getDataRetrieveAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult],
    dataRetrieveCtx: DataRetrieveCtx
  ): Option[List[String]] =
    dataRetrieve
      .get(dataRetrieveCtx.id)
      .flatMap { case DataRetrieveResult(_, data, _) =>
        data match {
          case RetrieveDataType.ObjectType(map) => map.get(dataRetrieveCtx.attribute).map(List(_))
          case RetrieveDataType.ListType(xs)    => xs.traverse(_.get(dataRetrieveCtx.attribute))
        }
      }

  private[eval] def getDataRetrieveAddressAttribute(
    dataRetrieve: Map[DataRetrieveId, DataRetrieveResult]
  ): List[String] = {
    def getAttr(attr: String) =
      getDataRetrieveAttribute(dataRetrieve, DataRetrieveCtx(DataRetrieveId("company"), DataRetrieve.Attribute(attr)))
        .flatMap(_.headOption)

    {
      for {
        addressLine1 <- getAttr("address_line_1")
        addressLine2 <- getAttr("address_line_2")
        poBox        <- getAttr("po_box")
        locality     <- getAttr("locality")
        region       <- getAttr("region")
        postalCode   <- getAttr("postal_code")
        country      <- getAttr("country")
      } yield List(
        addressLine1,
        addressLine2,
        poBox,
        locality,
        region,
        postalCode,
        if (isInUK(country)) "" else country
      )
    }
      .getOrElse(List.empty[String])
      .filter(!_.isBlank)
  }

  private def isInUK(country: String): Boolean = ukParts(country.toUpperCase)

  private val ukParts = Set("ENGLAND", "SCOTLAND", "WALES", "NORTHERN IRELAND", "GREAT BRITAIN", "UNITED KINGDOM")
}
