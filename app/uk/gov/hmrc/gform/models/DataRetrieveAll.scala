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

package uk.gov.hmrc.gform.models

import cats.syntax.eq._
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }

final case class DataRetrieveAll(lookup: Map[DataRetrieveId, DataRetrieve]) extends AnyVal {
  def isInteger(dataRetriveId: DataRetrieveId, attribute: DataRetrieve.Attribute): Boolean =
    lookup.get(dataRetriveId) match {
      case None     => false
      case Some(dr) => dr.attrTypeMapping.get(attribute).fold(false)(_ === DataRetrieve.AttrType.Integer)
    }
}

object DataRetrieveAll {
  val empty: DataRetrieveAll = DataRetrieveAll(Map.empty)

  def from[A <: PageMode](formModel: FormModel[A]): DataRetrieveAll =
    DataRetrieveAll(
      formModel.pages.map(_.dataRetrieves).flatMap(drs => drs.map(dr => dr.id -> dr)).toMap ++
        formModel.dataRetrieve.fold(Map.empty[DataRetrieveId, DataRetrieve])(drs =>
          drs.toList.map(dr => dr.id -> dr).toMap
        )
    )

}
