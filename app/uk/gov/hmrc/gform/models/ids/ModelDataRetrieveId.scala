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

package uk.gov.hmrc.gform.models.ids

import uk.gov.hmrc.gform.sharedmodel.DataRetrieveId

sealed trait ModelDataRetrieveId {
  def baseId = this match {
    case ModelDataRetrieveId.Pure(id)       => DataRetrieveId(id)
    case ModelDataRetrieveId.Indexed(id, _) => DataRetrieveId(id)
  }
}

object ModelDataRetrieveId {
  private val IndexedId = "^(\\d+)_(.+)$".r

  def fromId(dataRetrieveId: DataRetrieveId): ModelDataRetrieveId = dataRetrieveId.value match {
    case IndexedId(index, id) => Indexed(id, index.toInt)
    case other                => Pure(other)
  }

  case class Pure(id: String) extends ModelDataRetrieveId
  case class Indexed(id: String, index: Int) extends ModelDataRetrieveId
}
