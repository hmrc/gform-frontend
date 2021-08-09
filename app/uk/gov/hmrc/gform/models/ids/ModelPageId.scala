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

package uk.gov.hmrc.gform.models.ids

import uk.gov.hmrc.gform.sharedmodel.formtemplate.PageId

sealed trait ModelPageId {
  def toIndexed(index: Int): ModelPageId = this match {
    case ModelPageId.Pure(id)      => ModelPageId.Indexed(id, index)
    case ModelPageId.Indexed(_, _) => this
  }

  def baseId: String = this match {
    case ModelPageId.Pure(id)       => id
    case ModelPageId.Indexed(id, _) => id
  }
}

object ModelPageId {

  private val IndexedPageId = "^(\\d+)_(.+)$".r

  def fromPageId(pageId: PageId): ModelPageId = pageId.id match {
    case IndexedPageId(index, id) => Indexed(id, index.toInt)
    case other                    => Pure(other)
  }

  case class Pure(id: String) extends ModelPageId
  case class Indexed(id: String, index: Int) extends ModelPageId
}
