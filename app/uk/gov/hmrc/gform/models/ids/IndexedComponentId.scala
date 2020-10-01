/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.syntax.option._

sealed trait IndexedComponentId extends Product with Serializable {

  def baseComponentId: BaseComponentId

  def maybeIndex: Option[Int] = fold[Option[Int]](_ => none)(_.index.some)

  def increment: IndexedComponentId = mapIndex(_ + 1)
  def decrement: IndexedComponentId = mapIndex(_ - 1)

  private def mapIndex(f: Int => Int) = fold[IndexedComponentId](identity) {
    case IndexedComponentId.Indexed(baseComponentId, index) =>
      IndexedComponentId.indexed(baseComponentId, f(index))
  }
  def fold[B](
    f: IndexedComponentId.Pure => B
  )(
    g: IndexedComponentId.Indexed => B
  ): B = this match {
    case t: IndexedComponentId.Pure    => f(t)
    case t: IndexedComponentId.Indexed => g(t)
  }
}

object IndexedComponentId {

  def pure(baseComponentId: BaseComponentId): IndexedComponentId = Pure(baseComponentId)
  def indexed(baseComponentId: BaseComponentId, index: Int): IndexedComponentId = Indexed(baseComponentId, index)

  case class Pure(baseComponentId: BaseComponentId) extends IndexedComponentId
  case class Indexed(baseComponentId: BaseComponentId, index: Int) extends IndexedComponentId

}
