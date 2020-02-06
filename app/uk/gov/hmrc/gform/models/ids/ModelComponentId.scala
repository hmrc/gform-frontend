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

import cats.{ Eq, Show }
import cats.instances.int._
import cats.syntax.eq._
import cats.syntax.option._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

sealed trait ModelComponentId extends Product with Serializable {

  def indexedComponentId: IndexedComponentId

  def baseComponentId: BaseComponentId = indexedComponentId.baseComponentId
  def maybeIndex: Option[Int] = indexedComponentId.maybeIndex

  def toAtomicFormComponentId(atom: Atom): ModelComponentId.Atomic =
    fold(pure => ModelComponentId.Atomic(pure.indexedComponentId, atom))(identity)

  def hasExpansionPrefix(index: Int): Boolean = maybeIndex.fold(false)(_ === index)

  def expandWithPrefix(index: Int): ModelComponentId = map {
    case i: IndexedComponentId.Indexed => i
    case i: IndexedComponentId.Pure    => IndexedComponentId.indexed(i.baseComponentId, index)
  }

  def increment: ModelComponentId = map(_.increment)
  def decrement: ModelComponentId = map(_.decrement)

  private def map(f: IndexedComponentId => IndexedComponentId): ModelComponentId =
    fold(
      pure => ModelComponentId.pure(f(pure.indexedComponentId))
    )(
      atomic => ModelComponentId.atomic(f(atomic.indexedComponentId), atomic.atom)
    )

  def toFormComponentId: FormComponentId = FormComponentId(toMongoIdentifier)

  def toMongoIdentifier: String =
    fold {
      case ModelComponentId.Pure(IndexedComponentId.Pure(BaseComponentId(value)))           => value
      case ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId(value), index)) => index + "_" + value
    } {
      case ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId(value)), Atom(atom)) => value + "-" + atom
      case ModelComponentId.Atomic(IndexedComponentId.Indexed(BaseComponentId(value), index), Atom(atom)) =>
        index + "_" + value + "-" + atom
    }

  def fold[B](
    f: ModelComponentId.Pure => B
  )(
    g: ModelComponentId.Atomic => B
  ): B = this match {
    case t: ModelComponentId.Pure   => f(t)
    case t: ModelComponentId.Atomic => g(t)
  }
}

object ModelComponentId {

  def pure(indexedComponentId: IndexedComponentId): ModelComponentId = Pure(indexedComponentId)
  def atomic(indexedComponentId: IndexedComponentId, atom: Atom): ModelComponentId = Atomic(indexedComponentId, atom)

  def atomicCurry(indexedComponentId: IndexedComponentId)(atom: Atom): ModelComponentId.Atomic =
    Atomic(indexedComponentId, atom)

  case class Pure(indexedComponentId: IndexedComponentId) extends ModelComponentId
  case class Atomic(indexedComponentId: IndexedComponentId, atom: Atom) extends ModelComponentId

  implicit val catsEq: Eq[ModelComponentId] = Eq.fromUniversalEquals
  implicit val show: Show[ModelComponentId] = Show.show(_.toString)
}
