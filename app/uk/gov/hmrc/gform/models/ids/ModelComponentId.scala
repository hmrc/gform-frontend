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

import cats.{ Eq, Show }
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId

sealed trait ModelComponentId extends Product with Serializable {

  def indexedComponentId: IndexedComponentId

  def baseComponentId: BaseComponentId = indexedComponentId.baseComponentId
  def maybeIndex: Option[Int] = indexedComponentId.maybeIndex

  def toAtomicFormComponentId(atom: Atom): ModelComponentId.Atomic =
    fold(pure => ModelComponentId.Atomic(pure.indexedComponentId, atom))(identity)

  def expandWithPrefix(index: Int): ModelComponentId = map {
    case i: IndexedComponentId.Indexed => i
    case i: IndexedComponentId.Pure    => IndexedComponentId.indexed(i.baseComponentId, index)
  }

  def increment: ModelComponentId = map(_.increment)
  def decrement: ModelComponentId = map(_.decrement)

  def map(f: IndexedComponentId => IndexedComponentId): ModelComponentId =
    fold(pure => ModelComponentId.pure(f(pure.indexedComponentId)))(atomic =>
      ModelComponentId.atomic(f(atomic.indexedComponentId), atomic.atom)
    )

  def toFormComponentId: FormComponentId = FormComponentId(toMongoIdentifier)

  def toMongoIdentifier: String =
    fold {
      case ModelComponentId.Pure(IndexedComponentId.Pure(BaseComponentId(value)))           => value
      case ModelComponentId.Pure(IndexedComponentId.Indexed(BaseComponentId(value), index)) => s"${index}_$value"
    } {
      case ModelComponentId.Atomic(IndexedComponentId.Pure(BaseComponentId(value)), Atom(atom)) => s"$value-$atom"
      case ModelComponentId.Atomic(IndexedComponentId.Indexed(BaseComponentId(value), index), Atom(atom)) =>
        s"${index}_$value-$atom"
    }

  def fold[B](
    f: ModelComponentId.Pure => B
  )(
    g: ModelComponentId.Atomic => B
  ): B = this match {
    case t: ModelComponentId.Pure   => f(t)
    case t: ModelComponentId.Atomic => g(t)
  }

  def isAtomic(atomValue: String): Boolean = this match {
    case ModelComponentId.Atomic(_, Atom(v)) if v == atomValue => true
    case _                                                     => false
  }

  def removeIndex: ModelComponentId = fold[ModelComponentId] { case ModelComponentId.Pure(indexedComponentId) =>
    ModelComponentId.Pure(indexedComponentId.toPure)
  } { case ModelComponentId.Atomic(indexedComponentId, atom) =>
    ModelComponentId.Atomic(indexedComponentId.toPure, atom)
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
