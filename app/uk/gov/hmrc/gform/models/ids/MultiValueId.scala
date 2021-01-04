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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent

sealed trait MultiValueId extends Product with Serializable {

  def modelComponentId: ModelComponentId.Pure

  def lookupFor(formComponent: FormComponent): Map[ModelComponentId, FormComponent] =
    toModelComponentIds.map(_ -> formComponent).toMap

  def toModelComponentIds: List[ModelComponentId] =
    fold[List[ModelComponentId]](
      pure => List(pure.modelComponentId)
    )(
      multiple => multiple.modelComponentId :: multiple.atoms.toList
    )

  def atomsModelComponentIds: List[ModelComponentId] =
    fold[List[ModelComponentId]](_.modelComponentId :: Nil)(_.atoms.toList)

  def firstAtomModelComponentId: ModelComponentId.Atomic =
    fold[ModelComponentId.Atomic](pure =>
      throw new IllegalArgumentException(s"Cannot ask for atom for pure value: $pure"))(_.atoms.head)

  def fold[B](
    f: MultiValueId.Pure => B
  )(
    g: MultiValueId.MultiValue => B
  ): B = this match {
    case t: MultiValueId.Pure       => f(t)
    case t: MultiValueId.MultiValue => g(t)
  }
}

object MultiValueId {

  def pure(modelComponentId: ModelComponentId.Pure): MultiValueId = Pure(modelComponentId)
  def multiValue(modelComponentId: ModelComponentId.Pure, atoms: NonEmptyList[ModelComponentId.Atomic]): MultiValueId =
    MultiValue(modelComponentId, atoms)

  case class Pure(modelComponentId: ModelComponentId.Pure) extends MultiValueId
  case class MultiValue(modelComponentId: ModelComponentId.Pure, atoms: NonEmptyList[ModelComponentId.Atomic])
      extends MultiValueId

}
