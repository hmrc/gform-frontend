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

package uk.gov.hmrc.gform.gform

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Group => FtGroup }

sealed trait RenderUnit extends Product with Serializable {
  def fold[B](f: RenderUnit.Pure => B)(g: RenderUnit.Group => B): B = this match {
    case r: RenderUnit.Pure  => f(r)
    case r: RenderUnit.Group => g(r)
  }
}

object RenderUnit {

  def pure(formComponent: FormComponent): RenderUnit = Pure(formComponent)
  def group(group: FtGroup, formComponent: FormComponent): RenderUnit =
    Group(formComponent.baseComponentId, NonEmptyList.one(group -> formComponent))
  case class Pure(formComponent: FormComponent) extends RenderUnit
  case class Group(baseComponentId: BaseComponentId, formComponents: NonEmptyList[(FtGroup, FormComponent)])
      extends RenderUnit {
    def prepend(formComponent: (FtGroup, FormComponent)): RenderUnit =
      Group(baseComponentId, formComponent :: formComponents)

    def formComponent: FormComponent = formComponents.head._2
    def group: FtGroup = formComponents.head._1
  }
}
