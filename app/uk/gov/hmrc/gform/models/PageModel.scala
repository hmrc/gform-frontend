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

package uk.gov.hmrc.gform.models

import cats.instances.int._
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.ids.{ ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.IsRevealingChoice
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AddToListId, AllValidIfs, FormComponent, FormComponentId, IncludeIf, Page, Section, ValidIf }

sealed trait PageModel[A <: PageMode] extends Product with Serializable {
  def title: SmartString = fold(_.page.title)(_.expandedTitle)

  def isTerminationPage = fold(_.page.isTerminationPage)(_ => false)

  def isAddToList(addToListId: AddToListId): Boolean =
    fold(_.source.byAddToListId(addToListId))(_.source.byAddToListId(addToListId))

  def repeaterOf(addToListId: AddToListId): Option[Repeater[A]] =
    fold[Option[Repeater[A]]](_ => None)(r => if (r.source.byAddToListId(addToListId)) Some(r) else None)

  def indexOfAddToList(index: Int, addToListId: AddToListId): Boolean =
    fold(_ => false)(r => r.source.byAddToListId(addToListId) && r.index === index)

  def addToListSize: Int = fold(_ => 0)(_.source.pages.size)

  def sourceIsAddToList: Option[Section.AddToList] = fold(_.source.addToList)(_.source.addToList)

  def fold[B](f: Singleton[A] => B)(g: Repeater[A] => B): B = this match {
    case s: Singleton[A] => f(s)
    case r: Repeater[A]  => g(r)
  }

  private def nestedFields(formComponent: FormComponent): List[FormComponent] =
    formComponent :: formComponent.childrenFormComponents

  def allFormComponents: List[FormComponent] =
    fold(_.page.fields.flatMap(nestedFields))(r => r.addAnotherQuestion :: Nil)
  def allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  def allMultiValueIds: List[MultiValueId] = allFormComponents.map(_.multiValueId)

  //def allVariadicValueIds: Set[VariadicValueId] = allMultiValueIds.flatMap(_.toVariadicValueIds).toSet
  def allModelComponentIds: Set[ModelComponentId] = allMultiValueIds.flatMap(_.toModelComponentIds).toSet

  def pageLookup: Map[FormComponentId, PageModel[A]] =
    allFormComponents.foldLeft(Map.empty[FormComponentId, PageModel[A]]) {
      case (acc, fc) =>
        acc + (fc.id -> this)
    }

  def getIncludeIf: Option[IncludeIf] = fold(_.page.includeIf)(_.includeIf)

  def allValidIfs: List[(List[ValidIf], FormComponent)] =
    fold(_.page.fields.collect { case fc @ AllValidIfs(validIfs) => (validIfs, fc) })(_ => Nil)
}

case class Singleton[A <: PageMode](page: Page[A], source: Section) extends PageModel[A]
case class Repeater[A <: PageMode](
  expandedTitle: SmartString,
  expandedDescription: SmartString,
  expandedShortName: SmartString,
  includeIf: Option[IncludeIf],
  addAnotherQuestion: FormComponent,
  index: Int,
  source: Section.AddToList
) extends PageModel[A]
