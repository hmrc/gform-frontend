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

import cats.data.NonEmptyList
import uk.gov.hmrc.gform.models.ids.{ ModelComponentId, ModelPageId, MultiValueId }
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllChoiceIncludeIfs, AllMiniSummaryListIncludeIfs, AllValidIfs, Confirmation, FormComponent, FormComponentId, IncludeIf, Instruction, IsPostcodeLookup, IsUpscanInitiateFileUpload, Page, PageId, PresentationHint, RedirectCtx, RemoveItemIf, ValidIf }

sealed trait PageModel[A <: PageMode] extends Product with Serializable {
  def title: SmartString = fold(_.page.title)(_.expandedUpdateTitle)(_.expandedTitle)
  def caption: Option[SmartString] = fold(_.page.caption)(_.expandedCaption)(_.expandedCaption)
  def noPIITitle: Option[SmartString] = fold(_.page.noPIITitle)(_.expandedNoPIIUpdateTitle)(_.expandedNoPIITitle)
  def id: Option[PageId] = fold(_.page.id)(c => Some(c.expandedId))(r => Some(r.expandedId))

  def isTerminationPage = fold(_.page.isTerminationPage)(_ => false)(_ => false)

  def fold[B](e: Singleton[A] => B)(f: CheckYourAnswers[A] => B)(g: Repeater[A] => B): B = this match {
    case s: Singleton[A]        => e(s)
    case c: CheckYourAnswers[A] => f(c)
    case r: Repeater[A]         => g(r)
  }

  private def nestedFields(formComponent: FormComponent): List[FormComponent] =
    formComponent :: formComponent.childrenFormComponents

  def allFormComponents: List[FormComponent] =
    fold(_.page.allFields.flatMap(nestedFields))(_.formComponent :: Nil)(_.addAnotherQuestion :: Nil)

  def allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  def allMultiValueIds: List[MultiValueId] = allFormComponents.map(_.multiValueId)

  def allModelComponentIds: Set[ModelComponentId] = allMultiValueIds.flatMap(_.toModelComponentIds).toSet

  def pageLookup: Map[FormComponentId, PageModel[A]] =
    allFormComponents.foldLeft(Map.empty[FormComponentId, PageModel[A]]) { case (acc, fc) =>
      acc + (fc.id -> this)
    }

  def getIncludeIf: Option[IncludeIf] = fold(_.page.includeIf)(_ => None)(_.includeIf)

  def allValidIfs: List[(List[ValidIf], FormComponent)] =
    fold(_.page.allFields.collect { case fc @ AllValidIfs(validIfs) => (validIfs, fc) })(_ => Nil)(_ => Nil)

  def allChoiceIncludeIfs: List[(IncludeIf, FormComponent)] =
    fold(_.page.allFields.collect { case fc @ AllChoiceIncludeIfs(includeIfs) => includeIfs.map((_, fc)) }.flatten)(_ =>
      Nil
    )(_ => Nil)

  def allMiniSummaryListIncludeIfs: List[(IncludeIf, FormComponent)] =
    fold(_.page.allFields.collect { case fc @ AllMiniSummaryListIncludeIfs(includeIfs) =>
      includeIfs.map((_, fc))
    }.flatten)(_ => Nil)(_ => Nil)

  def allComponentIncludeIfs: List[(IncludeIf, FormComponent)] =
    fold(_.page.allFields.flatMap(fc => fc.includeIf.map(_ -> fc)))(_ => Nil)(_ => Nil)

  def allATLRepeatsWhiles: List[IncludeIf] =
    fold(_ => List.empty[IncludeIf])(_ => List.empty[IncludeIf])(repeater => repeater.repeatsWhile.toList)

  def maybeConfirmation: Option[Confirmation] = fold(_.page.confirmation)(_ => None)(_ => None)

  def confirmationPage(confirmationLookup: Map[ModelPageId, ConfirmationPage.Confirmee]): ConfirmationPage =
    maybeConfirmation match {
      case None               => id.flatMap(id => confirmationLookup.get(id.modelPageId)).getOrElse(ConfirmationPage.Not)
      case Some(confirmation) => ConfirmationPage.fromConfirmation(confirmation)
    }

  def upscanInitiateRequests: List[FormComponentId] =
    fold(_.page.allFieldsNested.collect { case IsUpscanInitiateFileUpload(formComponent) =>
      formComponent.id
    })(_ => Nil)(_ => Nil)

  def postcodeLookup: Option[FormComponent] = fold(_.page.allFields.collectFirst { case fc @ IsPostcodeLookup(_) =>
    fc
  })(_ => None)(_ => None)

  def redirects: List[RedirectCtx] = fold(_.page.redirects.map(_.toList).toList.flatten)(_ => Nil)(_ => Nil)

  def dataRetrieves: List[DataRetrieve] =
    fold(_.page.dataRetrieve.toList.flatMap(_.toList))(_ => List.empty[DataRetrieve])(_ => List.empty[DataRetrieve])

}

case class Singleton[A <: PageMode](page: Page[A]) extends PageModel[A]
case class CheckYourAnswers[A <: PageMode](
  expandedId: PageId,
  expandedTitle: Option[SmartString],
  expandedCaption: Option[SmartString],
  expandedUpdateTitle: SmartString,
  expandedNoPIITitle: Option[SmartString],
  expandedNoPIIUpdateTitle: Option[SmartString],
  expandedHeader: Option[SmartString],
  expandedFooter: Option[SmartString],
  expandedContinueLabel: Option[SmartString],
  formComponent: FormComponent,
  index: Int,
  presentationHint: Option[PresentationHint],
  expandedRemoveItemIf: Option[RemoveItemIf],
  fields: Option[NonEmptyList[FormComponent]]
) extends PageModel[A]
case class Repeater[A <: PageMode](
  expandedTitle: SmartString,
  expandedCaption: Option[SmartString],
  expandedId: PageId,
  expandedNoPIITitle: Option[SmartString],
  expandedDescription: SmartString,
  expandedSummaryDescription: SmartString,
  expandedShortName: SmartString,
  expandedSummaryName: SmartString,
  includeIf: Option[IncludeIf],
  addAnotherQuestion: FormComponent,
  index: Int,
  instruction: Option[Instruction],
  fields: Option[NonEmptyList[FormComponent]],
  repeatsUntil: Option[IncludeIf],
  repeatsWhile: Option[IncludeIf]
) extends PageModel[A]
