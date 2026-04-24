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
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.recalculation.FreeCalculator
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExpr, IsChoice, IsGroup, IsTableComp }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.KeyDisplayWidth.KeyDisplayWidth
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ AllValidIfs, AtlDescription, Confirmation, FormComponent, FormComponentId, IncludeIf, Instruction, IsFileUpload, IsMultiFileUpload, IsPostcodeLookup, Page, PageId, PresentationHint, RedirectCtx, RemoveItemIf, ValidIf }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.LayoutDisplayWidth.LayoutDisplayWidth

sealed trait PageModel extends Product with Serializable {
  def title: SmartString = fold(_.page.title)(_.expandedUpdateTitle)(_.expandedTitle)
  def caption: Option[SmartString] = fold(_.page.caption)(_.expandedCaption)(_.expandedCaption)
  def noPIITitle: Option[SmartString] = fold(_.page.noPIITitle)(_.expandedNoPIIUpdateTitle)(_.expandedNoPIITitle)
  def id: Option[PageId] = fold(_.page.id)(c => Some(c.expandedId))(r => Some(r.expandedId))

  def isTerminationPage(freeCalculator: FreeCalculator) =
    fold(_.page.isTerminationPage(freeCalculator))(_ => false)(_ => false)

  def fold[B](e: Singleton => B)(f: CheckYourAnswers => B)(g: Repeater => B): B = this match {
    case s: Singleton        => e(s)
    case c: CheckYourAnswers => f(c)
    case r: Repeater         => g(r)
  }

  def booleanExprs(): List[BooleanExpr] = {

    val pageLevelIncludeIf: List[IncludeIf] = getIncludeIf.toList

    pageLevelIncludeIf.map(_.booleanExpr) ++
      allComponentIncludeIfs.map(_._1).map(_.booleanExpr) ++
      allValidIfs.flatMap(_._1).map(_.booleanExpr)
  }

  private def nestedFields(formComponent: FormComponent): List[FormComponent] =
    formComponent :: formComponent.childrenFormComponents

  def allFormComponents: List[FormComponent] =
    fold(_.page.allFields.flatMap(nestedFields))(_.fields.toList.flatMap(_.toList))(_.addAnotherQuestion :: Nil)

  def allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  def allMultiValueIds: List[MultiValueId] = allFormComponents.map(_.multiValueId)

  def allModelComponentIds: Set[ModelComponentId] = allMultiValueIds.flatMap(_.toModelComponentIds).toSet

  def pageLookup: Map[FormComponentId, PageModel] =
    allFormComponents.foldLeft(Map.empty[FormComponentId, PageModel]) { case (acc, fc) =>
      acc + (fc.id -> this)
    }

  def getIncludeIf: Option[IncludeIf] = fold(_.page.includeIf)(_ => None)(_ => None)

  def getNotRequiredIf: Option[IncludeIf] = fold(_.page.notRequiredIf)(_ => None)(_.notRequiredIf)

  def allValidIfs: List[(List[ValidIf], FormComponent)] =
    fold(_.page.allFields.collect { case fc @ AllValidIfs(validIfs) => (validIfs, fc) })(_ => Nil)(_ => Nil)

  def allComponentIncludeIfs: List[(IncludeIf, FormComponent)] =
    fold(_.page.allFields.flatMap(fc => fc.includeIf.map(_ -> fc)))(_ => Nil)(_ => Nil)

  def allATLRepeatsWhiles =
    fold(_ => List.empty[(BaseComponentId, List[IncludeIf])])(_ => List.empty[(BaseComponentId, List[IncludeIf])])(
      repeater => List(repeater.addAnotherQuestion.id.baseComponentId -> repeater.repeatsWhile.toList)
    )

  def maybeConfirmation: Option[Confirmation] = fold(_.page.confirmation)(_ => None)(_ => None)

  def confirmationPage: ConfirmationPage =
    maybeConfirmation.map(ConfirmationPage.fromConfirmation).getOrElse(ConfirmationPage.Not)

  def upscanInitiateRequests(defaultMaximumFileSize: Int): List[(FormComponentId, Int)] =
    fold(_.page.allFieldsNested.collect {
      case fc @ IsFileUpload(fu)      => fc.id -> fu.fileSizeLimit.getOrElse(defaultMaximumFileSize)
      case fc @ IsMultiFileUpload(fu) => fc.id -> fu.fileSizeLimit.getOrElse(defaultMaximumFileSize)
    })(_ => Nil)(_ => Nil)

  def postcodeLookup: Option[FormComponent] = fold(_.page.allFields.collectFirst { case fc @ IsPostcodeLookup(_) =>
    fc
  })(_ => None)(_ => None)

  def redirects: List[RedirectCtx] = fold(_.page.redirects.map(_.toList).toList.flatten)(_ => Nil)(_ => Nil)

  def dataRetrieves: List[DataRetrieve] =
    fold(_.page.dataRetrieve.toList.flatMap(_.toList))(_ => List.empty[DataRetrieve])(_ => List.empty[DataRetrieve])

}

case class Singleton(page: Page) extends PageModel
case class CheckYourAnswers(
  expandedId: PageId,
  expandedTitle: Option[SmartString],
  expandedCaption: Option[SmartString],
  expandedUpdateTitle: SmartString,
  expandedNoPIITitle: Option[SmartString],
  expandedNoPIIUpdateTitle: Option[SmartString],
  expandedHeader: Option[SmartString],
  expandedFooter: Option[SmartString],
  expandedContinueLabel: Option[SmartString],
  index: Int,
  presentationHint: Option[PresentationHint],
  expandedRemoveItemIf: Option[RemoveItemIf],
  fields: Option[NonEmptyList[FormComponent]],
  displayWidth: Option[LayoutDisplayWidth],
  keyDisplayWidth: Option[KeyDisplayWidth]
) extends PageModel
case class Repeater(
  expandedTitle: SmartString,
  expandedCaption: Option[SmartString],
  expandedId: PageId,
  expandedNoPIITitle: Option[SmartString],
  expandedDescription: AtlDescription,
  expandedSummaryDescription: SmartString,
  expandedShortName: SmartString,
  expandedSummaryName: SmartString,
  addAnotherQuestion: FormComponent,
  index: Int,
  instruction: Option[Instruction],
  fields: Option[NonEmptyList[FormComponent]],
  repeatsUntil: Option[IncludeIf],
  repeatsWhile: Option[IncludeIf],
  expandedDescriptionTotal: Option[AtlDescription.KeyValueBased],
  notRequiredIf: Option[IncludeIf],
  displayWidth: Option[LayoutDisplayWidth],
  expandedRemovePageContent: Option[SmartString]
) extends PageModel

object Singleton {
  def expand(page: Page, freeCalculator: FreeCalculator)(implicit messages: Messages): Singleton = {
    val expanded = page.fields.flatMap {
      case fc @ IsChoice(choice)   => OptionDataUtils.expand(fc, choice, freeCalculator.calc) :: Nil
      case fc @ IsTableComp(table) => TableUtils.expand(fc, table, freeCalculator) :: Nil
      //case fc @ IsRevealingChoice(_) => fc :: Nil
      case fc @ IsGroup(group) => ExpandUtils.expandGroup(fc, group, freeCalculator.variadicFormData)
      case otherwise           => otherwise :: Nil
    }
    Singleton(page.copy(fields = expanded))
  }
}
