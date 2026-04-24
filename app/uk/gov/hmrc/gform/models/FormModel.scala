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
import cats.syntax.eq._
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.recalculation.FormModelMetadata
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

final class FormModel(
  val brackets: Brackets
) {

  val pagesWithIndex: NonEmptyList[(PageModel, SectionNumber)] = brackets.toPageModelWithNumber

  val metadata: FormModelMetadata = FormModelMetadata.fromPageModels(pagesWithIndex)

  val allConfirmations: List[Confirmation] = pagesWithIndex.toList.flatMap { case (pageModel, _) =>
    pageModel.maybeConfirmation.toList
  }

  val confirmationPageMap = pagesWithIndex.toList.flatMap { case (pageModel, sectionNumber) =>
    pageModel.maybeConfirmation.map { confirmation =>
      sectionNumber -> confirmation
    }
  }

  val (pages, availableSectionNumbers) = pagesWithIndex.toList.unzip

  object taskList {

    def availablePages(coordinates: Coordinates): List[PageModel] = {
      val taskModel: TaskModel = brackets.unsafeToTaskList.bracketsFor(coordinates)
      taskModel match {
        case TaskModel.AllHidden() => List.empty[PageModel]
        case TaskModel.Editable(xs) =>
          val pagesWithIndex: NonEmptyList[(PageModel, SectionNumber)] = xs.flatMap(_.toPageModelWithNumber)
          val (pages, availableSectionNumbers) = pagesWithIndex.toList.unzip
          pages
      }
    }

    def allFormComponents(coordinates: Coordinates): List[FormComponent] =
      availablePages(coordinates).flatMap(_.allFormComponents)
  }

  val nextVisibleSectionNumber: SectionNumber => Option[SectionNumber] = {
    case sectionNumber: SectionNumber.TaskList =>
      availableSectionNumbers
        .collect { case t: SectionNumber.TaskList => t }
        .find(sn => sectionNumber.coordinates === sn.coordinates && sn.sectionNumber >= sectionNumber.sectionNumber)
    case sectionNumber: SectionNumber.Classic =>
      availableSectionNumbers
        .find(sn => sn >= sectionNumber)
  }

  val allFormComponents: List[FormComponent] = pages.flatMap(_.allFormComponents)

  val allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  val fcLookup: Map[FormComponentId, FormComponent] =
    allFormComponents.map(fc => fc.id -> fc).toMap

  val allMultiValueIds: List[MultiValueId] =
    allFormComponents.map(_.multiValueId)

  val allModelComponentIds: Set[ModelComponentId] = allMultiValueIds.flatMap(_.toModelComponentIds).toSet

  def allDynamicChoices: List[(FormComponentId, Set[BaseComponentId])] = allFormComponents
    .collect { case fc @ HasDynamicChoice((fcId, baseComponentIds)) =>
      (fcId, baseComponentIds)
    }

  val propagator = Propagator(allFormComponents)

  def allUpperCaseIds: Set[ModelComponentId] =
    allFormComponents.collect { case fc @ IsCapitalised() =>
      fc.modelComponentId
    }.toSet

  val allMultiSelectionIds: Set[ModelComponentId] = allFormComponents
    .collect {
      case fc @ IsChoice(_)          => fc.id
      case fc @ IsRevealingChoice(_) => fc.id
    }
    .map(_.modelComponentId)
    .toSet

  val allFileIds: Set[ModelComponentId] = allFormComponents
    .collect { case fc @ IsFileUpload(_) =>
      fc.id
    }
    .map(_.modelComponentId)
    .toSet

  val allMultiFileIds: Set[ModelComponentId] = allFormComponents
    .collect { case fc @ IsMultiFileUpload(_) =>
      fc.id
    }
    .map(_.modelComponentId)
    .toSet

  val dateLookup: Map[ModelComponentId, DateValue] = allFormComponents.collect {
    case fc @ IsDate(Date(_, _, Some(value))) => fc.id.modelComponentId -> value
  }.toMap

  val addressLookup: Set[BaseComponentId] = allFormComponents.collect { case fc @ IsAddress(_) =>
    fc.id.baseComponentId
  }.toSet

  val taxPeriodDate: Set[BaseComponentId] = allFormComponents.collect { case fc @ IsTaxPeriodDate() =>
    fc.id.baseComponentId
  }.toSet

  val overseasAddressLookup: Set[BaseComponentId] = allFormComponents.collect { case fc @ IsOverseasAddress(_) =>
    fc.id.baseComponentId
  }.toSet

  val postcodeLookup: Set[BaseComponentId] = allFormComponents.collect { case fc @ IsPostcodeLookup(_) =>
    fc.modelComponentId.toAtomicFormComponentId(PostcodeLookup.postcode).baseComponentId
  }.toSet

  val allBooleanExprs: List[BooleanExpr] = brackets.toBrackets.toList.flatMap(_.allBooleanExprs())

  val pageModelLookup: Map[SectionNumber, PageModel] = pagesWithIndex.toList.map(_.swap).toMap

  val fcIdRepeatsExprLookup: Map[FormComponentId, Expr] = brackets.repeatingPageBrackets.flatMap { repeatingBracket =>
    repeatingBracket.singletons.toList.flatMap(
      _.singleton.page.allFields.map(fc => (fc.id, repeatingBracket.source.repeats))
    )
  }.toMap

  val constraints: Map[BaseComponentId, TextConstraint] = allFormComponents.collect {
    case fc @ HasConstraint(constraint) =>
      fc.id.baseComponentId -> constraint
  }.toMap

  def map(
    e: Singleton => Singleton
  )(
    f: CheckYourAnswers => CheckYourAnswers
  )(
    g: Repeater => Repeater
  ): FormModel = new FormModel(
    brackets.map(e)(f)(g)
  )

  def apply(sectionNumber: SectionNumber): PageModel = pageModelLookup(sectionNumber)

  def bracket(sectionNumber: SectionNumber): Bracket =
    brackets.withSectionNumber(sectionNumber)

  def addToListBrackets: List[Bracket.AddToList] = brackets.addToListBrackets
  def addToListIds: Set[AddToListId] = addToListBrackets.map(_.source.id).toSet
  def nonRepeatingPageBrackets: List[Bracket.NonRepeatingPage] = brackets.nonRepeatingPageBrackets
  def repeatingPageBrackets: List[Bracket.RepeatingPage] = brackets.repeatingPageBrackets

  def isDefinedAt(modelComponentId: ModelComponentId): Boolean = allModelComponentIds(modelComponentId)

  val pageLookup: Map[FormComponentId, PageModel] =
    pages.foldLeft(Map.empty[FormComponentId, PageModel])(_ ++ _.pageLookup)

  def find(modelComponentId: ModelComponentId): Option[FormComponent] =
    fcLookup.get(modelComponentId.toFormComponentId)

  def findBigger(indexedComponentId: IndexedComponentId): List[FormComponent] =
    allFormComponents.filter { formComponent =>
      val indexed = formComponent.modelComponentId.indexedComponentId
      indexed.baseComponentId === indexedComponentId.baseComponentId &&
      indexed.maybeIndex.fold(false)(index => indexedComponentId.maybeIndex.fold(false)(_ < index))
    }

  def allFormComponentsExceptFromPage(pageModel: PageModel): List[FormComponent] = {
    val pageIds = pageModel.allFormComponentIds.toSet
    allFormComponents.filterNot(fc => pageIds.contains(fc.id))
  }

  def visibleSectionNumber(sectionNumber: SectionNumber): SectionNumber =
    if (availableSectionNumbers.contains(sectionNumber)) {
      sectionNumber
    } else {
      // User is trying to see invisible page, so we need to send him to appropriate SectionNumber instead
      availableSectionNumbers match {
        case Nil       => throw new IllegalArgumentException(s"Cannot find valid sectionNumber for $sectionNumber.")
        case head :: _ => availableSectionNumbers.findLast(_ < sectionNumber).getOrElse(head)
      }
    }

  val sectionNumberLookup: Map[FormComponentId, SectionNumber] =
    pagesWithIndex.toList.foldLeft(Map.empty[FormComponentId, SectionNumber]) { case (acc, (pm, sn)) =>
      acc ++ pm.allFormComponentIds.map(fcId => fcId -> sn).toMap
    }

  val addToListSectionNumbers = addToListBrackets.flatMap(_.toPageModelWithNumber.toList).map(_._2)
  val addToListRepeaterSectionNumbers = addToListBrackets.flatMap(_.iterations.toList).map(_.repeater.sectionNumber)
}

object FormModel {

  def fromEnrolmentSection(enrolmentSection: EnrolmentSection): FormModel = {
    val singleton = Singleton(enrolmentSection.toPage)
    new FormModel(
      Brackets.Classic(
        NonEmptyList.one(
          Bracket.NonRepeatingPage(
            SingletonWithNumber(singleton, SectionNumber.classicZero),
            enrolmentSection.toSection
          )
        )
      )
    )
  }
}

private object HasIncludeIf {
  def unapply(pageModel: PageModel): Option[IncludeIf] =
    pageModel.fold(_.page.includeIf)(_ => None)(_ => None)
}
