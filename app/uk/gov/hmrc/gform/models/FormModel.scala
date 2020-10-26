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
import uk.gov.hmrc.gform.eval.{ AllPageModelExpressions, ExprMetadata, ExprType, RevealingChoiceInfo, StandaloneSumInfo, StaticTypeData, StaticTypeInfo, SumInfo, TypeInfo }
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId, MultiValueId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class FormModel[A <: PageMode](
  pagesWithIndex: List[(PageModel[A], SectionNumber)],
  staticTypeInfo: StaticTypeInfo,
  revealingChoiceInfo: RevealingChoiceInfo,
  sumInfo: SumInfo,
  standaloneSumInfo: StandaloneSumInfo // This represents ${abc.sum} expressions which are not in "value" property of FormComponent
) {

  val (pages, availableSectionNumbers) = pagesWithIndex.unzip

  val pagesMap: Map[PageModel[A], SectionNumber] = pagesWithIndex.toMap

  val allFormComponents: List[FormComponent] = pages.flatMap(_.allFormComponents)

  val allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  val fcLookup: Map[FormComponentId, FormComponent] =
    allFormComponents.map(fc => fc.id -> fc).toMap

  val allMultiValueIds: List[MultiValueId] =
    allFormComponents.map(_.multiValueId)

  val allModelComponentIds: Set[ModelComponentId] = allMultiValueIds.flatMap(_.toModelComponentIds).toSet

  val allMultiSelectionIds: Set[ModelComponentId] = allFormComponents
    .collect {
      case fc @ IsChoice(_)          => fc.id
      case fc @ IsRevealingChoice(_) => fc.id
    }
    .map(_.modelComponentId)
    .toSet

  val exprsMetadata: List[ExprMetadata] = pages.flatMap {
    case AllPageModelExpressions(exprs) => exprs
    case _                              => Nil
  }

  private val pageModelLookup: Map[SectionNumber, PageModel[A]] = pagesWithIndex.map(_.swap).toMap

  def map[B <: PageMode](f: PageModel[A] => PageModel[B]): FormModel[B] = FormModel(
    pagesWithIndex.map {
      case (page, sectionNumber) => f(page) -> sectionNumber
    },
    staticTypeInfo,
    revealingChoiceInfo,
    sumInfo,
    standaloneSumInfo
  )
  def flatMap[B <: PageMode](f: PageModel[A] => List[PageModel[B]]): FormModel[B] = {
    val i: List[PageModel[B]] = pagesWithIndex.flatMap { case (page, sectionNumber) => f(page) }
    FormModel.fromPages(i, staticTypeInfo, revealingChoiceInfo, sumInfo)
  }

  def apply(sectionNumber: SectionNumber): PageModel[A] = pageModelLookup(sectionNumber)
  def apply(sectionNumber: Int): PageModel[A] = pageModelLookup(SectionNumber(sectionNumber))

  def isDefinedAt(modelComponentId: ModelComponentId): Boolean = allModelComponentIds(modelComponentId)

  def filter[B <: PageMode](predicate: PageModel[A] => Boolean): FormModel[B] =
    FormModel[A](
      pagesWithIndex.filter { case (page, sectionNumber) => predicate(page) },
      staticTypeInfo,
      revealingChoiceInfo,
      sumInfo,
      standaloneSumInfo
    ).map(_.asInstanceOf[PageModel[B]])

  private def toStaticTypeData(formComponentId: FormComponentId): Option[StaticTypeData] =
    staticTypeInfo.get(formComponentId.baseComponentId)

  def toFirstOperandTypeInfo(expr: Expr): TypeInfo = {
    def illegal = TypeInfo.illegal(expr)
    val first: Option[Expr] = expr.leafs.headOption
    first.fold(illegal) {
      case FormCtx(formComponentId) => explicitTypedExpr(expr, formComponentId)
      case IsNumberConstant(_)      => TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case otherwise                => TypeInfo(expr, StaticTypeData(ExprType.string, None))
    }
  }

  def explicitTypedExpr(expr: Expr, fcId: FormComponentId): TypeInfo = {
    def illegal = TypeInfo.illegal(expr)
    toStaticTypeData(fcId).fold(illegal)(staticTypeData => TypeInfo(expr, staticTypeData))
  }

  def pageLookup: Map[FormComponentId, PageModel[A]] =
    pages.foldLeft(Map.empty[FormComponentId, PageModel[A]])(_ ++ _.pageLookup)

  def find(modelComponentId: ModelComponentId): Option[FormComponent] =
    fcLookup.get(modelComponentId.toFormComponentId)

  def findBigger(indexedComponentId: IndexedComponentId): List[FormComponent] =
    allFormComponents.filter { formComponent =>
      val indexed = formComponent.modelComponentId.indexedComponentId
      indexed.baseComponentId === indexedComponentId.baseComponentId &&
      indexed.maybeIndex.fold(false)(index => indexedComponentId.maybeIndex.fold(false)(_ < index))
    }

  def allFormComponentsExceptFromPage(pageModel: PageModel[A]): List[FormComponent] = {
    val pageIds = pageModel.allFormComponentIds.toSet
    allFormComponents.filterNot(fc => pageIds.contains(fc.id))
  }

  def allIncludeIfsWithDependingFormComponents: List[(IncludeIf, List[FormComponent])] = pages.collect {
    case (pm @ HasIncludeIf(includeIf)) =>
      (includeIf, pm.fold(_.page.fields)(_ => List.empty))
  }

  def allValidIfs: List[(List[ValidIf], FormComponent)] = pages.flatMap(_.allValidIfs)

  def lastSectionNumberWith(addToListId: AddToListId): SectionNumber =
    SectionNumber(pages.lastIndexWhere(pm => pm.fold(_ => false)(r => r.source.id === addToListId)))

  def firstsAddToList: Map[AddToListId, Int] =
    pagesWithIndex.foldRight(Map.empty[AddToListId, Int]) {
      case ((pageModel, SectionNumber(index)), acc) =>
        pageModel.sourceIsAddToList.fold(acc) { addToList =>
          acc + (addToList.id -> index)
        }
    }

  def allAddToList: List[Section.AddToList] =
    pages.flatMap { pageModel =>
      pageModel.sourceIsAddToList
    }.distinct

  def repeaters(addToListId: AddToListId): List[Repeater[A]] = {
    val IsRepeater = new IsRepeater(addToListId)
    pages.collect {
      case IsRepeater(repeater) => repeater
    }
  }

  def repeaterFor(index: Int, addToListId: AddToListId): Option[Repeater[A]] = {
    val IsRepeater = new IsRepeater(addToListId)
    pages.collectFirst {
      case IsRepeater(repeater) if repeater.index === index => repeater
    }
  }

  def repeaterFor(addToListId: AddToListId): Option[Repeater[A]] = {
    val IsRepeater = new IsRepeater(addToListId)
    pages.collectFirst {
      case IsRepeater(repeater) => repeater
    }
  }

  def singletonsBySource: List[(Section, Seq[PageModel[A]])] =
    pages
      .filter(_.fold(_ => true)(_ => false))
      .groupBy(_.source)
      .toList
}

object FormModel {

  def empty[A <: PageMode]: FormModel[A] =
    FormModel.fromPages(
      List.empty[PageModel[A]],
      StaticTypeInfo.empty,
      RevealingChoiceInfo.empty,
      SumInfo.empty
    )

  def fromPages[A <: PageMode](
    pages: List[PageModel[A]],
    staticTypeInfo: StaticTypeInfo,
    revealingChoiceInfo: RevealingChoiceInfo,
    sumInfo: SumInfo
  ): FormModel[A] = {

    val standaloneSumInfo = StandaloneSumInfo.from(pages, sumInfo)

    FormModel(
      pages.zipWithIndex.map { case (page, index) => page -> SectionNumber(index) },
      staticTypeInfo,
      revealingChoiceInfo,
      sumInfo,
      standaloneSumInfo
    )
  }
}

private class IsRepeater(addToListId: AddToListId) {
  def unapply[A <: PageMode](pageModel: PageModel[A]): Option[Repeater[A]] =
    pageModel.repeaterOf(addToListId)
}

private object HasIncludeIf {
  def unapply(pageModel: PageModel[_ <: PageMode]): Option[IncludeIf] =
    pageModel.fold(_.page.includeIf)(_.includeIf)
}
