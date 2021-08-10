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

package uk.gov.hmrc.gform.models

import cats.data.NonEmptyList
import cats.syntax.eq._
import uk.gov.hmrc.gform.eval.{ AllPageModelExpressions, ExprMetadata, ExprType, RevealingChoiceInfo, StandaloneSumInfo, StaticTypeData, StaticTypeInfo, SumInfo, TypeInfo }
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, ModelPageId, MultiValueId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

case class FormModel[A <: PageMode](
  brackets: BracketsWithSectionNumber[A],
  staticTypeInfo: StaticTypeInfo,
  revealingChoiceInfo: RevealingChoiceInfo,
  sumInfo: SumInfo,
  standaloneSumInfo: StandaloneSumInfo // This represents ${abc.sum} expressions which are not in "value" property of FormComponent
) {

  val pagesWithIndex: NonEmptyList[(PageModel[A], SectionNumber)] = brackets.toPageModelWithNumber

  val pageIdSectionNumberMap: Map[ModelPageId, SectionNumber] = pagesWithIndex.toList.flatMap {
    case (pageModel, number) =>
      pageModel.id.map(id => (id.modelPageId, number))
  }.toMap

  val (pages, availableSectionNumbers) = pagesWithIndex.toList.unzip

  val allFormComponents: List[FormComponent] = pages.flatMap(_.allFormComponents)

  val allFormComponentIds: List[FormComponentId] = allFormComponents.map(_.id)

  val fcLookup: Map[FormComponentId, FormComponent] =
    allFormComponents.map(fc => fc.id -> fc).toMap

  val allMultiValueIds: List[MultiValueId] =
    allFormComponents.map(_.multiValueId)

  val allModelComponentIds: Set[ModelComponentId] = allMultiValueIds.flatMap(_.toModelComponentIds).toSet

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
    .collect { case fc @ IsFileUpload() =>
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

  val overseasAddressLookup: Set[BaseComponentId] = allFormComponents.collect { case fc @ IsOverseasAddress(_) =>
    fc.id.baseComponentId
  }.toSet

  val exprsMetadata: List[ExprMetadata] = brackets.toBrackets.toList.flatMap {
    case AllPageModelExpressions(exprMetadatas) => exprMetadatas
    case _                                      => Nil
  }

  private val pageModelLookup: Map[SectionNumber, PageModel[A]] = pagesWithIndex.toList.map(_.swap).toMap

  val fcIdRepeatsExprLookup: Map[FormComponentId, Expr] = brackets.repeatingPageBrackets.flatMap { repeatingBracket =>
    repeatingBracket.singletons.toList.flatMap(
      _.singleton.page.fields.map(fc => (fc.id, repeatingBracket.source.repeats))
    )
  }.toMap

  def map[B <: PageMode](f: Singleton[A] => Singleton[B])(g: Repeater[A] => Repeater[B]): FormModel[B] = FormModel(
    BracketsWithSectionNumber(brackets.brackets.map(_.map(f, g))),
    staticTypeInfo,
    revealingChoiceInfo,
    sumInfo,
    standaloneSumInfo
  )

  def flatMapRepeater(
    f: (
      NonEmptyList[BracketPlain.AddToListIteration[A]],
      Section.AddToList
    ) => NonEmptyList[BracketPlain.AddToListIteration[A]]
  ): FormModel[A] = {
    val bracketPlains = brackets.toBrackets.map {
      case BracketPlain.AddToList(iterations, source) => BracketPlain.AddToList(f(iterations, source), source)
      case o                                          => o
    }
    FormModel.fromPages(bracketPlains, staticTypeInfo, revealingChoiceInfo, sumInfo)
  }

  def apply(sectionNumber: SectionNumber): PageModel[A] = pageModelLookup(sectionNumber)
  def apply(sectionNumber: Int): PageModel[A] = pageModelLookup(SectionNumber(sectionNumber))

  def bracket(sectionNumber: SectionNumber): Bracket[A] =
    brackets.withSectionNumber(sectionNumber)

  def addToListBrackets: List[Bracket.AddToList[A]] = brackets.addToListBrackets
  def nonRepeatingPageBrackets: List[Bracket.NonRepeatingPage[A]] = brackets.nonRepeatingPageBrackets
  def repeatingPageBrackets: List[Bracket.RepeatingPage[A]] = brackets.repeatingPageBrackets

  def isDefinedAt(modelComponentId: ModelComponentId): Boolean = allModelComponentIds(modelComponentId)

  def filter[B <: PageMode](predicate: PageModel[A] => Boolean): FormModel[B] = {
    val filtered: List[Bracket[A]] = brackets.brackets.map(_.filter(predicate)).collect { case Some(bracket) =>
      bracket
    }
    NonEmptyList
      .fromList(filtered)
      .fold(throw new IllegalArgumentException("All pages of the form are invisible")) { brackets =>
        FormModel[A](
          BracketsWithSectionNumber(brackets),
          staticTypeInfo,
          revealingChoiceInfo,
          sumInfo,
          standaloneSumInfo
        ).asInstanceOf[FormModel[B]]
      }
  }

  private def toStaticTypeData(formComponentId: FormComponentId): Option[StaticTypeData] =
    staticTypeInfo.get(formComponentId.baseComponentId)

  def toFirstOperandTypeInfo(expr: Expr): TypeInfo = {
    def illegal = TypeInfo.illegal(expr)
    val first: Option[Expr] = expr.firstExprForTypeResolution(this)
    first.fold(illegal) {
      case FormCtx(formComponentId)              => explicitTypedExpr(expr, formComponentId)
      case DateCtx(_)                            => TypeInfo(expr, StaticTypeData(ExprType.dateString, None))
      case IsNumberConstant(_) | PeriodExt(_, _) => TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case Period(_, _) | PeriodValue(_)         => TypeInfo(expr, StaticTypeData(ExprType.period, None))
      case otherwise                             => TypeInfo(expr, StaticTypeData(ExprType.string, None))
    }
  }

  def explicitTypedExpr(expr: Expr, fcId: FormComponentId): TypeInfo = {
    def illegal = TypeInfo.illegal(expr)
    toStaticTypeData(fcId).fold(illegal)(staticTypeData => TypeInfo(expr, staticTypeData))
  }

  val pageLookup: Map[FormComponentId, PageModel[A]] =
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
      (includeIf, pm.fold(_.page.fields)(_.addAnotherQuestion :: Nil))
  }

  def allValidIfs: List[(List[ValidIf], FormComponent)] = pages.flatMap(_.allValidIfs)
  def allComponentIncludeIfs: List[(IncludeIf, FormComponent)] = pages.flatMap(_.allComponentIncludeIfs)

  def visibleSectionNumber(sectionNumber: SectionNumber): SectionNumber =
    if (availableSectionNumbers.contains(sectionNumber)) {
      sectionNumber
    } else {
      // User is trying to see invisible page, so we need to send him to appropriate SectionNumber instead
      availableSectionNumbers match {
        case Nil       => throw new IllegalArgumentException(s"Cannot find valid sectionNumber for $sectionNumber.")
        case head :: _ => availableSectionNumbers.reverse.find(_ < sectionNumber).getOrElse(head)
      }
    }
}

object FormModel {

  def fromEnrolmentSection[A <: PageMode](enrolmentSection: EnrolmentSection): FormModel[A] = {
    val singleton = Singleton(enrolmentSection.toPage).asInstanceOf[Singleton[A]]
    FormModel.fromPages(
      NonEmptyList.one(BracketPlain.NonRepeatingPage(singleton, enrolmentSection.toSection)),
      StaticTypeInfo.empty,
      RevealingChoiceInfo.empty,
      SumInfo.empty
    )
  }

  def fromPages[A <: PageMode](
    brackets: NonEmptyList[BracketPlain[A]],
    staticTypeInfo: StaticTypeInfo,
    revealingChoiceInfo: RevealingChoiceInfo,
    sumInfo: SumInfo
  ): FormModel[A] = {

    val standaloneSumInfo = StandaloneSumInfo.from(brackets, sumInfo)

    val bracketsWithSectionNumber = BracketsWithSectionNumber(Bracket.fromBrackets(brackets))

    FormModel(
      bracketsWithSectionNumber,
      staticTypeInfo,
      revealingChoiceInfo,
      sumInfo,
      standaloneSumInfo
    )
  }
}

private object HasIncludeIf {
  def unapply(pageModel: PageModel[_ <: PageMode]): Option[IncludeIf] =
    pageModel.fold(_.page.includeIf)(_.includeIf)
}
