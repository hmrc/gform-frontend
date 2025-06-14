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
import uk.gov.hmrc.gform.eval.{ AllPageModelExpressions, ExprMetadata, ExprType, RevealingChoiceInfo, StandaloneSumInfo, StaticTypeData, StaticTypeInfo, SumInfo, TypeInfo }
import uk.gov.hmrc.gform.models.FormModel.modelComponentsToIndexedComponentMap
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, IndexedComponentId, ModelComponentId, ModelPageId, MultiValueId }
import uk.gov.hmrc.gform.sharedmodel.DataRetrieve
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.collection.mutable

case class FormModel[A <: PageMode](
  brackets: BracketsWithSectionNumber[A],
  staticTypeInfo: StaticTypeInfo,
  revealingChoiceInfo: RevealingChoiceInfo,
  sumInfo: SumInfo,
  standaloneSumInfo: StandaloneSumInfo, // This represents ${abc.sum} expressions which are not in "value" property of FormComponent
  dataRetrieve: Option[NonEmptyList[DataRetrieve]]
) {

  val pagesWithIndex: NonEmptyList[(PageModel[A], SectionNumber)] = brackets.toPageModelWithNumber

  val pageIdSectionNumberMap: Map[ModelPageId, SectionNumber] = pagesWithIndex.toList.flatMap {
    case (pageModel, number) =>
      pageModel.id.map(id => (id.modelPageId, number))
  }.toMap

  // Reverse lookup to find out SectionNumber which is confirmed by given PageId
  val reverseConfirmationMap: Map[ModelPageId, ConfirmationPage.Confirmee] = pagesWithIndex.toList.flatMap {
    case (pageModel, sectionNumber) =>
      pageModel.maybeConfirmation.toList.flatMap(confirmation =>
        confirmation.redirects.toList
          .flatMap(_.toList)
          .map(
            _.pageId.modelPageId -> ConfirmationPage.Confirmee(sectionNumber, confirmation)
          )
      )
  }.toMap

  val confirmationPageMap = pagesWithIndex.toList.flatMap { case (pageModel, sectionNumber) =>
    pageModel.maybeConfirmation.map { confirmation =>
      sectionNumber -> confirmation
    }
  }

  val (pages, availableSectionNumbers) = pagesWithIndex.toList.unzip

  object taskList {

    def availablePages(coordinates: Coordinates): List[PageModel[A]] = {
      val taskModel: TaskModel[A] = brackets.unsafeToTaskList.bracketsFor(coordinates)
      taskModel match {
        case TaskModel.AllHidden() => List.empty[PageModel[A]]
        case TaskModel.Editable(xs) =>
          val pagesWithIndex: NonEmptyList[(PageModel[A], SectionNumber)] = xs.flatMap(_.toPageModelWithNumber)
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

  def allUpperCaseIds: Set[ModelComponentId] =
    allFormComponents.collect { case fc @ IsCapitalised() =>
      fc.modelComponentId
    }.toSet

  def allIndexedComponentIds: Map[BaseComponentId, List[ModelComponentId]] =
    modelComponentsToIndexedComponentMap(allFormComponents.map(_.modelComponentId))

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

  val lookupRegister: Map[BaseComponentId, Register] = allFormComponents.collect {
    case fc @ HasLookupRegister(register) =>
      fc.id.baseComponentId -> register
  }.toMap

  val postcodeLookup: Set[BaseComponentId] = allFormComponents.collect { case fc @ IsPostcodeLookup(_) =>
    fc.modelComponentId.toAtomicFormComponentId(PostcodeLookup.postcode).baseComponentId
  }.toSet

  val choiceLookup: Map[ModelComponentId, NonEmptyList[OptionData]] = allFormComponents.collect {
    case fc @ IsChoice(choice) =>
      fc.modelComponentId -> choice.options
  }.toMap

  val hideChoicesSelected: Set[ModelComponentId] = allFormComponents.collect {
    case fc @ IsChoice(choice) if choice.hideChoicesSelected =>
      fc.modelComponentId
  }.toSet

  val exprsMetadata: List[ExprMetadata] = brackets.toBrackets.toList.flatMap {
    case AllPageModelExpressions(exprMetadatas) => exprMetadatas
    case _                                      => Nil
  }

  val dataRetrieveAll: DataRetrieveAll = DataRetrieveAll.from(this)

  val pageModelLookup: Map[SectionNumber, PageModel[A]] = pagesWithIndex.toList.map(_.swap).toMap

  val fcIdRepeatsExprLookup: Map[FormComponentId, Expr] = brackets.repeatingPageBrackets.flatMap { repeatingBracket =>
    repeatingBracket.singletons.toList.flatMap(
      _.singleton.page.allFields.map(fc => (fc.id, repeatingBracket.source.repeats))
    )
  }.toMap

  val constraints: Map[BaseComponentId, TextConstraint] = allFormComponents.collect {
    case fc @ HasConstraint(constraint) =>
      fc.id.baseComponentId -> constraint
  }.toMap

  def map[B <: PageMode](
    e: Singleton[A] => Singleton[B]
  )(
    f: CheckYourAnswers[A] => CheckYourAnswers[B]
  )(
    g: Repeater[A] => Repeater[B]
  ): FormModel[B] = FormModel(
    brackets.map(e)(f)(g),
    staticTypeInfo,
    revealingChoiceInfo,
    sumInfo,
    standaloneSumInfo,
    dataRetrieve
  )

  def flatMapRepeater(
    f: (
      NonEmptyList[Bracket.AddToListIteration[A]],
      Section.AddToList
    ) => NonEmptyList[Bracket.AddToListIteration[A]]
  ): FormModel[A] = {

    def applyFToRepeater(brackets: NonEmptyList[Bracket[A]]): NonEmptyList[Bracket[A]] =
      brackets.map {
        case Bracket.AddToList(iterations, source) =>
          Bracket.AddToList(f(iterations, source), source)
        case o => o
      }

    def bracketPlainCoordinated: BracketPlainCoordinated[A] =
      brackets.toBracketPlainCoordinated.fold[BracketPlainCoordinated[A]] { classic =>
        BracketPlainCoordinated.Classic[A](applyFToRepeater(classic.brackets))
      } { taskList =>
        BracketPlainCoordinated.TaskList[A](taskList.coordinatedBrackets.map { case (coor, taskModelCoordinated) =>
          coor -> taskModelCoordinated.modifyBrackets(applyFToRepeater)
        })
      }

    FormModel.fromPages(bracketPlainCoordinated, staticTypeInfo, revealingChoiceInfo, sumInfo, dataRetrieve)
  }

  def apply(sectionNumber: SectionNumber): PageModel[A] =
    pageModelLookup(sectionNumber)

  def bracket(sectionNumber: SectionNumber): Bracket[A] =
    brackets.withSectionNumber(sectionNumber)

  def addToListBrackets: List[Bracket.AddToList[A]] = brackets.addToListBrackets
  def addToListIds: Set[AddToListId] = addToListBrackets.map(_.source.id).toSet
  def nonRepeatingPageBrackets: List[Bracket.NonRepeatingPage[A]] = brackets.nonRepeatingPageBrackets
  def repeatingPageBrackets: List[Bracket.RepeatingPage[A]] = brackets.repeatingPageBrackets

  def isDefinedAt(modelComponentId: ModelComponentId): Boolean = allModelComponentIds(modelComponentId)

  def filter[B <: PageMode](predicate: PageModel[A] => Boolean): FormModel[B] = {

    def applyPredicate(brackets: NonEmptyList[Bracket[A]]): List[Bracket[A]] =
      brackets.map(_.filter(predicate)).collect { case Some(bracket) =>
        bracket
      }

    val bracketsWithSectionNumber = brackets.fold[BracketsWithSectionNumber[A]] { classic =>
      val filtered: List[Bracket[A]] = applyPredicate(classic.brackets)
      NonEmptyList
        .fromList(filtered)
        .fold(throw new IllegalArgumentException("All pages of the form are invisible")) { brackets =>
          BracketsWithSectionNumber.Classic(brackets)
        }
    } { taskList =>
      BracketsWithSectionNumber.TaskList {
        taskList.brackets.map { case (coor, taskModel) =>
          taskModel match {
            case TaskModel.Editable(brackets) => coor -> TaskModel(applyPredicate(brackets))
            case TaskModel.AllHidden()        => coor -> TaskModel.AllHidden[A]()
          }
        }
      }
    }
    FormModel[A](
      bracketsWithSectionNumber,
      staticTypeInfo,
      revealingChoiceInfo,
      sumInfo,
      standaloneSumInfo,
      dataRetrieve
    ).asInstanceOf[FormModel[B]]
  }

  private def toStaticTypeData(formComponentId: FormComponentId): Option[StaticTypeData] =
    staticTypeInfo.get(formComponentId.baseComponentId)

  def toFirstOperandTypeInfo(expr: Expr): TypeInfo = {
    def illegal = TypeInfo.illegal(expr)
    val first: Option[Expr] = expr.firstExprForTypeResolution(this)
    first.fold(illegal) {
      case FormCtx(formComponentId) => explicitTypedExpr(expr, formComponentId)
      case DateCtx(_)               => TypeInfo(expr, StaticTypeData(ExprType.dateString, None))
      case IsNumberConstant(_) | PeriodExt(_, _) | UserCtx(UserField.Enrolment(_, _, Some(UserFieldFunc.Count))) |
          Size(_, _) | CsvCountryCountCheck(_, _, _) | Count(_) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case DataRetrieveCtx(_, DataRetrieve.Attribute("registeredOfficeAddress")) |
          DataRetrieveCtx(_, DataRetrieve.Attribute("agencyAddress")) =>
        TypeInfo(expr, StaticTypeData(ExprType.address, None))
      case DataRetrieveCtx(id, attribute) if dataRetrieveAll.isNumber(id, attribute) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case IndexOfDataRetrieveCtx(DataRetrieveCtx(id, attribute), _) if dataRetrieveAll.isNumber(id, attribute) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case IndexOfDataRetrieveCtx(DataRetrieveCtx(id, attribute), _) if dataRetrieveAll.isDate(id, attribute) =>
        TypeInfo(expr, StaticTypeData(ExprType.dateString, Some(Number())))
      case DataRetrieveCtx(id, attribute) if dataRetrieveAll.isDate(id, attribute) =>
        TypeInfo(expr, StaticTypeData(ExprType.dateString, Some(Number())))
      case DataRetrieveCount(_) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, Some(Number())))
      case Period(_, _) | PeriodValue(_) => TypeInfo(expr, StaticTypeData(ExprType.period, None))
      case Between(_, _, _)              => TypeInfo(expr, StaticTypeData(ExprType.number, None))
      case Typed(_, tpe)                 => TypeInfo(expr, StaticTypeData.from(tpe))
      case DateFunction(_)               => TypeInfo(expr, StaticTypeData(ExprType.number, None))
      case ChoicesSelected(_) | ChoicesAvailable(_) | CountSelectedChoices(_) =>
        TypeInfo(expr, StaticTypeData(ExprType.number, None))
      case IndexOf(formComponentId, _)   => explicitTypedExpr(expr, formComponentId)
      case AuthCtx(AuthInfo.ItmpAddress) => TypeInfo(expr, StaticTypeData(ExprType.address, None))
      case otherwise                     => TypeInfo(expr, StaticTypeData(ExprType.string, None))
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
  def allChoiceIncludeIfs: List[(IncludeIf, FormComponent)] = pages.flatMap(_.allChoiceIncludeIfs)

  def allMiniSummaryListIncludeIfs: List[(IncludeIf, FormComponent)] = pages.flatMap(_.allMiniSummaryListIncludeIfs)

  def allIncludeIfsWithDependingFormComponents: List[(IncludeIf, List[FormComponent])] = pages.collect {
    case pm @ HasIncludeIf(includeIf) =>
      (includeIf, pm.fold(_.page.allFields)(_ => Nil)(_.addAnotherQuestion :: Nil))
  } ++ allChoiceIncludeIfs.map(i => (i._1, List(i._2))) ++ allMiniSummaryListIncludeIfs.map(i => (i._1, List(i._2)))

  def allValidIfs: List[(List[ValidIf], FormComponent)] = pages.flatMap(_.allValidIfs)
  def allComponentIncludeIfs: List[(IncludeIf, FormComponent)] =
    pages.flatMap(_.allComponentIncludeIfs)

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

  def modelComponentsToIndexedComponentMap(
    modelComponentList: List[ModelComponentId]
  ): Map[BaseComponentId, List[ModelComponentId]] = {
    val map: mutable.Map[BaseComponentId, mutable.ListBuffer[ModelComponentId]] = mutable.Map.empty
    modelComponentList.foreach {
      case mcId if mcId.indexedComponentId.isIndexed =>
        val bcId = mcId.baseComponentId
        map.getOrElseUpdate(bcId, mutable.ListBuffer.empty) += mcId
      case _ => ()
    }
    map
      .foldLeft(Map.newBuilder[BaseComponentId, List[ModelComponentId]]) { case (builder, (bcId, lmcId)) =>
        builder.addOne((bcId, lmcId.toList))
      }
      .result()
  }

  def fromEnrolmentSection[A <: PageMode](enrolmentSection: EnrolmentSection): FormModel[A] = {
    val singleton = Singleton(enrolmentSection.toPage).asInstanceOf[Singleton[A]]
    FormModel.fromPages(
      BracketPlainCoordinated.Classic(
        NonEmptyList.one(
          Bracket.NonRepeatingPage(
            SingletonWithNumber(singleton, SectionNumber.classicZero),
            enrolmentSection.toSection
          )
        )
      ),
      StaticTypeInfo.empty,
      RevealingChoiceInfo.empty,
      SumInfo.empty,
      None
    )
  }

  def fromPages[A <: PageMode](
    brackets: BracketPlainCoordinated[A],
    staticTypeInfo: StaticTypeInfo,
    revealingChoiceInfo: RevealingChoiceInfo,
    sumInfo: SumInfo,
    dataRetrieve: Option[NonEmptyList[DataRetrieve]]
  ): FormModel[A] = {

    def standaloneSumInfo: StandaloneSumInfo = StandaloneSumInfo.from(brackets, sumInfo)

    val bracketsWithSectionNumber = BracketsWithSectionNumber.fromBracketCoordinated(brackets)

    FormModel(
      bracketsWithSectionNumber,
      staticTypeInfo,
      revealingChoiceInfo,
      sumInfo,
      standaloneSumInfo,
      dataRetrieve
    )
  }
}

private object HasIncludeIf {
  def unapply(pageModel: PageModel[_ <: PageMode]): Option[IncludeIf] =
    pageModel.fold(_.page.includeIf)(_ => None)(_.includeIf)
}
