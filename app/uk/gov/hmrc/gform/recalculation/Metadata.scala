/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.recalculation

import cats.data.NonEmptyList
import scala.util.Try
import uk.gov.hmrc.gform.eval.{ ExprType, StaticTypeData, StaticTypeInfo }
import uk.gov.hmrc.gform.models.DataRetrieveAll
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.ids.{ IndexedComponentId, ModelComponentId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.AuthInfo
import uk.gov.hmrc.gform.sharedmodel.{ DataRetrieve, DataRetrieveId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class Metadata(
  val lookup: Map[BaseComponentId, RefInfo],
  groups: Set[BaseComponentId],
  groupComponents: Set[FormComponentId],
  atomicFields: Set[BaseComponentId],
  val atomicsLookup: Map[FormComponentId, IndexedComponentId => NonEmptyList[ModelComponentId.Atomic]],
  val componentTypeLookup: Map[BaseComponentId, ComponentType],
  val dataRetrieveAll: DataRetrieveAll,
  val staticTypeInfo: StaticTypeInfo,
  val lookupRegister: Map[BaseComponentId, Register],
  val addToListComponentIds: Set[BaseComponentId],
  val addToListComponentIds2: Map[AddToListId, Set[BaseComponentId]],
  val hideChoicesSelected: Set[BaseComponentId], // ids of choices with "hideChoicesSelected": true
  val choiceLookup: Map[BaseComponentId, (FormComponent, Choice)],
  val addToListIds: Set[BaseComponentId],
  val allFileUploads: Set[BaseComponentId], // All file uploads
  val allMultiFileUploads: Set[BaseComponentId] // All multi file uploads
) {

  val addToListIdLookup: Map[BaseComponentId, AddToListId] =
    addToListComponentIds2.flatMap { case (addToListId, baseComponentIds) =>
      baseComponentIds.toList.map { baseComponentId =>
        baseComponentId -> addToListId
      }
    }

  def refInfo(baseComponentId: BaseComponentId): RefInfo = lookup(baseComponentId)
  def isRepeatedField(baseComponentId: BaseComponentId) = refInfo(baseComponentId).isRepeatedField()
  def isGroup(baseComponentId: BaseComponentId): Boolean = groups(baseComponentId)
  def isGroupComponent(formComponentId: FormComponentId): Boolean = groupComponents(formComponentId)
  def isAtomic(baseComponentId: BaseComponentId): Boolean = atomicFields(baseComponentId)

  def booleanExprStaticType(booleanExpr: BooleanExpr): StaticTypeData =
    booleanExpr.allExpressions.map(expr => exprStaticType(expr)).headOption match {
      case None                 => StaticTypeData(ExprType.String, None)
      case Some(staticTypeData) => staticTypeData
    }

  def addToListIdFor(baseComponentId: BaseComponentId): Option[AddToListId] = addToListIdLookup.get(baseComponentId)

  def exprStaticType(expr: Expr): StaticTypeData =
    expr.staticType() match {
      case Sum(expr)                     => exprStaticType(expr)
      case FormCtx(fcId)                 => staticTypeInfo(fcId.baseComponentId)
      case Count(_)                      => StaticTypeData(ExprType.Number, None)
      case Size(_, _)                    => StaticTypeData(ExprType.Number, None)
      case Index(_)                      => StaticTypeData(ExprType.Number, None)
      case IndexOf(fcId, _)              => staticTypeInfo(fcId.baseComponentId)
      case IndexOfInChoice(fcId, _)      => StaticTypeData(ExprType.Number, None)
      case DateFunction(_)               => StaticTypeData(ExprType.Number, None)
      case Between(_, _, _)              => StaticTypeData(ExprType.Number, None)
      case CsvCountryCountCheck(_, _, _) => StaticTypeData(ExprType.Number, None)
      case Typed(_, tpe)                 => StaticTypeData.from(tpe)
      case DataRetrieveCtx(id, attribute) if dataRetrieveAll.isNumber(id, attribute) =>
        StaticTypeData(ExprType.Number, Some(Number()))
      case DataRetrieveCtx(_, DataRetrieve.Attribute("registeredOfficeAddress")) |
          DataRetrieveCtx(_, DataRetrieve.Attribute("agencyAddress")) |
          DataRetrieveCtx(_, DataRetrieve.Attribute("primaryAddress")) =>
        StaticTypeData(ExprType.Address, None)
      case Constant(c) if Try(c.toDouble).isSuccess        => StaticTypeData(ExprType.Number, Some(Number()))
      case ChoicesCount(_)                                 => StaticTypeData(ExprType.Number, None)
      case ChoicesSelected(_)                              => StaticTypeData(ExprType.Number, None)
      case CountSelectedChoices(_)                         => StaticTypeData(ExprType.Number, None)
      case ChoicesAvailable(_, _)                          => StaticTypeData(ExprType.Number, None)
      case AuthCtx(AuthInfo.ItmpAddress)                   => StaticTypeData(ExprType.Address, None)
      case FormTemplateCtx(FormTemplateProp.FileSizeLimit) => StaticTypeData(ExprType.Number, None)
      case Period(_, _, _)                                 => StaticTypeData(ExprType.Number, None)
      case _                                               => StaticTypeData(ExprType.String, None)
    }

}

object Metadata {

  private def formComponentsToRefInfo(
    sections: List[Section],
    maybeCoordinates: Option[Coordinates]
  ): List[(BaseComponentId, RefInfo)] =
    sections.zipWithIndex.flatMap { case (section, index) =>
      section.fold[List[(BaseComponentId, RefInfo)]] { nonRepeatingPage =>
        val refInfo =
          maybeCoordinates match {
            case None    => RefInfo.NonRepeatingPage(TemplateSectionIndex(index))
            case Some(c) => RefInfo.TaskListNonRepeatingPage(c, TemplateSectionIndex(index))
          }
        (nonRepeatingPage.page.allEnterableFormComponents ++ nonRepeatingPage.page.allGroups).map { field =>
          field.id.baseComponentId -> refInfo
        }
      } { repeatingPage =>
        val refInfo = maybeCoordinates match {
          case None    => RefInfo.RepeatingPage(TemplateSectionIndex(index))
          case Some(c) => RefInfo.TaskListRepeatingPage(c, TemplateSectionIndex(index))
        }
        repeatingPage.page.allEnterableFormComponents.map { field =>
          field.id.baseComponentId -> refInfo
        }
      } { addToList =>
        val refInfo = maybeCoordinates match {
          case None    => RefInfo.AddToListPage(TemplateSectionIndex(index))
          case Some(c) => RefInfo.TaskListAddToListPage(c, TemplateSectionIndex(index))
        }
        (addToList.addAnotherQuestion.id.baseComponentId -> refInfo) :: addToList.pages.toList.flatMap { page =>
          page.allEnterableFormComponents.map { field =>
            field.id.baseComponentId -> refInfo
          }
        }
      }
    }

  def fromEnrolmentSection(enrolmentSection: EnrolmentSection): Metadata =
    new Metadata(
      lookup = Map.empty[BaseComponentId, RefInfo],
      groups = Set.empty[BaseComponentId],
      groupComponents = Set.empty[FormComponentId],
      atomicFields = Set.empty[BaseComponentId],
      atomicsLookup = Map.empty[FormComponentId, IndexedComponentId => NonEmptyList[ModelComponentId.Atomic]],
      componentTypeLookup = Map.empty[BaseComponentId, ComponentType],
      dataRetrieveAll = DataRetrieveAll.empty,
      staticTypeInfo = StaticTypeInfo.empty,
      lookupRegister = Map.empty[BaseComponentId, Register],
      addToListComponentIds = Set.empty[BaseComponentId],
      addToListComponentIds2 = Map.empty[AddToListId, Set[BaseComponentId]],
      hideChoicesSelected = Set.empty[BaseComponentId],
      choiceLookup = Map.empty[BaseComponentId, (FormComponent, Choice)],
      addToListIds = Set.empty[BaseComponentId],
      allFileUploads = Set.empty[BaseComponentId],
      allMultiFileUploads = Set.empty[BaseComponentId]
    )

  def from(formTemplate: FormTemplate): Metadata = {

    val declarationSectionFields: List[FormComponent] =
      formTemplate.destinations.fold(destinationList =>
        destinationList.declarationSection.fold(List.empty[FormComponent])(_.toPage.allEnterableFormComponents)
      )(destinationPrint => List.empty[FormComponent])

    val declarationSectionRefInfos: Map[BaseComponentId, RefInfo] = declarationSectionFields.map { field =>
      field.id.baseComponentId -> RefInfo.NonRepeatingPage(TemplateSectionIndex(-1))
    }.toMap

    val formKind = formTemplate.formKind

    val refInfos: Map[BaseComponentId, RefInfo] = formKind.fold { classic =>
      formComponentsToRefInfo(classic.sections, None).toMap
    } { taskList =>
      val sections: List[(BaseComponentId, RefInfo)] = taskList.sections.toList.zipWithIndex.flatMap {
        case (taskSection, taskSectionNumber) =>
          taskSection.tasks.toList.zipWithIndex.flatMap { case (task, taskNumber) =>
            val coordinates = Coordinates(TaskSectionNumber(taskSectionNumber), TaskNumber(taskNumber))
            formComponentsToRefInfo(task.sections.toList, Some(coordinates))
          }

      }
      sections.toMap
    } ++ declarationSectionRefInfos

    val groups: Set[BaseComponentId] = formKind.fold { classic =>
      classic.sections.flatMap { section =>
        section.fold[List[BaseComponentId]](nonRepeatingPage =>
          nonRepeatingPage.page.fields.flatMap {
            case fc @ IsGroup(_) => fc.id.baseComponentId :: Nil
            case _               => List.empty[BaseComponentId]
          }
        )(repeatingPage => List.empty[BaseComponentId])(addToList => List.empty[BaseComponentId])
      }
    }(taskList => List.empty[BaseComponentId]).toSet

    val groupComponents: Set[FormComponentId] = formKind.fold { classic =>
      classic.sections.flatMap { section =>
        section.fold[List[FormComponentId]](nonRepeatingPage =>
          nonRepeatingPage.page.fields.flatMap {
            case IsGroup(group) => group.fields.map(_.id)
            case _              => List.empty[FormComponentId]
          }
        )(repeatingPage => List.empty[FormComponentId])(addToList => List.empty[FormComponentId])
      }
    }(taskList => List.empty[FormComponentId]).toSet

    val atomicFields: Set[BaseComponentId] =
      formKind.allEnterableFields.collect { case fc @ IsMultiField(_) =>
        fc.id.baseComponentId
      }.toSet

    val atomicsLookup: Map[FormComponentId, IndexedComponentId => NonEmptyList[ModelComponentId.Atomic]] =
      formKind.allEnterableFields.collect { case fc @ IsMultiField(multi) =>
        fc.id -> multi.fields _
      }.toMap

    val allEnterableFields =
      formKind.allEnterableFields ++ formKind.allAddAnotherQuestions ++ formKind.allGroups ++ declarationSectionFields

    val componentTypeLookup: Map[BaseComponentId, ComponentType] =
      allEnterableFields.map { fc =>
        fc.id.baseComponentId -> fc.`type`
      }.toMap

    val dataRetrieveLookup: Map[DataRetrieveId, DataRetrieve] = formKind.allSections.sections
      .flatMap { section =>
        section.section.allPages.flatMap(page => page.dataRetrieve.toList.flatMap(_.toList))
      }
      .map(dr => dr.id -> dr)
      .toMap

    val dataRetrieveAll: DataRetrieveAll = DataRetrieveAll(dataRetrieveLookup)

    val staticTypeInfo =
      allEnterableFields
        .map(fc => fc.id.baseComponentId -> fc.staticTypeData)
        .toMap

    val lookupRegister: Map[BaseComponentId, Register] = formKind.allEnterableFields.collect {
      case fc @ HasLookupRegister(register) =>
        fc.id.baseComponentId -> register
    }.toMap

    val addToListComponentIds: Set[BaseComponentId] = formKind.allSections.sections.flatMap { section =>
      section.section.fold[Set[BaseComponentId]](_ => Set.empty)(_ => Set.empty) { addToList =>
        addToList.allIds.map(_.baseComponentId).toSet
      }
    }.toSet

    val addToListComponentIds2: Map[AddToListId, Set[BaseComponentId]] = formKind.allSections.sections.flatMap {
      section =>
        section.section.fold[List[(AddToListId, Set[BaseComponentId])]](_ => List.empty)(_ => List.empty) { addToList =>
          List(addToList.id -> addToList.allIds.map(_.baseComponentId).toSet)
        }
    }.toMap

    val hideChoicesSelected: Set[BaseComponentId] = formKind.allEnterableFields.collect {
      case fc @ IsChoice(choice) if choice.hideChoicesSelected =>
        fc.modelComponentId.baseComponentId
    }.toSet

    val choiceLookup: Map[BaseComponentId, (FormComponent, Choice)] = formKind.allEnterableFields.collect {
      case fc @ IsChoice(choice) =>
        fc.baseComponentId -> (fc, choice)
    }.toMap

    val addToListIds: Set[BaseComponentId] = formKind.allAddAnotherQuestions.map(_.id.baseComponentId).toSet

    val allFileUploads: Set[BaseComponentId] = formKind.allEnterableFields.collect { case fc @ IsFileUpload(_) =>
      fc.baseComponentId
    }.toSet

    val allMultiFileUploads: Set[BaseComponentId] = formKind.allEnterableFields.collect {
      case fc @ IsMultiFileUpload(_) =>
        fc.baseComponentId
    }.toSet

    new Metadata(
      refInfos,
      groups,
      groupComponents,
      atomicFields,
      atomicsLookup,
      componentTypeLookup,
      dataRetrieveAll,
      StaticTypeInfo(staticTypeInfo),
      lookupRegister,
      addToListComponentIds,
      addToListComponentIds2,
      hideChoicesSelected,
      choiceLookup,
      addToListIds,
      allFileUploads,
      allMultiFileUploads
    )
  }
}
