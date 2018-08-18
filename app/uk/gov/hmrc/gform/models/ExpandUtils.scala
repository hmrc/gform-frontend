/*
 * Copyright 2018 HM Revenue & Customs
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

import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormField, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ExpandUtils {

  def submittedFCs(
    data: Map[FormComponentId, Seq[String]],
    formComponents: List[FormComponent]): List[FormComponent] = {
    val fcIds: Set[FormComponentId] = data.keys.toSet

    formComponents.filter {
      case fc @ IsDate(_)       => Date.fields(fc.id).forall(ds => fcIds.exists(_ == ds))
      case fc @ IsAddress(_)    => Address.fields(fc.id).forall(ds => fcIds.exists(_ == ds))
      case fc @ IsUkSortCode(_) => UkSortCode.fields(fc.id).forall(ds => fcIds.exists(_ == ds))
      case fc                   => fcIds.exists(_ == fc.id)
    }
  }

  def getAlwaysEmptyHidden(data: Map[FormComponentId, Seq[String]], section: Section): List[FormComponent] = {
    val aeh = alwaysEmptyHidden(data, section) _
    aeh({ case IsInformationMessage(info) => info }) ++ // It is safe to include hidden fields for info messages, since they are not submissible
      aeh({ case IsChoice(choice)         => choice }) ++
      aeh({ case IsFileUpload()           => () })
  }

  private def alwaysEmptyHidden[A](data: Map[FormComponentId, Seq[String]], section: Section)(
    pf: PartialFunction[FormComponent, A]): List[FormComponent] = {

    val (groupFcs, groups): (List[FormComponent], List[Group]) = section.fields.collect {
      case fc @ IsGroup(group) => (fc, group)
    } unzip

    val fieldsInGroups: List[A] = groups.flatMap(_.fields).collect(pf)

    val formComponents: List[FormComponent] = groupFcs.flatMap(_.expandFormComponent.expandedFC)
    val filtered = formComponents.filter(fc => pf.lift(fc).isDefined)

    val fcIds: Set[FormComponentId] = data.keys.toSet
    val present = fcIds.filter(key => filtered.exists(_.id == key))
    filtered.take(Math.max(fieldsInGroups.size, present.size))
  }

  def findFormComponent(targetFcId: FormComponentId, sections: List[Section]): Option[FormComponent] =
    sections.flatMap(_.fields).find(_.id == targetFcId)

  private val NumericPrefix = "^(\\d+)_.*".r

  private def addPrefix(n: Int, targetFcId: FormComponentId): FormComponentId =
    targetFcId.value match {
      case NumericPrefix(_) => targetFcId
      case _                => FormComponentId(n + "_" + targetFcId.value)
    }

  private def groupListWithPrefix(n: Int, group: Group): GroupList =
    GroupList(group.fields.map(fc => fc.copy(id = addPrefix(n, fc.id))))

  /**
    * Append 0_ prefix to FormComponentId, iff it already doesn't have any other prefix.
    */
  def appendZeroPrefix(targetFcId: FormComponentId): FormComponentId = addPrefix(0, targetFcId)

  /**
    * Strip n_ prefix from FormComponentId.
    */
  def stripAnyPrefix(targetFcId: FormComponentId): FormComponentId =
    targetFcId.value match {
      case NumericPrefix(prefix) => FormComponentId(targetFcId.value.replace(prefix + "_", ""))
      case _                     => targetFcId
    }

  def stripZeroPrefix(targetFcId: FormComponentId): FormComponentId =
    targetFcId.value match {
      case NumericPrefix(prefix) if prefix == "0" => FormComponentId(targetFcId.value.replace("0_", ""))
      case _                                      => targetFcId
    }

  private def groupIds[A, B](group: Group, f: List[FormComponentId] => A): List[A] =
    group.fields.map {
      case fc @ IsDate(_)       => f(Date.fields(fc.id))
      case fc @ IsAddress(_)    => f(Address.fields(fc.id))
      case fc @ IsUkSortCode(_) => f(UkSortCode.fields(fc.id))
      case fc                   => f(List(fc.id))
    }

  /**
    * Returns FormComponentIds of group fields given their index
    */
  def groupIndex(i: Int, group: Group): List[FormComponentId] = {
    val fcs: List[FormComponentId] = groupIds(group, identity).flatten

    i match {
      case 1 => fcs
      case n => fcs.map(targetFcId => addPrefix(n - 1, targetFcId))
    }
  }

  private def occurrenceCount(ids: List[FormComponentId])(formData: FormData): Int = {
    val res = formData.fields
      .filter { formField =>
        ids.exists(_ == stripAnyPrefix(formField.id))
      }
    res.size / ids.size
  }

  def addNextGroup(maybeGroupFc: Option[FormComponent], formData: FormData): (FormData, Option[String]) =
    maybeGroupFc match {
      case Some(groupFC @ IsGroup(group)) =>
        // We do not have and an index we are adding. We need to derive it from data
        val index = {
          val fs: List[FormData => Int] = groupIds(group, fcIds => occurrenceCount(fcIds) _)
          val existingData = fs.map(_(formData)).sum
          existingData / group.fields.size
        }

        val formDataFields =
          groupFC.expandFormComponent.allIds.collect {
            case fcId if fcId.value.startsWith(index.toString) => FormField(fcId, "")
          }

        val anchor = group.fields
          .dropWhile {
            case IsInformationMessage(_) => true
            case _                       => false
          }
          .headOption
          .flatMap {
            case fc @ IsDate(_)       => Date.fields(fc.id).headOption
            case fc @ IsAddress(_)    => Address.fields(fc.id).headOption
            case fc @ IsUkSortCode(_) => UkSortCode.fields(fc.id).headOption
            case fc @ IsChoice(_)     => Some(fc.id.appendIndex(0))
            case fc                   => Some(fc.id)
          } map (fcId => index + "_" + fcId.value)

        (FormData(formDataFields), anchor)
      case None => (FormData(List.empty[FormField]), None)
    }

  def removeGroupFromData(
    idx: Int,
    maybeGroupFc: Option[FormComponent],
    data: Map[FormComponentId, Seq[String]]): Map[FormComponentId, Seq[String]] =
    maybeGroupFc match {
      case None => data
      case Some(groupFC @ IsGroup(group)) =>
        val allGroupFcIds: Set[FormComponentId] = groupFC.expandFormComponent.allIds.toSet
        val groupIdToRemove = groupIndex(idx, group)

        val updatedData: Map[FormComponentId, Seq[String]] = data -- groupIdToRemove
        val remainingGroupIds: Set[FormComponentId] = updatedData.keys.toSet.filter(id => allGroupFcIds.exists(_ == id))

        val groupFcWithoutLast: FormComponent = {
          val groupUpd = group.copy(repeatsMax = Some(remainingGroupIds.size / group.fields.size))
          groupFC.copy(`type` = groupUpd)
        }

        val noGaps: List[FormComponentId] =
          groupFcWithoutLast.expandFormComponent.allIds.map(appendZeroPrefix).sortBy(_.value)

        val sortedIntersect: List[FormComponentId] = remainingGroupIds.toList.map(appendZeroPrefix).sortBy(_.value)

        val updatedMap: Map[FormComponentId, Seq[String]] =
          noGaps
            .zip(sortedIntersect)
            .map {
              case (targetFcId, sourceFcId) =>
                (stripZeroPrefix(targetFcId), updatedData.get(stripZeroPrefix(sourceFcId)).getOrElse(Seq.empty[String]))
            }
            .toMap

        (updatedData -- allGroupFcIds) ++ updatedMap

    }

  def getAllFieldsInGroup(
    topFieldValue: FormComponent,
    group: Group,
    data: Map[FormComponentId, Seq[String]]): List[GroupList] = {

    val gFCIds: List[FormComponentId] = group.fields.map(_.id)
    val gFC: Set[FormComponentId] = gFCIds.toSet

    val presentOnPage: List[FormComponent] = submittedFCs(data, topFieldValue.expandFormComponent.expandedFC)

    val grouped: Map[Option[FormComponentId], List[FormComponent]] =
      presentOnPage.groupBy(fc => gFC.find(id => stripAnyPrefix(fc.id) == id))

    grouped.values.toList.transpose.map(componentList => sortGroupList(componentList, gFCIds))
  }

  private def sortGroupList(componentList: List[FormComponent], base: List[FormComponentId]): GroupList =
    GroupList(base.map(fcId => componentList.find(fc => stripAnyPrefix(fc.id) == fcId)).flatten)

  def fillToMin(groupLists: List[GroupList], group: Group): List[GroupList] =
    group.repeatsMin match {
      case Some(min) if min > groupLists.size =>
        groupLists ++ (groupLists.size until min).toList.map(index => groupListWithPrefix(index, group))
      case _ => groupLists
    }
}
