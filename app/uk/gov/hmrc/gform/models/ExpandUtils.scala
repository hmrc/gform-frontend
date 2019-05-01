/*
 * Copyright 2019 HM Revenue & Customs
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

import cats.syntax.eq._
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.sharedmodel.form.{ FormData, FormDataRecalculated, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ExpandUtils {

  def nonSubmittedFCsOfNonGroup(data: FormDataRecalculated, section: Section): List[FormComponent] = {
    val groupFields: List[List[FormComponent]] =
      section.fields.collect {
        case IsGroup(group) => group.fields
      }

    (section.fields ++ groupFields.flatten).filter {
      case IsGroup(_)      => false
      case IsMultiField(_) => false
      case fc              => !data.data.contains(fc.id)
    }
  }

  def submittedFCs(data: FormDataRecalculated, formComponents: List[FormComponent]): List[FormComponent] = {
    val fcIds: Set[FormComponentId] = data.data.keys.toSet

    formComponents.filter {
      case fc @ IsMultiField(mf) => mf.fields(fc.id).forall(fcIds)
      case fc                    => fcIds(fc.id)
    }
  }

  def getAlwaysEmptyHiddenGroup(
    data: FormDataRecalculated,
    section: Section,
    lookupExtractors: LookupExtractors): List[FormComponent] = {
    val aeh = alwaysEmptyHidden(data, section) _
    aeh({ case IsInformationMessage(info)               => info }) ++ // It is safe to include hidden fields for info messages, since they are not submissible
      aeh({ case IsChoice(choice)                       => choice }) ++
      aeh({ case lookupExtractors.IsRadioLookup(lookup) => lookup }) ++
      aeh({ case IsFileUpload()                         => () })
  }

  private def alwaysEmptyHidden[A](data: FormDataRecalculated, section: Section)(
    pf: PartialFunction[FormComponent, A]): List[FormComponent] = {

    val (groupFcs, groups): (List[FormComponent], List[Group]) = section.fields.collect {
      case fc @ IsGroup(group) => (fc, group)
    } unzip

    val fieldsInGroups: List[A] = groups.flatMap(_.fields).collect(pf)

    val formComponents: List[FormComponent] = groupFcs.flatMap(_.expandFormComponent(data.data).formComponents)
    val filtered = formComponents.filter(fc => pf.lift(fc).isDefined)

    val fcIds: Set[FormComponentId] = data.data.keys.toSet
    val present = fcIds.filter(key => filtered.exists(_.id == key))
    filtered.take(Math.max(fieldsInGroups.size, present.size))
  }

  def getAlwaysEmptyHidden(section: Section, lookupExtractors: LookupExtractors): List[FormComponent] =
    section.fields.filter {
      case IsChoice(_)                       => true
      case lookupExtractors.IsRadioLookup(_) => true
      case _                                 => false
    }

  def hiddenFileUploads(section: Section): List[FormComponent] =
    section.fields.filter {
      case IsFileUpload() => true
      case _              => false
    }

  def findFormComponent(targetFcId: FormComponentId, sections: List[Section]): Option[FormComponent] =
    sections.flatMap(_.fields).find(_.id == targetFcId)

  private val NumericPrefix = "^(\\d+)_.*".r

  private def hasPrefix(n: Int, fcId: FormComponentId): Boolean =
    fcId.value.startsWith(n.toString + "_")

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
      case fc @ IsMultiField(mf) => f(mf.fields(fc.id).toList)
      case fc                    => f(List(fc.id))
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

  private def occurrenceCount(ids: List[FormComponentId])(formData: FormDataRecalculated): Int = {
    val res = formData.data.keys
      .filter { fcId =>
        ids.contains(stripAnyPrefix(fcId))
      }
    res.size / ids.size
  }

  def addNextGroup(
    maybeGroupFc: Option[FormComponent],
    data: FormDataRecalculated,
    lookupExtractors: LookupExtractors): (FormDataRecalculated, Option[String]) =
    maybeGroupFc match {
      case Some(groupFC @ IsGroup(group)) =>
        // We do not have and an index we are adding. We need to derive it from data
        val index = {
          val fs: List[FormDataRecalculated => Int] = groupIds(group, fcIds => occurrenceCount(fcIds) _)
          val existingData = fs.map(_(data)).sum
          existingData / group.fields.size
        }

        val addedGroupIds = groupIndex(index + 1, group)
        val newData = data.data ++ addedGroupIds.map(fcId => fcId -> Seq("")).toMap

        val anchor = group.fields
          .dropWhile {
            case IsInformationMessage(_) => true
            case _                       => false
          }
          .headOption
          .map {
            case fc @ IsMultiField(mf)                   => mf.fields(fc.id).head
            case fc @ IsChoice(_)                        => fc.id.appendIndex(0)
            case fc @ lookupExtractors.IsRadioLookup(tt) => fc.id.appendIndex(0)
            case fc @ IsHmrcTaxPeriod(_)                 => fc.id
            case fc                                      => fc.id
          } map (fcId => index + "_" + fcId.value)

        (data.copy(recData = data.recData.copy(data = newData)), anchor)
      case None => (data, None)
    }

  def removeGroupFromData(
    idx: Int,
    maybeGroupFc: Option[FormComponent],
    data: FormDataRecalculated): FormDataRecalculated =
    maybeGroupFc match {
      case None => data
      case Some(groupFC @ IsGroup(group)) =>
        val allGroupFcIds: Set[FormComponentId] = groupFC.expandFormComponent(data.data).allIds.toSet
        val groupIdToRemove = groupIndex(idx, group)

        val updatedData: Map[FormComponentId, Seq[String]] = data.data -- groupIdToRemove
        val remainingGroupIds: Set[FormComponentId] = updatedData.keys.toSet.filter(id => allGroupFcIds(id))

        val groupFcWithoutLast: FormComponent = {
          val groupUpd = group.copy(repeatsMax = Some(remainingGroupIds.size / group.fields.size))
          groupFC.copy(`type` = groupUpd)
        }

        val noGaps: List[FormComponentId] =
          groupFcWithoutLast.expandFormComponent(data.data).allIds.map(appendZeroPrefix).sortBy(_.value)

        val sortedIntersect: List[FormComponentId] = remainingGroupIds.toList.map(appendZeroPrefix).sortBy(_.value)

        val updatedMap: Map[FormComponentId, Seq[String]] =
          noGaps
            .zip(sortedIntersect)
            .map {
              case (targetFcId, sourceFcId) =>
                (stripZeroPrefix(targetFcId), updatedData.getOrElse(stripZeroPrefix(sourceFcId), Seq.empty[String]))
            }
            .toMap
        val newData = (updatedData -- allGroupFcIds) ++ updatedMap

        data.copy(recData = data.recData.copy(data = newData))

    }

  def getAllFieldsInGroup(topFieldValue: FormComponent, group: Group, data: FormDataRecalculated): List[GroupList] = {

    val gFCIds: List[FormComponentId] = group.fields.map(_.id)
    val gFC: Set[FormComponentId] = gFCIds.toSet

    val presentOnPage: List[FormComponent] =
      submittedFCs(data, topFieldValue.expandFormComponent(data.data).formComponents)

    val baseFieldPresentOnPage = {
      val alreadyPresent = presentOnPage.map(_.id).toSet
      group.fields.filterNot(field => alreadyPresent.contains(field.id))
    }

    val grouped: Map[Option[FormComponentId], List[FormComponent]] =
      (baseFieldPresentOnPage ++ presentOnPage).groupBy(fc => gFC.find(id => stripAnyPrefix(fc.id) == id))

    grouped.values.toList.transpose.map(componentList => sortGroupList(componentList, gFCIds))
  }

  private def sortGroupList(componentList: List[FormComponent], base: List[FormComponentId]): GroupList =
    GroupList(base.flatMap(fcId => componentList.find(fc => stripAnyPrefix(fc.id) === fcId)))

  def fillToMin(groupLists: List[GroupList], group: Group): List[GroupList] =
    group.repeatsMin match {
      case Some(min) if min > groupLists.size =>
        groupLists ++ (groupLists.size until min).toList.map(index => groupListWithPrefix(index, group))
      case _ => groupLists
    }
}
