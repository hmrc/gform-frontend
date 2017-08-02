/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.service

import javax.inject.{ Inject, Singleton }

import uk.gov.hmrc.gform.connectors.SessionCacheConnector
import uk.gov.hmrc.gform.gformbackend.model.FormTemplate
import uk.gov.hmrc.gform.models.components.{ FieldId, FieldValue, Group, _ }
import uk.gov.hmrc.gform.models.{ Section, TextExpression, VariableInContext }
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.util.{ Success, Try }

@Singleton
class RepeatingComponentService @Inject() (val sessionCache: SessionCacheConnector) {

  def getAllSections(formTemplate: FormTemplate, data: Map[FieldId, Seq[String]])(implicit hc: HeaderCarrier) = {
    sessionCache.fetch().map { maybeCacheMap =>
      val cacheMap = maybeCacheMap.getOrElse(CacheMap("Empty", Map.empty))
      formTemplate.sections.flatMap { section =>
        if (isRepeatingSection(section)) {
          generateDynamicSections(section, formTemplate, data, cacheMap)
        } else {
          List(section)
        }
      }
    }
  }

  private def isRepeatingSection(section: Section) = section.repeatsMax.isDefined && section.fieldToTrack.isDefined

  private def generateDynamicSections(section: Section, formTemplate: FormTemplate, data: Map[FieldId, Seq[String]],
    cacheMap: CacheMap)(implicit hc: HeaderCarrier): List[Section] = {
    val max = evaluateExpression(section.repeatsMax.get.expr, formTemplate, data)
    val minRequested = evaluateExpression(section.repeatsMin.getOrElse(TextExpression(Constant("1"))).expr, formTemplate, data)
    val min = if (minRequested <= 0) 1 else minRequested
    val (maybeGroupField, requestedCount) = getRequestedCount(section.fieldToTrack.get, formTemplate, data, cacheMap)
    val count = if (requestedCount >= min && requestedCount <= max) {
      requestedCount
    } else if (max > min && requestedCount > max) {
      max
    } else {
      min
    }
    (1 to count).map { i =>
      copySection(section, i, maybeGroupField, data, cacheMap)
    }.toList
  }

  private def copySection(section: Section, index: Int, groupField: Option[FieldValue], data: Map[FieldId, Seq[String]],
    cacheMap: CacheMap)(implicit hc: HeaderCarrier) = {
    def copyField(field: FieldValue): FieldValue = {
      field.`type` match {
        case grp @ Group(fields, _, _, _, _, _) => field.copy(
          id = FieldId(s"${index}_${field.id.value}"),
          `type` = grp.copy(fields = fields.map(copyField))
        )
        case _ => field.copy(
          id = FieldId(s"${index}_${field.id.value}")
        )
      }
    }

    section.copy(
      title = buildText(Some(section.title), index, groupField, section.fieldToTrack.get, data, cacheMap).getOrElse(""),
      shortName = buildText(section.shortName, index, groupField, section.fieldToTrack.get, data, cacheMap),
      fields = section.fields.map(copyField)
    )
  }

  private def buildText(template: Option[String], index: Int, groupField: Option[FieldValue],
    fieldToTrack: VariableInContext, data: Map[FieldId, Seq[String]], cacheMap: CacheMap)(implicit hc: HeaderCarrier): Option[String] = {
    val groupFieldList = groupField match {
      case Some(field) => cacheMap.getEntry[List[List[FieldValue]]](field.id.value).getOrElse(Nil).flatten //getAllFieldsInGroup(field, field.`type`.asInstanceOf[Group])
      case None => Nil
    }

    val textToInsert = if (groupFieldList.isEmpty) {
      ""
    } else {
      data.getOrElse(groupFieldList(index - 1).id, Seq()).mkString
    }

    template match {
      case Some(text) => Some(
        text.replace("$t", textToInsert).replace("$n", index.toString)
      )
      case None => None
    }
  }

  private def evaluateExpression(expr: Expr, formTemplate: FormTemplate, data: Map[FieldId, Seq[String]])(implicit hc: HeaderCarrier): Int = {
    expr match {
      case Add(expr1, expr2) => evaluateExpression(expr1, formTemplate, data) + evaluateExpression(expr2, formTemplate, data)
      case Multiply(expr1, expr2) => evaluateExpression(expr1, formTemplate, data) * evaluateExpression(expr2, formTemplate, data)
      case FormCtx(fieldId) => getFormFieldIntValue(fieldId, data)
      case Constant(value) => Try(value.toInt) match {
        case Success(intValue) => intValue
        case _ => 0
      }
      //      case AuthCtx(value: AuthInfo) =>
      //      case EeittCtx(value: Eeitt) =>
      case _ => 0
    }
  }

  private def getRequestedCount(id: VariableInContext, formTemplate: FormTemplate, data: Map[FieldId, Seq[String]], cacheMap: CacheMap)(implicit hc: HeaderCarrier): (Option[FieldValue], Int) = {
    // Currently the fieldToTrack field in the Section can contain two types of fields:
    // - Field not in a repeating group. In this case the value typed in this field is used.
    // - Field in a repeating group. In this case the repeating group the fieldToTrack belongs to is linked to the
    //                               repeating Section. New instances in the repeating group create a new section too.

    val repeatingGroupsFound = findRepeatingGroupsContainingField(id.field, formTemplate)

    if (repeatingGroupsFound.isEmpty) {
      (None, getFormFieldIntValue(id.field, data))
    } else {
      val groupFieldValue = repeatingGroupsFound.head
      val fieldsInGroup = cacheMap.getEntry[List[List[FieldValue]]](groupFieldValue.id.value).getOrElse(Nil).flatten
      (Some(groupFieldValue), fieldsInGroup.size)
    }
  }

  private def getFormFieldIntValue(id: String, data: Map[FieldId, Seq[String]]) = {
    data.get(FieldId(id)) match {
      case Some(value) => Try(value.head.toInt) match {
        case Success(intValue) => intValue
        case _ => 0
      }
      case None => 0
    }
  }

  private def findRepeatingGroupsContainingField(id: String, formTemplate: FormTemplate): Set[FieldValue] = {

    def findRepeatingGroups(groupField: Option[FieldValue], fieldList: List[FieldValue]): Set[FieldValue] = {
      fieldList.flatMap { field =>
        field.`type` match {
          case Group(fields, _, repMax, _, _, _) if repMax.isDefined => findRepeatingGroups(Some(field), fields)
          case othertype if groupField.isDefined && field.id.value.equals(id) => List(groupField.get)
          case _ => Nil
        }
      }.toSet
    }

    formTemplate.sections.flatMap(section => findRepeatingGroups(None, section.fields)).toSet
  }

  def appendNewGroup(formGroupId: String)(implicit hc: HeaderCarrier): Future[Option[List[List[FieldValue]]]] = {
    // on the forms, the AddGroup button's name has the following format:
    // AddGroup-(groupFieldId)
    // that's the reason why the extraction below is required
    val startPos = formGroupId.indexOf('-') + 1
    val componentId = formGroupId.substring(startPos)

    for {
      dynamicListOpt <- sessionCache.fetchAndGetEntry[List[List[FieldValue]]](componentId)
      dynamicList = dynamicListOpt.getOrElse(Nil) // Nil should never happen
      cacheMap <- sessionCache.cache[List[List[FieldValue]]](componentId, addGroupEntry(dynamicList))
    } yield cacheMap.getEntry[List[List[FieldValue]]](componentId)
  }

  def removeGroup(formGroupId: String, data: Map[FieldId, scala.Seq[String]])(implicit hc: HeaderCarrier) = {
    // on the forms, the RemoveGroup button's name has the following format:
    // RemoveGroup-(groupFieldId)
    // that's the reason why the extraction below is required
    val groupIdStartPos = formGroupId.indexOf('-') + 1
    val componentId = formGroupId.substring(groupIdStartPos)
    val indexEndPos = componentId.indexOf("_")
    val index = componentId.substring(0, indexEndPos).toInt
    val groupId = componentId.substring(indexEndPos + 1)

    for {
      dynamicListOpt <- sessionCache.fetchAndGetEntry[List[List[FieldValue]]](groupId)
      dynamicList = dynamicListOpt.getOrElse(Nil)
      (newList, newData) = renameFieldIdsAndData(dynamicList diff List(dynamicList(index - 1)), data)
      _ <- sessionCache.cache[List[List[FieldValue]]](groupId, newList)
    } yield newData
  }

  private def renameFieldIdsAndData(list: List[List[FieldValue]], data: Map[FieldId, scala.Seq[String]]): (List[List[FieldValue]], Map[FieldId, scala.Seq[String]]) = {

    var newData = data
    val result = (1 until list.size).map { i =>
      list(i).map { field =>
        val newId = FieldId(buildNewId(field.id.value, i))
        newData = renameFieldInData(field.id, newId, newData)
        field.copy(id = newId)
      }
    }.toList.::(list(0))

    (result, newData)
  }

  private def renameFieldInData(src: FieldId, dst: FieldId, data: Map[FieldId, scala.Seq[String]]) = {
    if (data.contains(src)) {
      val value = data(src)
      (data - src) + (dst -> value)
    } else {
      data
    }
  }

  private def buildNewId(id: String, newIndex: Int) = {
    val endOfIndex = id.indexOf('_') + 1
    val idNoIndex = id.substring(endOfIndex)
    s"${newIndex}_${idNoIndex}"
  }

  private def addGroupEntry(dynamicList: List[List[FieldValue]]) = {
    val countForNewEntry = dynamicList.size
    val newEntry = dynamicList(0).map { field => field.copy(id = FieldId(s"${countForNewEntry}_${field.id.value}")) }
    dynamicList :+ newEntry
  }

  private def isRepeatsMaxReached(count: Int, groupField: Group) = {
    groupField.repeatsMax match {
      case Some(max) => if (count >= max) {
        true
      } else {
        false
      }
      case None => true
    }
  }

  def getRepeatingGroupsForRendering(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
    sessionCache.fetchAndGetEntry[List[List[FieldValue]]](topFieldValue.id.value).flatMap {
      case Some(dynamicList) => Future.successful((dynamicList, isRepeatsMaxReached(dynamicList.size, groupField)))
      case None => initialiseDynamicGroupList(topFieldValue, groupField)
    }
  }

  private def initialiseDynamicGroupList(parentField: FieldValue, group: Group)(implicit hc: HeaderCarrier) = {
    val dynamicList = group.repeatsMin match {
      case Some(min) if min == 1 | min <= 0 => List(group.fields)
      case Some(min) if min > 1 =>
        group.fields +: (1 until min).map { i =>
          group.fields.map(field => field.copy(id = FieldId(s"${i}_${field.id.value}")))
        }.toList
      case None => List(group.fields)
    }

    sessionCache.cache[List[List[FieldValue]]](parentField.id.value, dynamicList).map { _ =>
      (dynamicList, isRepeatsMaxReached(dynamicList.size, group))
    }
  }

  def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier): List[FieldValue] = {
    val resultOpt = Await.result(sessionCache.fetchAndGetEntry[List[List[FieldValue]]](topFieldValue.id.value), 10 seconds)
    resultOpt.getOrElse(List(groupField.fields)).flatten
  }

  def getAllFieldsInGroupForSummary(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier): List[FieldValue] = {
    val resultOpt = Await.result(sessionCache.fetchAndGetEntry[List[List[FieldValue]]](topFieldValue.id.value), 10 seconds)
    buildGroupFieldsLabelsForSummary(resultOpt.getOrElse(List(groupField.fields)), topFieldValue)
  }

  def clearSession(implicit hc: HeaderCarrier) = sessionCache.remove()

  private def buildGroupFieldsLabelsForSummary(list: List[List[FieldValue]], fieldValue: FieldValue) = {
    (0 until list.size).flatMap { i =>
      list(i).map { field =>
        field.copy(
          label = LabelHelper.buildRepeatingLabel(Some(field.label), i + 1).getOrElse(""),
          shortName = LabelHelper.buildRepeatingLabel(field.shortName, i + 1)
        )
      }
    }.toList
  }
}

object LabelHelper {
  def buildRepeatingLabel(field: FieldValue, index: Int) = {
    if (field.label.contains("$n")) {
      field.label.replace("$n", index.toString)
    } else {
      field.presentationHint.map(ph => "").orElse(Some(field.label)).get
    }
  }

  def buildRepeatingLabel(text: Option[String], index: Int) = text match {
    case Some(txt) if text.get.contains("$n") => Some(txt.replace("$n", index.toString))
    case _ => text
  }
}
