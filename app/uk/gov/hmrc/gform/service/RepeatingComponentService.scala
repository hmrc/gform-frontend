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

import julienrf.json.derived
import play.api.libs.json.OFormat

import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.gform.connectors.SessionCacheConnector
import uk.gov.hmrc.gform.models.components.{ FieldId, FieldValue, Group }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

@Singleton
class RepeatingComponentService @Inject() (val sessionCache: SessionCacheConnector) {

  def appendNewGroup(formGroupId: String)(implicit hc: HeaderCarrier) = {
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

  def removeGroup(formGroupId: String)(implicit hc: HeaderCarrier) = {
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
      newList = dynamicList diff List(dynamicList(index - 1))
      cacheMap <- sessionCache.cache[List[List[FieldValue]]](groupId, newList)
    } yield cacheMap.getEntry[List[List[FieldValue]]](groupId)
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
    val dynamicList = group.fields +: (1 to group.repeatsMin.getOrElse(1)).map { i =>
      group.fields.map(field => field.copy(id = FieldId(s"${i}_${field.id.value}")))
    }.toList

    sessionCache.cache[List[List[FieldValue]]](parentField.id.value, dynamicList).map { _ =>
      (dynamicList, isRepeatsMaxReached(dynamicList.size, group))
    }
  }

  def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier): List[FieldValue] = {
    Try(Await.result(sessionCache.fetchAndGetEntry[List[List[FieldValue]]](topFieldValue.id.value), 10 seconds)) match {
      case Success(value) => value.getOrElse(List(groupField.fields)).flatten
      case Failure(e) =>
        play.Logger.error(e.getStackTrace.mkString)
        groupField.fields
    }
  }

  //  private def validateRepeatCount(requestedCount: Int, fieldValue: FieldValue, groupField: Group) = {
  //    (groupField.repeatsMax, groupField.repeatsMin) match {
  //      case (Some(max), Some(min)) if requestedCount >= min && requestedCount <= max => requestedCount
  //      case (Some(max), Some(min)) if requestedCount >= min && requestedCount > max => max
  //      case (Some(max), Some(min)) if requestedCount < min => min
  //      case (Some(max), None) if requestedCount <= max => requestedCount
  //      case (Some(max), None) if requestedCount > max => max
  //      case _ => 1
  //    }
  //  }
}
