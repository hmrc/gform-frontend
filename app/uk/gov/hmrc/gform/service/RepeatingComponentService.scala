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

import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.gform.connectors.SessionCacheConnector
import uk.gov.hmrc.gform.models.components.{ FieldId, FieldValue, Group }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{ Failure, Success, Try }

@Singleton
class RepeatingComponentService @Inject() (val sessionCache: SessionCacheConnector) {

  def increaseGroupCount(formGroupId: String)(implicit hc: HeaderCarrier) = {
    // on the forms, the AddGroup button's name has the following format:
    // AddGroup-(groupFieldId)
    // that's the reason why the extraction below is required
    val startPos = formGroupId.indexOf('-') + 1
    val componentId = formGroupId.substring(startPos)
    increaseCount(componentId)
  }

  def increaseCount(componentId: String)(implicit hc: HeaderCarrier) = {
    for {
      countOpt <- sessionCache.fetchAndGetEntry[Int](componentId)
      count = countOpt.getOrElse(1) + 1
      cacheMap <- sessionCache.cache[Int](componentId, count)
    } yield cacheMap.getEntry[Int](componentId)
  }

  def getCount(componentId: String)(implicit hc: HeaderCarrier) = {
    sessionCache.fetchAndGetEntry[Int](componentId).map {
      case Some(count) => count
      case None => 1
    }
  }

  def synchronousGetCount(componentId: String)(implicit hc: HeaderCarrier) = {
    Try(Await.result(getCount(componentId), 10 seconds)) match {
      case Success(value) => value
      case Failure(e) =>
        play.Logger.error(e.getStackTrace.mkString)
        1
    }
  }

  def getValidatedRepeatingCount(fieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
    getCount(fieldValue.id.value).map { sessionCount =>
      validateRepeatCount(sessionCount, fieldValue, groupField)
    }
  }

  def synchronousGetValidatedRepeatingCount(fieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
    val sessionCount = synchronousGetCount(fieldValue.id.value)
    validateRepeatCount(sessionCount, fieldValue, groupField)
  }

  def buildRepeatingId(fieldValue: FieldValue, instance: Int) = {
    FieldId(s"${fieldValue.id.value}_$instance")
  }

  def getAllFieldsInGroup(topFieldValue: FieldValue, groupField: Group)(implicit hc: HeaderCarrier) = {
    val count = synchronousGetValidatedRepeatingCount(topFieldValue, groupField)
    (0 until count).flatMap { i =>
      groupField.fields.map { fieldValue =>
        if (i == 0) fieldValue
        else fieldValue.copy(id = buildRepeatingId(fieldValue, i))
      }
    }.toList
  }

  private def validateRepeatCount(requestedCount: Int, fieldValue: FieldValue, groupField: Group) = {
    (groupField.repeatsMax, groupField.repeatsMin) match {
      case (Some(max), Some(min)) if requestedCount >= min && requestedCount <= max => requestedCount
      case (Some(max), Some(min)) if requestedCount >= min && requestedCount > max => max
      case (Some(max), Some(min)) if requestedCount < min => min
      case (Some(max), None) if requestedCount <= max => requestedCount
      case (Some(max), None) if requestedCount > max => max
      case _ => 1
    }
  }
}
