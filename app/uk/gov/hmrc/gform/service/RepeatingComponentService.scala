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

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.libs.json.Writes
import uk.gov.hmrc.gform.connectors.SessionCacheConnector
import uk.gov.hmrc.play.http.HeaderCarrier

@Singleton
class RepeatingComponentService @Inject()(val sessionCache: SessionCacheConnector) {

  def increaseGroupCount(formGroupId: String)(implicit hc: HeaderCarrier) = {
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

}
