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

package uk.gov.hmrc.gform.controllers

import uk.gov.hmrc.http.BadRequestException

case class RequestRelatedData(requestData: Map[String, Seq[String]]) extends AnyVal {
  def +(rrd: RequestRelatedData): RequestRelatedData = RequestRelatedData(requestData ++ rrd.requestData)
  def getOption(key: String): Option[String] =
    requestData
      .get(key)
      .flatMap(_.headOption)
  def get(key: String): String =
    getOption(key).getOrElse(throw new BadRequestException(s"Missing '$key' in request data"))

}

object RequestRelatedData {
  val empty = RequestRelatedData(Map.empty)
}
