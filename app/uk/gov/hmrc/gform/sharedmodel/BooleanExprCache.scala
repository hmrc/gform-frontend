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

package uk.gov.hmrc.gform.sharedmodel

import play.api.libs.json.{ Format, Json }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, JsonUtils }

case class BooleanExprCache(mapping: Map[DataSource, Map[String, Boolean]]) extends AnyVal {
  def get(dataSource: DataSource, value: String): Option[Boolean] =
    for {
      look <- mapping.get(dataSource)
      res  <- look.get(value)
    } yield res
}

object BooleanExprCache {
  implicit val b: Format[Map[DataSource, Map[String, Boolean]]] =
    JsonUtils.formatMapO[DataSource, Map[String, Boolean]](DataSource.fromString, _.convertToString())

  implicit val format: Format[BooleanExprCache] = Json.format
  val empty = BooleanExprCache(Map.empty)
}
