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

import play.api.libs.json.{ Format, JsResult, JsValue, Json }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ DataSource, JsonUtils }

import scala.collection.immutable

case class BooleanExprCache(mapping: collection.Map[DataSource, collection.Map[String, Boolean]]) extends AnyVal {
  def get(dataSource: DataSource, value: String): Option[Boolean] =
    for {
      look <- mapping.get(dataSource)
      res  <- look.get(value)
    } yield res
}

object BooleanExprCache {
  implicit def collectionMapFormat[T](implicit format: Format[T]): Format[collection.Map[String, T]] =
    new Format[collection.Map[String, T]] {
      override def reads(json: JsValue): JsResult[collection.Map[String, T]] =
        json.validate[immutable.Map[String, T]].map(_.asInstanceOf[collection.Map[String, T]])

      override def writes(o: collection.Map[String, T]): JsValue = Json.toJson(o.toMap)
    }
  implicit val b: Format[collection.Map[DataSource, collection.Map[String, Boolean]]] =
    JsonUtils.formatMapO[DataSource, collection.Map[String, Boolean]](DataSource.fromString, _.convertToString())

  implicit val format: Format[BooleanExprCache] = Json.format
  val empty = BooleanExprCache(Map.empty)
}
