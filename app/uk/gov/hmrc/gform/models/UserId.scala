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

package uk.gov.hmrc.gform.models

import play.api.Logger
import play.api.libs.json._

import scala.collection.Map

case class UserId(value: String) extends AnyVal {
  override def toString = value
}

object UserId {

  val reads: Reads[UserId] = Reads[UserId] {
    case JsString(str) => JsSuccess(UserId(str))
    case JsObject(obj) =>
      obj.get("groupIdentifier") match {
        case None => JsError(s"groupIdentifier expected inside the obj.")
        case Some(JsString(x)) => JsSuccess(UserId(x))
      }
    case unknown => JsError(s"JsString or JsObject value expected, got: $unknown")
  }

  val writes: Writes[UserId] = Writes[UserId](a => JsString(a.value))
  implicit val format: Format[UserId] = Format[UserId](reads, writes)
}
