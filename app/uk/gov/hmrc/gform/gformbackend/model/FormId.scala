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

package uk.gov.hmrc.gform.gformbackend.model

import play.api.libs.json._
import uk.gov.hmrc.gform.models.ValueClassFormat

case class FormId(value: String) extends AnyVal {
  override def toString = value
}

object FormId {
  implicit val format: Format[FormId] = ValueClassFormat.format(FormId.apply)(_.value)

  implicit val optionalFormat: Format[Option[FormId]] = new Format[Option[FormId]] {
    override def reads(json: JsValue): JsResult[Option[FormId]] = {
      json.validateOpt[FormId]
    }

    override def writes(o: Option[FormId]) = {
      o match {
        case Some(x) => JsString(x.value)
        case None => JsString("None")
      }
    }
  }
}

object FormIdAsMongoId {
  val writes = OWrites[FormId](id => Json.obj("_id" -> id.value))

  val reads = Reads[FormId] { jsObj =>
    (jsObj \ "_id") match {
      case JsDefined(JsString(id)) => JsSuccess(FormId(id))
      case _ => JsError(s"Invalid formId, expected fieldName '_id', got: $jsObj")
    }
  }

  val format = OFormat[FormId](reads, writes)
}
