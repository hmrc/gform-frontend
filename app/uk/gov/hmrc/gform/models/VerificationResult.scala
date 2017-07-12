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

case class SaveResult(success: Option[String], error: Option[String])

object SaveResult {

  val reads = Reads[SaveResult] {
    case x =>
      Logger.info("THIS IS X: " + Json.prettyPrint(x))
      JsSuccess(SaveResult(None, None))
    case _ => JsError("THIS IS AN ERROR")
  }

  val writes = Writes[SaveResult] { x =>
    JsString(x.toString)
  }

  implicit val formats = Format[SaveResult](reads, writes) //Json.format[SaveResult]
}
