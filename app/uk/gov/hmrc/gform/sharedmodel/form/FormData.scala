/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.json._
import uk.gov.hmrc.gform.models.ids.ModelComponentId

case class FormData(fields: List[FormField]) {

  val toData: Map[ModelComponentId, String] =
    fields.map(x => x.id -> x.value).toMap

  val valueBytesSize: Int = fields.map(f => f.value.getBytes("UTF-8").length).sum

  def find(id: ModelComponentId): Option[String] = toData.get(id)

  def ++(other: FormData): FormData =
    FormData((toData ++ other.toData).map { case (k, v) => FormField(k, v) }.toList)
}

object FormData {
  implicit val format: OFormat[FormData] = Json.format[FormData]
}
