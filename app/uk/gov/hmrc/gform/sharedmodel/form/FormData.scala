/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.Semigroup
import play.api.libs.json._
import uk.gov.hmrc.gform.models.ids.ModelComponentId

case class FormData(fields: Seq[FormField]) {
  val toData: Map[ModelComponentId, String] = {
    val a: List[(ModelComponentId, String)] = fields.toList.map(x => x.id -> x.value)
    a.toMap
  }
  def find(id: ModelComponentId): Option[String] = toData.get(id)
}

object FormData {

  implicit val semigroup: Semigroup[FormData] = new Semigroup[FormData] {
    def combine(x: FormData, y: FormData): FormData = FormData(x.fields ++ y.fields)
  }

  implicit val format: OFormat[FormData] = Json.format[FormData]
}
