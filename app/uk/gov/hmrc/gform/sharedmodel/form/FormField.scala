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

package uk.gov.hmrc.gform.sharedmodel.form

import play.api.libs.functional.syntax._
import play.api.libs.json._
import uk.gov.hmrc.gform.models.ExpandUtils
import uk.gov.hmrc.gform.models.ids.ModelComponentId

// Contains the *value* of a FormComponentId, where the FormComponentId is either:
//   * the FormComponentId of a FormComponent with a singular value ComponentType, such as a Text or TextArea, OR
//   * the synthesised FormComponentId of a MultiField ComponentType, such as Date (see MultiField.fields)
case class FormField(id: ModelComponentId, value: String)

object FormField {

  implicit val reads: Reads[FormField] = ((JsPath \ "id")
    .read[String]
    .map(ExpandUtils.toModelComponentId) and
    (JsPath \ "value").read[String])(FormField.apply _)

  implicit val writes: OWrites[FormField] = OWrites[FormField] { formField =>
    Json.obj(
      "id"    -> formField.id.toMongoIdentifier,
      "value" -> formField.value
    )
  }

  implicit val format: OFormat[FormField] = OFormat(reads, writes)
}
