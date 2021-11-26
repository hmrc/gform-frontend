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

package uk.gov.hmrc.gform.auth.models

import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormTemplate, FormTemplateId, FormTemplateWithRedirects, JsonUtils }

case class CompositeAuthDetails(mappings: Map[FormTemplateId, String] = Map.empty) {
  def add(formTemplate: FormTemplate, value: String): CompositeAuthDetails =
    CompositeAuthDetails(
      mappings + (formTemplate._id -> value) ++ formTemplate.legacyFormIds.fold(List.empty[(FormTemplateId, String)])(
        _.map(_ -> value).toList
      )
    )

  def remove(key: FormTemplateId): CompositeAuthDetails =
    CompositeAuthDetails(mappings - key)

  def get(formTemplateWithRedirects: FormTemplateWithRedirects): Option[String] =
    mappings
      .get(formTemplateWithRedirects.formTemplate._id)
      .orElse(formTemplateWithRedirects.redirect.flatMap(mappings.get))
}

object CompositeAuthDetails {
  val empty = CompositeAuthDetails()

  val formatMap: Format[Map[FormTemplateId, String]] =
    JsonUtils.formatMap(FormTemplateId.apply, _.value)

  implicit val format: Format[CompositeAuthDetails] = Format(
    formatMap.map(CompositeAuthDetails.apply),
    formatMap.contramap(_.mappings)
  )
}
