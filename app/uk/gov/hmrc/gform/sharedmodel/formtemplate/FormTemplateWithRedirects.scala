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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.gform.shutter.Shutter

final case class FormTemplateWithRedirects(
  formTemplate: FormTemplate,
  specimenSource: Option[
    FormTemplate
  ], // If formTemplate is speciment, this holds template from which specimen has been generated
  redirect: Option[FormTemplateId], // FormTemplateId which has formTemplate._id in its legacyIds
  shutter: Option[Shutter] = None
)

object FormTemplateWithRedirects {
  def noRedirects(formTemplate: FormTemplate, specimenSource: Option[FormTemplate]): FormTemplateWithRedirects =
    FormTemplateWithRedirects(formTemplate, specimenSource, Option.empty[FormTemplateId])
  implicit val format: OFormat[FormTemplateWithRedirects] = derived.oformat()
}
