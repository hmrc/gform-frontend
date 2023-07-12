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

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.{ Format, JsString, OFormat }
import uk.gov.hmrc.gform.sharedmodel.ValueClassFormat

final case class FormTemplateVersion(version: String) extends AnyVal

object FormTemplateVersion {

  implicit val equal: Eq[FormTemplateVersion] = Eq.fromUniversalEquals

  implicit val oformat: OFormat[FormTemplateVersion] = derived.oformat()

  val vformat: Format[FormTemplateVersion] =
    ValueClassFormat
      .vformat[FormTemplateVersion]("formTemplateVersion", FormTemplateVersion.apply, x => JsString(x.version))

  val destformat: OFormat[FormTemplateVersion] =
    ValueClassFormat.oformat[FormTemplateVersion]("version", FormTemplateVersion.apply, _.version)

}
