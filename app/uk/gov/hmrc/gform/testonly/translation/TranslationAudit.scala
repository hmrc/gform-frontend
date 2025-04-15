/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly.translation

import java.time.Instant
import julienrf.json.derived
import play.api.libs.json.{ Json, OFormat }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

final case class TranslationAuditOverview(
  _id: TranslationAuditId,
  formTemplateId: FormTemplateId,
  result: TranslationResult,
  createdAt: Instant
)

object TranslationAuditOverview {
  implicit val format: OFormat[TranslationAuditOverview] =
    Json.format[TranslationAuditOverview]
}

sealed trait TranslationResult extends Product with Serializable {
  def report(): String = this match {
    case TranslationResult.Success        => "Success"
    case TranslationResult.Failure(error) => error
  }
}

object TranslationResult {
  case object Success extends TranslationResult
  case class Failure(reason: String) extends TranslationResult

  implicit val format: OFormat[TranslationResult] = derived.oformat()
}
