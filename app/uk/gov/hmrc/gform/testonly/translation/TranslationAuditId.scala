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

import cats.Eq
import play.api.libs.json.{ Format, JsError, JsString, JsSuccess, Reads, Writes }

final case class TranslationAuditId(value: String) extends AnyVal

object TranslationAuditId {

  implicit val equal: Eq[TranslationAuditId] = Eq.fromUniversalEquals

  val writes: Writes[TranslationAuditId] = Writes[TranslationAuditId](id => JsString(id.value))
  val reads: Reads[TranslationAuditId] = Reads[TranslationAuditId] {
    case JsString(value) => JsSuccess(TranslationAuditId(value))
    case otherwise       => JsError(s"Invalid translationAuditId, expected JsString, got: $otherwise")
  }

  implicit val format: Format[TranslationAuditId] = Format[TranslationAuditId](reads, writes)
}
