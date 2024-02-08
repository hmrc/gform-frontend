/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.gform.testonly

import julienrf.json.derived
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.form.FormId

import java.time.Instant

case class Snapshot(
  templateId: FormTemplateId,
  snapshotId: SnapshotId,
  savedAt: Instant,
  description: Description,
  gformVersion: GformVersion,
  gformFrontendVersion: GformFrontendVersion
)

object Snapshot {
  implicit val format: OFormat[Snapshot] = derived.oformat()
}

case class SnapshotWithData(
  snapshot: Snapshot,
  formData: JsObject
)
object SnapshotWithData {
  implicit val format: OFormat[SnapshotWithData] = derived.oformat()
}

case class SaveRequest(
  formId: FormId,
  description: Description,
  gformFrontendVersion: GformFrontendVersion
)

object SaveRequest {
  implicit val format: OFormat[SaveRequest] = derived.oformat()
}

case class SaveReply(
  formId: FormId
)

object SaveReply {
  implicit val format: OFormat[SaveReply] = derived.oformat()
}

case class UpdateSnapshotRequest(
  snapshotId: SnapshotId,
  formData: JsObject,
  description: Description
)

object UpdateSnapshotRequest {
  implicit val format: OFormat[UpdateSnapshotRequest] = derived.oformat()
}

case class UpdateFormDataRequest(
  formId: FormId,
  formData: JsObject
)

object UpdateFormDataRequest {
  implicit val format: OFormat[UpdateFormDataRequest] = derived.oformat()
}

case class SnapshotId(value: String) extends AnyVal

object SnapshotId {
  implicit val format: Format[SnapshotId] = Json.valueFormat
}

case class Description(value: String) extends AnyVal
object Description {
  implicit val format: Format[Description] = Json.valueFormat
}

case class GformVersion(value: String) extends AnyVal

object GformVersion {
  implicit val format: Format[GformVersion] = Json.valueFormat
}

case class GformFrontendVersion(value: String) extends AnyVal

object GformFrontendVersion {
  implicit val format: Format[GformFrontendVersion] = Json.valueFormat
}
