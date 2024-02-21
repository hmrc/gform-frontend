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
import play.api.data.Forms._
import play.api.data._
import play.api.data.validation._
import play.api.libs.json._
import uk.gov.hmrc.gform.sharedmodel.form.FormId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId

import java.time.Instant

case class SnapshotOverview(
  templateId: FormTemplateId,
  snapshotId: SnapshotId,
  savedAt: Instant,
  description: Description,
  gformVersion: GformVersion,
  gformFrontendVersion: GformFrontendVersion,
  formData: Option[JsObject],
  ggFormData: Option[GovernmentGatewayFormData]
)

object SnapshotOverview {
  implicit val format: OFormat[SnapshotOverview] = derived.oformat()
}

case class SaveRequest(
  formId: FormId,
  description: Description,
  gformFrontendVersion: GformFrontendVersion,
  ggFormData: Option[GovernmentGatewayFormData]
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

object SnapshotForms {
  case class UpdateSnapshotUserData(snapshotId: String, formData: String, description: String)
  def validJson(errorMessage: String): Constraint[String] = Constraint("constraints.validJson") { text =>
    try {
      Json.parse(text)
      Valid
    } catch {
      case _: Throwable => Invalid(ValidationError(errorMessage))
    }
  }
  val updateSnapshotUserData: Form[UpdateSnapshotUserData] = Form(
    mapping(
      "snapshotId"  -> nonEmptyText,
      "formData"    -> text.verifying(validJson("Invalid JSON format")),
      "description" -> text
    )(UpdateSnapshotUserData.apply)(UpdateSnapshotUserData.unapply)
  )

  case class UpdateFormUserData(formData: String, snapshotId: String)

  val updateFormUserData: Form[UpdateFormUserData] = Form(
    mapping(
      "formData"   -> text,
      "snapshotId" -> text
    )(UpdateFormUserData.apply)(UpdateFormUserData.unapply)
  )

  case class SnapshotIdUserData(snapshotId: String)

  val snapshotIdUserData: Form[SnapshotIdUserData] = Form(
    mapping(
      "snapshotId" -> text
    )(SnapshotIdUserData.apply)(SnapshotIdUserData.unapply)
  )

  case class SaveFormUserData(description: String, currentFormId: String)

  val saveFormUserData: Form[SaveFormUserData] = Form(
    mapping(
      "description"   -> text,
      "currentFormId" -> text
    )(SaveFormUserData.apply)(SaveFormUserData.unapply)
  )
}
