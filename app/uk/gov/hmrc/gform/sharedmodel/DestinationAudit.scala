/*
 * Copyright 2019 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel

import java.util.UUID

import java.time.LocalDateTime
import play.api.libs.json.Json
import uk.gov.hmrc.gform.sharedmodel.form.{ FormId, FormStatus }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormTemplateId
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.DestinationId
import uk.gov.hmrc.gform.submission.SubmissionRef

case class DestinationAudit(
  formId: FormId,
  formTemplateId: FormTemplateId,
  destinationId: DestinationId,
  destinationType: String,
  destinationResponseStatus: Option[Int],
  workflowState: FormStatus,
  userId: UserId,
  caseworkerUserName: Option[String],
  caseworkerComment: Option[String],
  submissionReference: SubmissionRef,
  summaryHtmlId: SummaryHtmlId,
  id: UUID = UUID.randomUUID,
  timestamp: LocalDateTime = LocalDateTime.now
)

object DestinationAudit {
  implicit val format = Json.format[DestinationAudit]
}
