/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.nonRepudiation

import play.api.libs.json.Json
import uk.gov.hmrc.gform.auditing.AuditingModule
import uk.gov.hmrc.gform.sharedmodel.form.Form

import scala.concurrent.ExecutionContext
import uk.gov.hmrc.http.HeaderCarrier

class NonRepudiationHelpers(auditModule: AuditingModule) {

  def formDataToJson(form: Form): String = Json.toJson(form.formData).toString()

  def computeHash(formJson: String): String = sha256Hash(formJson)

  def sendAuditEvent(hashedValue: String, formAsString: String, eventId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = auditModule.auditService.sendSubmissionEventHashed(hashedValue, formAsString, eventId)

  def sha256Hash(text: String): String =
    String.format(
      "%064x",
      new java.math.BigInteger(
        1,
        java.security.MessageDigest
          .getInstance("SHA-256")
          .digest(text
            .getBytes("UTF-8"))))
}
