/*
 * Copyright 2017 HM Revenue & Customs
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

package uk.gov.hmrc.gform.auditing

import play.api.libs.json.Json
import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  val formToMap: Form => Map[String, String] = {
    form =>
      val dataMap = Map(
        "FormId" -> form._id.value,
        "EnvelopeId" -> form.envelopeId.value,
        "FormTemplateId" -> form.formTemplateId.value,
        "UserId" -> form.userId.value //TODO is userId required in the formData anymore.
      )

      dataMap ++ form.formData.fields.map(x => x.id.value -> x.value).toMap
  }
  def sendSubmissionEvent(form: Form)(implicit ex: ExecutionContext, hc: HeaderCarrier, retrievals: Retrievals, request: Request[_]) = {
    sendEvent(formToMap(form))
  }

  private def sendEvent(detail: Map[String, String])(implicit ex: ExecutionContext, hc: HeaderCarrier, retrievals: Retrievals, request: Request[_]) =
    auditConnector.sendEvent(eventFor(detail))

  private def eventFor(detail: Map[String, String])(implicit hc: HeaderCarrier, retrievals: Retrievals, request: Request[_]) = {
    /*
    Enrolment(
                      key: String,
                      identifiers: Seq[EnrolmentIdentifier],
                      state: String,
                      confidenceLevel: ConfidenceLevel,
                      delegatedAuthRule: Option[String] = None)
     */
    val cosa = Json.toJson(retrievals.enrolments.enrolments)
    println(s"HOLA: [$cosa]")
    DataEvent(
      auditSource = "GForm",
      auditType = "submission complete auditing",
      tags = hc.headers.toMap,
      detail = detail ++ Map(
      "nino" -> "", //authContext.principal.name.getOrElse(""),
      "vrn" -> "", //authContext.principal.accounts.vat.map(_.vrn.vrn).getOrElse(""),
      "saUtr" -> "", //authContext.principal.accounts.ated.getOrElse("").toString,
      "ctUtr" -> "", //authContext.principal.accounts.ct.getOrElse("").toString,
      "deviceId" -> hc.deviceID.map(a => a).getOrElse("")
    )
    )
  }
}
