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

import play.api.mvc.Request
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FieldValue, Section, UkSortCode }
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.frontend.auth.AuthContext
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def formToMap(form: Form, section: List[Section]): Map[String, String] = {
    val dataMap = Map(
      "FormId" -> form._id.value,
      "EnvelopeId" -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId" -> form.userId.value //TODO is userId required in the formData anymore.
    )
    val optSortCode: List[FieldValue] = section.flatMap(_.fields.filter(_.`type` == UkSortCode))

    val processedData: Seq[FormField] = {
      optSortCode.flatMap { fieldValue =>
        UkSortCode.fields(fieldValue.id).flatMap { fieldId =>
          val sortCode: String = form.formData.fields.filter(_.id == fieldId).map(_.value).mkString("-")
          form.formData.fields.filterNot(_.id == fieldId) ++ Seq(FormField(fieldValue.id, sortCode))
        }
      }
    }

    val data = processedData.map(x => x.id.value -> x.value).toMap

    dataMap ++ data
  }
  def sendSubmissionEvent(form: Form, sections: List[Section])(implicit ex: ExecutionContext, hc: HeaderCarrier, authContext: AuthContext, request: Request[_]) = {
    sendEvent(formToMap(form, sections))
  }

  private def sendEvent(detail: Map[String, String])(implicit ex: ExecutionContext, hc: HeaderCarrier, authContext: AuthContext, request: Request[_]) =
    auditConnector.sendEvent(eventFor(detail))

  private def eventFor(detail: Map[String, String])(implicit hc: HeaderCarrier, authContext: AuthContext, request: Request[_]) = {
    DataEvent(
      auditSource = "GForm",
      auditType = "submission complete auditing",
      tags = hc.headers.toMap,
      detail = detail ++ Map(
      "nino" -> authContext.principal.name.getOrElse(""),
      "vrn" -> authContext.principal.accounts.vat.map(_.vrn.vrn).getOrElse(""),
      "saUtr" -> authContext.principal.accounts.ated.getOrElse("").toString,
      "ctUtr" -> authContext.principal.accounts.ct.getOrElse("").toString,
      "deviceId" -> hc.deviceID.map(a => a).getOrElse("")
    )
    )
  }
}
