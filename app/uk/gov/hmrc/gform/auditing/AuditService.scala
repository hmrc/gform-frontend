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

import play.api.Logger
import play.api.libs.json.Json
import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.auth.models.Retrievals._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BaseSection, FormComponent, UkSortCode }
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def formToMap(form: Form, sections: List[BaseSection]): Map[String, String] = {
    val dataMap = Map(
      "FormId" -> form._id.value,
      "EnvelopeId" -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId" -> form.userId.value //TODO is userId required in the formData anymore.
    )
    val optSortCode: List[FormComponent] = sections.flatMap(_.fields.filter(_.`type` match {
      case x: UkSortCode => true
      case _ => false
    }))

    val processedData: Seq[FormField] = if (optSortCode.nonEmpty) {
      optSortCode.flatMap { fieldValue =>
        val xc = UkSortCode.fields(fieldValue.id).flatMap { fieldId =>
          form.formData.fields.filterNot(_.id == fieldId)
        }
        val sortCode = UkSortCode.fields(fieldValue.id).flatMap { fieldId =>
          val sortCode: String = form.formData.fields.filter(_.id == fieldId).map(_.value).mkString("-")
          Seq(FormField(fieldValue.id, sortCode))
        }
        xc ++ sortCode
      }
    } else {
      form.formData.fields
    }

    val data = processedData.map(x => x.id.value -> x.value).toMap

    dataMap ++ data
  }

  def sendSubmissionEvent(form: Form, sections: List[BaseSection], retrievals: Retrievals)(implicit ex: ExecutionContext, hc: HeaderCarrier, request: Request[_]) = {
    sendEvent(formToMap(form, sections), retrievals)
  }

  private def sendEvent(detail: Map[String, String], retrievals: Retrievals)(implicit ex: ExecutionContext, hc: HeaderCarrier, request: Request[_]) =
    auditConnector.sendEvent(eventFor(detail, retrievals))

  private def eventFor(detail: Map[String, String], retrievals: Retrievals)(implicit hc: HeaderCarrier, request: Request[_]) = {
    val x = DataEvent(
      auditSource = "GForm",
      auditType = "submission complete auditing",
      tags = hc.headers.toMap,
      detail = detail ++ Map(
      "nino" -> getTaxIdValue(None, "NINO", retrievals),
      "vrn" -> getTaxIdValue(None, "VATRegNo", retrievals),
      "saUtr" -> getTaxIdValue(Some("IR-SA"), "UTR", retrievals),
      "ctUtr" -> getTaxIdValue(Some("IR-CT"), "UTR", retrievals),
      "deviceId" -> hc.deviceID.map(a => a).getOrElse("")
    )
    )
    println(Json.prettyPrint(Json.toJson(x)) + "THIS IS THE SUBMISSION JSON")
    x
  }
}
