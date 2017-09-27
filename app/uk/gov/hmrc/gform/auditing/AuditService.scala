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

import java.util.UUID

import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.json.{ JsString, JsValue, Json }
import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.models.Retrievals
import uk.gov.hmrc.gform.auth.models.Retrievals._
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField, FormId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BaseSection, FormComponent, Group, UkSortCode }
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def formToMap(form: Form, sections: List[BaseSection]): Map[String, String] = {

    val optSortCode: List[FormComponent] = sections.flatMap { section =>
      val groupFields = section.fields.collect {
        case FormComponent(_, Group(fields, _, _, _, _, _), _, _, _, _, _, _, _, _, _) => fields
      }.flatten
      (groupFields ++ section.fields.filter(_.`type` match {
        case x: Group => false
        case _ => true
      })).filter(_.`type` match {
        case x: UkSortCode => true
        case _ => false
      })
    }

    val processedData: Seq[FormField] = if (optSortCode.nonEmpty) {
      optSortCode.flatMap { fieldValue =>
        val xc = UkSortCode.fields(fieldValue.id).flatMap { fieldId =>
          form.formData.fields.filterNot(_.id == fieldId)
        }
        val sortCode = UkSortCode.fields(fieldValue.id).flatMap { fieldId =>
          form.formData.fields.filter(_.id == fieldId)
        }.map(_.value).mkString("-")
        xc ++ Seq(FormField(fieldValue.id, sortCode))
      }
    } else {
      form.formData.fields
    }

    processedData.map(x => x.id.value -> x.value).toMap
  }

  def sendSubmissionEvent(form: Form, sections: List[BaseSection], retrievals: Retrievals)(implicit ec: ExecutionContext, hc: HeaderCarrier, request: Request[_]) = {
    sendEvent(form, formToMap(form, sections), retrievals)
  }

  private def sendEvent(form: Form, detail: Map[String, String], retrievals: Retrievals)(implicit ec: ExecutionContext, hc: HeaderCarrier, request: Request[_]) =
    auditConnector.sendEvent(eventFor(form, detail, retrievals))

  private def eventFor(form: Form, detail: Map[String, String], retrievals: Retrievals)(implicit hc: HeaderCarrier, request: Request[_]) = {
    ExtendedDataEvent(
      auditSource = "Gform-Frontend",
      auditType = "formSubmitted",
      tags = hc.headers.toMap,
      detail = details(form, detail, retrievals)
    )
  }

  private def details(form: Form, detail: Map[String, String], retrievals: Retrievals)(implicit hc: HeaderCarrier) = {

    val userInfo = Json.toJson(Map(
      "nino" -> getTaxIdValue(None, "NINO", retrievals),
      "vrn" -> getTaxIdValue(None, "VATRegNo", retrievals),
      "saUtr" -> getTaxIdValue(Some("IR-SA"), "UTR", retrievals),
      "ctUtr" -> getTaxIdValue(Some("IR-CT"), "UTR", retrievals),
      "deviceId" -> hc.deviceID.map(a => a).getOrElse("")
    ).filter(values => values._2.nonEmpty))

    val userValues = Json.toJson(detail.filter(values => values._2.nonEmpty))
    Json.obj(
      "FormId" -> form._id.value,
      "EnvelopeId" -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId" -> form.userId.value,
      "UserValues" -> userValues,
      "UserInfo" -> userInfo
    )
  }
}
