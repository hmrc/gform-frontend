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

package uk.gov.hmrc.gform.auditing

import play.api.libs.json.Json
import play.api.mvc.Request
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, MaterialisedRetrievals }
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VATReg }
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BaseSection, FormComponent, Group, UkSortCode }
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def formToMap(form: Form, sections: List[BaseSection]): Map[String, String] = {

    val optSortCode: List[FormComponent] = sections.flatMap { section =>
      val groupFields = section.fields.collect {
        case FormComponent(_, Group(fields, _, _, _, _, _), _, _, _, _, _, _, _, _, _, _, _) => fields
      }.flatten
      (groupFields ++ section.fields.filter(_.`type` match {
        case x: Group => false
        case _        => true
      })).filter(_.`type` match {
        case x: UkSortCode => true
        case _             => false
      })
    }

    val processedData: Seq[FormField] = if (optSortCode.nonEmpty) {
      optSortCode.flatMap { fieldValue =>
        val xc = UkSortCode.fields(fieldValue.id).toList.flatMap { fieldId =>
          form.formData.fields.filterNot(_.id == fieldId)
        }
        val sortCode = UkSortCode
          .fields(fieldValue.id)
          .toList
          .flatMap { fieldId =>
            form.formData.fields.toList.filter(_.id == fieldId)
          }
          .map(_.value)
          .mkString("-")
        xc ++ Seq(FormField(fieldValue.id, sortCode))
      }
    } else {
      form.formData.fields
    }

    processedData.map(x => x.id.value -> x.value).toMap
  }

  def sendSubmissionEvent(
    form: Form,
    sections: List[BaseSection],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId)(implicit ec: ExecutionContext, hc: HeaderCarrier, request: Request[_]) =
    sendEvent(form, formToMap(form, sections), retrievals, customerId)

  private def sendEvent(
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId)(implicit ec: ExecutionContext, hc: HeaderCarrier, request: Request[_]): String = {
    val event = eventFor(form, detail, retrievals, customerId)
    auditConnector.sendExtendedEvent(event)
    event.eventId
  }

  private def eventFor(
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId)(implicit hc: HeaderCarrier, request: Request[_]) =
    ExtendedDataEvent(
      auditSource = "Gform-Frontend",
      auditType = "formSubmitted",
      tags = hc.headers.toMap,
      detail = details(form, detail, retrievals, customerId)
    )

  private def details(
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId)(implicit hc: HeaderCarrier) = {

    val userInfo =
      retrievals match {
        case mr: AuthenticatedRetrievals =>
          Json.toJson(
            Map(
              "nino"     -> mr.getTaxIdValue(NINO()),
              "vrn"      -> mr.getTaxIdValue(VATReg()),
              "saUtr"    -> mr.getTaxIdValue(IRSA()),
              "ctUtr"    -> mr.getTaxIdValue(IRCT()),
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty))
        case AnonymousRetrievals(_) =>
          Json.toJson(
            Map(
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty))
      }

    val userValues = Json.toJson(detail.filter(values => values._2.nonEmpty))
    Json.obj(
      "FormId"         -> form._id.value,
      "EnvelopeId"     -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId"         -> form.userId.value,
      "CustomerId"     -> customerId.id,
      "UserValues"     -> userValues,
      "UserInfo"       -> userInfo
    )
  }

  def sendSubmissionEventHashed(hashedValue: String, formAsString: String, eventId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) =
    sendHashedValues(hashedValue, formAsString, eventId)

  private def sendHashedValues(hash: String, formAsString: String, eventId: String)(
    implicit hc: HeaderCarrier,
    ec: ExecutionContext) = auditConnector.sendEvent(hashedValueEvent(hash, formAsString, eventId))

  private def hashedValueEvent(hashedValue: String, formString: String, eventId: String)(implicit hc: HeaderCarrier) =
    DataEvent(
      auditSource = "GForm",
      auditType = "printedReturnNonrepudiation",
      tags = hc.headers.toMap,
      eventId = eventId,
      detail = Map(
        "hashType"    -> "sha256",
        "hashedValue" -> hashedValue,
        "formData"    -> formString
      )
    )
}
