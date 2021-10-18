/*
 * Copyright 2021 HM Revenue & Customs
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
import uk.gov.hmrc.gform.auth.models.{ AnonymousRetrievals, AuthenticatedRetrievals, EmailRetrievals, MaterialisedRetrievals, VerifyRetrievals }
import uk.gov.hmrc.gform.commons.HeaderCarrierUtil
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VATReg }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }
import uk.gov.hmrc.http.{ HeaderCarrier }

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def formToMap[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): Map[String, String] =
    formModelVisibilityOptics.data.all.map { case (modelComponentId, variadicValue) =>
      modelComponentId.toMongoIdentifier -> variadicValue.toSeq.mkString(",")
    }.toMap

  def sendFormSaveEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSaved", form, Map.empty, retrievals, CustomerId.empty)

  def sendFormResumeEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formResumed", form, Map.empty, retrievals, CustomerId.empty)

  def sendSubmissionEvent[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSubmitted", form, formToMap(form, formModelVisibilityOptics), retrievals, customerId)

  private def sendEvent(
    auditType: String,
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    auditConnector.sendExplicitAudit(auditType, details(form, detail, retrievals, customerId))

  def calculateSubmissionEvent[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit hc: HeaderCarrier): ExtendedDataEvent =
    eventFor(form, formToMap(form, formModelVisibilityOptics), retrievals, customerId)

  private def eventFor(
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit hc: HeaderCarrier) =
    ExtendedDataEvent(
      auditSource = "Gform-Frontend",
      auditType = "formSubmitted",
      detail = details(form, detail, retrievals, customerId)
    )

  private def details(
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId
  )(implicit hc: HeaderCarrier) = {

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
            ).filter(values => values._2.nonEmpty)
          )
        case r: VerifyRetrievals =>
          Json.toJson(
            Map(
              "nino"     -> r.getTaxIdValue(NINO()),
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
        case AnonymousRetrievals(_) =>
          Json.toJson(
            Map(
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
        case EmailRetrievals(_) =>
          Json.toJson(
            Map(
              "deviceId" -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
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

  def sendSubmissionEventHashed(hashedValue: String, formAsString: String, eventId: String)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ) =
    sendHashedValues(hashedValue, formAsString, eventId)

  private def sendHashedValues(hash: String, formAsString: String, eventId: String)(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext
  ) = auditConnector.sendEvent(hashedValueEvent(hash, formAsString, eventId))

  private def hashedValueEvent(hashedValue: String, formString: String, eventId: String)(implicit hc: HeaderCarrier) =
    DataEvent(
      auditSource = "GForm",
      auditType = "printedReturnNonrepudiation",
      tags = HeaderCarrierUtil.allHeadersFromHC.toMap,
      eventId = eventId,
      detail = Map(
        "hashType"    -> "sha256",
        "hashedValue" -> hashedValue,
        "formData"    -> formString
      )
    )
}
