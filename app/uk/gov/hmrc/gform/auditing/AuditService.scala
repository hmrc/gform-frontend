/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.data.NonEmptyList
import julienrf.json.derived
import play.api.libs.json.{ Format, JsNumber, Json }
import uk.gov.hmrc.gform.addresslookup.PostcodeLookupRetrieve.AddressRecord
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.commons.HeaderCarrierUtil
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VATReg }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroupUtil, SubmissionRef }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }

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

  def sendFormCreateEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formCreated", form, Map.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormResumeEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formResumed", form, Map.empty, retrievals, CustomerId.empty, List.empty)

  def sendSubmissionEvent[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSubmitted", form, formToMap(form, formModelVisibilityOptics), retrievals, customerId, envelopeFiles)

  def formSavedEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSaved", form, Map.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormTimoutEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formTimeout", form, Map.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormSignOut[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSignOut", form, Map.empty, retrievals, CustomerId.empty, List.empty)

  private def sendEvent(
    auditType: String,
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    auditConnector.sendExplicitAudit(auditType, details(form, detail, retrievals, customerId, envelopeFiles))

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
      detail = details(form, detail, retrievals, customerId, List.empty)
    )

  private def details(
    form: Form,
    detail: Map[String, String],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit hc: HeaderCarrier) = {

    val userInfo =
      retrievals match {
        case mr: AuthenticatedRetrievals =>
          Json.toJson(
            Map(
              "email"          -> mr.getEmail.toString,
              "ggLogin"        -> "true",
              "nino"           -> mr.getTaxIdValue(NINO()),
              "vrn"            -> mr.getTaxIdValue(VATReg()),
              "saUtr"          -> mr.getTaxIdValue(IRSA()),
              "ctUtr"          -> mr.getTaxIdValue(IRCT()),
              "deviceId"       -> hc.deviceID.getOrElse(""),
              "affinityGroup"  -> AffinityGroupUtil.affinityGroupName(mr.affinityGroup),
              "credentialRole" -> mr.getCredentialRole.map(_.toString).getOrElse("")
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
        case er: EmailRetrievals =>
          Json.toJson(
            Map(
              "email"      -> er.getEmail.toString,
              "emailLogin" -> "true",
              "deviceId"   -> hc.deviceID.getOrElse("")
            ).filter(values => values._2.nonEmpty)
          )
      }

    val userValues = Json.toJson(detail.filter(values => values._2.nonEmpty))
    val envelopeFilesJsObj =
      if (envelopeFiles.nonEmpty)
        Json.obj(
          "FileInfo" -> Json.obj(
            "numberOfFiles" -> JsNumber(envelopeFiles.size),
            "files"         -> envelopeFiles.map(file => Json.obj("name" -> file.fileName, "length" -> JsNumber(file.length)))
          )
        )
      else
        Json.obj()

    Json.obj(
      "FormId"         -> form._id.value,
      "EnvelopeId"     -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId"         -> form.userId.value,
      "CustomerId"     -> customerId.id,
      "UserValues"     -> userValues,
      "UserAddresses"  -> getUserAddresses(form.thirdPartyData),
      "UserInfo"       -> userInfo,
      "SubmissionRef"  -> SubmissionRef(form.envelopeId).value
    ) ++ envelopeFilesJsObj
  }

  private def getUserAddresses(thirdPartyData: ThirdPartyData): Map[String, AuditAddress] = {
    import scala.language.implicitConversions
    implicit def toOpt(str: String): Option[String] = Option.unless(str.isBlank)(str)

    def auditAddressFromEntered(formData: Option[FormData], maybeUprn: Option[String]) = {
      def get(pattern: String): String =
        formData
          .flatMap(_.fields.find(ff => ff.id.toFormComponentId.value.contains(pattern)).map(_.value))
          .getOrElse("")

      val country: String = if (get("-uk") == "true") "United Kingdom" else get("-country")

      AuditAddress(
        get("-street1"),
        get("-street2"),
        get("-street3"),
        get("-street4"),
        get("postcode"),
        get("town"),
        country,
        maybeUprn
      )
    }

    def auditAddressFromLookup(lookupAddress: AddressRecord) =
      AuditAddress(
        lookupAddress.address.line1,
        lookupAddress.address.line2,
        lookupAddress.address.line3,
        lookupAddress.address.line4,
        lookupAddress.address.postcode,
        lookupAddress.address.town,
        lookupAddress.address.country.name,
        lookupAddress.uprn.map(_.toString)
      )

    def getAddressUprn(thirdPartyData: ThirdPartyData, fc: FormComponentId, addressId: String): Option[String] = {
      val maybeAddresses: Option[NonEmptyList[AddressRecord]] =
        thirdPartyData.postcodeLookup.flatMap(_.get(fc)).map(_.response).flatMap(_.addresses)
      maybeAddresses.flatMap(_.find(addr => addr.id == addressId).flatMap(found => found.uprn.map(_.toString)))
    }

    val confirmedAddresses: Set[FormComponentId] = thirdPartyData.confirmedAddresses.getOrElse(Set.empty)
    val selectedAddresses: Map[FormComponentId, String] = thirdPartyData.selectedAddresses.getOrElse(Map.empty)
    val enteredAddresses: Map[FormComponentId, FormData] = thirdPartyData.enteredAddresses.getOrElse(Map.empty)

    confirmedAddresses.map { fc =>
      if (enteredAddresses.contains(fc)) {
        // Need to get address from entered addresses
        val maybeUprn: Option[String] =
          selectedAddresses.get(fc).flatMap(addressId => getAddressUprn(thirdPartyData, fc, addressId))
        fc.value -> auditAddressFromEntered(enteredAddresses.get(fc), maybeUprn)
      } else {
        // Need to get address from postcode lookup response
        val maybeResponseAddresses: Option[NonEmptyList[AddressRecord]] =
          thirdPartyData.postcodeLookup.flatMap(_.get(fc)).map(_.response).flatMap(_.addresses)
        val addressResult: Option[AddressRecord] =
          maybeResponseAddresses.flatMap(_.find(_.id == selectedAddresses.getOrElse(fc, "")))
        fc.value -> addressResult.map(auditAddressFromLookup).getOrElse(AuditAddress.empty)
      }
    }.toMap
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
      tags = HeaderCarrierUtil.allHeadersFromHC().toMap,
      eventId = eventId,
      detail = Map(
        "hashType"    -> "sha256",
        "hashedValue" -> hashedValue,
        "formData"    -> formString
      )
    )
}

case class AuditAddress(
  addressLine1: String,
  addressLine2: Option[String],
  addressLine3: Option[String],
  addressLine4: Option[String],
  postCode: String,
  town: Option[String],
  country: Option[String],
  uprn: Option[String]
)

object AuditAddress {
  implicit val format: Format[AuditAddress] = derived.oformat()
  def empty = AuditAddress("", None, None, None, "", None, None, None)
}
