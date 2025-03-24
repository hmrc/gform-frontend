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
import play.api.libs.json.{ Format, JsNumber, JsObject, Json }
import uk.gov.hmrc.gform.addresslookup.PostcodeLookupRetrieve.AddressRecord
import uk.gov.hmrc.gform.auth.models._
import uk.gov.hmrc.gform.commons.HeaderCarrierUtil
import uk.gov.hmrc.gform.gform.CustomerId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.mappings.{ IRCT, IRSA, NINO, VATReg }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.objectStore.File
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, IsAddress, IsOverseasAddress }
import uk.gov.hmrc.gform.sharedmodel.{ AffinityGroupUtil, SubmissionRef, VariadicValue }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.{ DataEvent, ExtendedDataEvent }

import scala.concurrent.ExecutionContext

trait AuditService {

  def auditConnector: AuditConnector

  def getUserValues[D <: DataOrigin](
    thirdPartyData: ThirdPartyData,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): UserValues = {
    import scala.language.implicitConversions
    implicit def toOpt(str: String): Option[String] = Option.unless(str.isBlank)(str)

    val unitedKingdom: String = "United Kingdom"

    def getValueMap(components: List[FormComponent]): Map[String, String] = components
      .flatMap(_.multiValueId.toModelComponentIds)
      .flatMap { modelComponentId =>
        formModelVisibilityOptics.data.get(modelComponentId).map(modelComponentId -> _)
      }
      .map { case (modelComponentId, variadicValue) =>
        modelComponentId.toMongoIdentifier -> variadicValue.toSeq.mkString(",")
      }
      .toMap

    def getAddressMap(addressComponents: List[FormComponent]) = {
      def getThirdPartyDataAddresses: Map[String, AuditAddress] = {
        def auditAddressFromEntered(formData: Option[FormData], maybeUprn: Option[String]): AuditAddress = {
          def findByAtom(atom: String): String =
            formData
              .flatMap { fd =>
                fd.toData.find { case (mcId, _) => mcId.isAtomic(atom) }
              }
              .map { case (_, value) => value }
              .getOrElse("")

          val country: String = if (findByAtom("uk") == "true") unitedKingdom else findByAtom("country")

          AuditAddress(
            findByAtom("street1"),
            findByAtom("street2"),
            findByAtom("street3"),
            findByAtom("street4"),
            findByAtom("postcode"),
            findByAtom("town"),
            country,
            maybeUprn
          )
        }

        def auditAddressFromLookup(lookupAddress: AddressRecord): AuditAddress =
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

        def getUprn(thirdPartyData: ThirdPartyData, fc: FormComponentId, addressId: String): Option[String] = {
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
              selectedAddresses.get(fc).flatMap(addressId => getUprn(thirdPartyData, fc, addressId))
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

      def toAuditAddress(values: List[(ModelComponentId, VariadicValue)]): AuditAddress = {
        def findByAtom(primaryAtom: String, maybeAlternative: Option[String] = None): String = {
          val maybePrimary: Option[(ModelComponentId, VariadicValue)] = values.find { case (mcId, _) =>
            mcId.isAtomic(primaryAtom)
          }
          val result: Option[(ModelComponentId, VariadicValue)] = (maybePrimary, maybeAlternative) match {
            case (None, Some(alt)) => values.find { case (mcId, _) => mcId.isAtomic(alt) }
            case (_, _)            => maybePrimary
          }
          result.map { case (_, value) => value.toSeq.mkString(" ") }.getOrElse("")
        }

        val country: String = if (findByAtom("uk") == "true") unitedKingdom else findByAtom("country")

        AuditAddress(
          findByAtom("street1", Some("line1")),
          findByAtom("street2", Some("line2")),
          findByAtom("street3", Some("line3")),
          findByAtom("street4"),
          findByAtom("postcode"),
          findByAtom("city"),
          country,
          None
        )
      }

      val addressOnlyValues: List[(ModelComponentId, VariadicValue)] = addressComponents
        .flatMap(_.multiValueId.toModelComponentIds)
        .flatMap { modelComponentId =>
          formModelVisibilityOptics.data.get(modelComponentId).map(modelComponentId -> _)
        }

      val groupedAddressValues = addressOnlyValues.groupMap(_._1.indexedComponentId)(values => values)

      groupedAddressValues.map { case (_, listOfValues) =>
        listOfValues.head._1.toMongoIdentifier -> toAuditAddress(listOfValues)
      } ++ getThirdPartyDataAddresses
    }

    val addressComponents: List[FormComponent] = formModelVisibilityOptics.allFormComponents.filter {
      case IsAddress(_) | IsOverseasAddress(_) => true
      case _                                   => false
    }

    val componentsExcludingAddresses: List[FormComponent] =
      formModelVisibilityOptics.allFormComponents.diff(addressComponents)

    UserValues(
      getValueMap(componentsExcludingAddresses),
      getAddressMap(addressComponents)
    )
  }

  def sendFormCreateEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formCreated", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormResumeEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formResumed", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendSubmissionEvent[D <: DataOrigin](
    form: Form,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    retrievals: MaterialisedRetrievals,
    customerId: CustomerId,
    envelopeFiles: List[File]
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent(
      "formSubmitted",
      form,
      getUserValues(form.thirdPartyData, formModelVisibilityOptics),
      retrievals,
      customerId,
      envelopeFiles
    )

  def formSavedEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSaved", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormTimoutEvent[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formTimeout", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  def sendFormSignOut[D <: DataOrigin](
    form: Form,
    retrievals: MaterialisedRetrievals
  )(implicit ec: ExecutionContext, hc: HeaderCarrier): Unit =
    sendEvent("formSignOut", form, UserValues.empty, retrievals, CustomerId.empty, List.empty)

  private def sendEvent(
    auditType: String,
    form: Form,
    detail: UserValues,
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
    eventFor(form, getUserValues(form.thirdPartyData, formModelVisibilityOptics), retrievals, customerId)

  private def eventFor(
    form: Form,
    detail: UserValues,
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
    detail: UserValues,
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

    val userValues = Json.toJson(detail.userValues.filter(values => values._2.nonEmpty))
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

    val userAddressesJsObj: JsObject =
      if (detail.userAddresses.nonEmpty)
        Json.obj(
          "UserAddresses" -> detail.userAddresses
        )
      else
        Json.obj()

    Json.obj(
      "FormId"         -> form._id.value,
      "EnvelopeId"     -> form.envelopeId.value,
      "FormTemplateId" -> form.formTemplateId.value,
      "UserId"         -> form.userId.value,
      "CustomerId"     -> customerId.id,
      "UserValues"     -> userValues
    ) ++ userAddressesJsObj ++
      Json.obj(
        "UserInfo"      -> userInfo,
        "SubmissionRef" -> SubmissionRef(form.envelopeId).value
      ) ++ envelopeFilesJsObj
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

case class UserValues(
  userValues: Map[String, String],
  userAddresses: Map[String, AuditAddress]
)

object UserValues {
  lazy val empty: UserValues = UserValues(Map.empty, Map.empty)
}

case class AuditAddress(
  addressLine1: String,
  addressLine2: Option[String],
  addressLine3: Option[String],
  addressLine4: Option[String],
  postCode: String,
  country: Option[String],
  uprn: Option[String]
)

object AuditAddress {
  implicit val format: Format[AuditAddress] = Json.format[AuditAddress]
  lazy val empty: AuditAddress = AuditAddress("", None, None, None, "", None, None)

  def apply(
    al1: String,
    al2: Option[String],
    al3: Option[String],
    al4: Option[String],
    postCode: String,
    town: Option[String],
    country: Option[String],
    uprn: Option[String]
  ): AuditAddress =
    (al2, al3, al4, town) match {
      case (_, _, Some(l4), Some(t))      => AuditAddress(al1, al2, al3, Some(s"$l4 $t"), postCode, country, uprn)
      case (_, Some(_), None, Some(t))    => AuditAddress(al1, al2, al3, Some(t), postCode, country, uprn)
      case (Some(_), None, None, Some(t)) => AuditAddress(al1, al2, Some(t), None, postCode, country, uprn)
      case (None, None, None, Some(t))    => AuditAddress(al1, Some(t), None, None, postCode, country, uprn)
      case (_, _, _, None)                => AuditAddress(al1, al2, al3, al4, postCode, country, uprn)
    }
}
