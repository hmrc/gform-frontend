/*
 * Copyright 2022 HM Revenue & Customs
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

package uk.gov.hmrc.gform.sharedmodel.form

import cats.data.NonEmptyList
import cats.implicits._
import play.api.libs.json.{ Format, Json, OFormat }
import uk.gov.hmrc.gform.addresslookup.{ AddressLookupResult, PostcodeLookup }
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, emailFieldId }
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.des.DesRegistrationResponse
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Address, FormComponentId, JsonUtils }
import uk.gov.hmrc.gform.sharedmodel.{ BooleanExprCache, DataRetrieveId, DataRetrieveResult, NotChecked, Obligations }

case class ThirdPartyData(
  desRegistrationResponse: Option[DesRegistrationResponse],
  obligations: Obligations,
  emailVerification: Map[EmailFieldId, EmailAndCode],
  queryParams: QueryParams,
  reviewData: Option[Map[String, String]] = None,
  booleanExprCache: BooleanExprCache,
  dataRetrieve: Option[Map[DataRetrieveId, DataRetrieveResult]],
  postcodeLookup: Option[Map[FormComponentId, AddressLookupResult]],
  selectedAddresses: Option[Map[FormComponentId, String]],
  enteredAddresses: Option[Map[FormComponentId, FormData]]
) {

  def enteredAddressFor(formComponentId: FormComponentId): Option[FormData] = for {
    adresses <- enteredAddresses
    formData <- adresses.get(formComponentId)
  } yield formData

  def enteredAddressPostcode(formComponentId: FormComponentId): Option[String] =
    enteredAddressFor(formComponentId).flatMap { formData =>
      val lookup = formData.toData

      val key = ModelComponentId.atomicCurry(formComponentId.modelComponentId.indexedComponentId)(
        uk.gov.hmrc.gform.sharedmodel.formtemplate.PostcodeLookup.postcode
      )

      lookup.get(key)
    }

  def addressRecordFor(formComponentId: FormComponentId): Option[PostcodeLookup.AddressRecord] = for {
    lookup              <- postcodeLookup
    selections          <- selectedAddresses
    addressId           <- selections.get(formComponentId)
    addressLookupResult <- lookup.get(formComponentId)
    addresses           <- addressLookupResult.response.addresses
    address             <- addresses.find(_.id === addressId)
  } yield address

  private def addressFor(formComponentId: FormComponentId): Option[Either[FormData, PostcodeLookup.AddressRecord]] =
    enteredAddressFor(formComponentId).map(Left(_)).orElse(addressRecordFor(formComponentId).map(Right(_)))

  def addressLines(formComponentId: FormComponentId) = addressFor(formComponentId).map {

    case Left(formData) =>
      val lookup = formData.toData
      val lines: NonEmptyList[String] =
        Address.summaryPageFields(formComponentId.modelComponentId.indexedComponentId).map { modelCompoentIdAtomic =>
          lookup.get(modelCompoentIdAtomic).getOrElse("")
        }
      lines.toList.filter(_.nonEmpty)

    case Right(addressRecord) =>
      import addressRecord.address._
      List(line1, line2, line3, line4, town, postcode).filter(_.nonEmpty)
  }

  def addressesFor(
    formComponentId: FormComponentId
  ): Option[(NonEmptyList[PostcodeLookup.AddressRecord], AddressLookupResult)] = for {
    lookup              <- postcodeLookup
    addressLookupResult <- lookup.get(formComponentId)
    addresses           <- addressLookupResult.response.addresses
  } yield (addresses, addressLookupResult)

  def enteredAddressDataFor(
    formComponentId: FormComponentId
  ): Option[FormData] = for {
    lookup   <- enteredAddresses
    formData <- lookup.get(formComponentId)
  } yield formData

  def enteredAddressDataForWithFallback(
    formComponentId: FormComponentId
  ): Option[FormData] = enteredAddressDataFor(formComponentId).orElse(
    addressRecordFor(formComponentId).map { addressRecord =>
      import addressRecord.address
      FormData(
        List(
          Address.street1  -> address.line1,
          Address.street2  -> address.line2,
          Address.street3  -> address.line3,
          Address.street4  -> address.line4,
          Address.postcode -> address.postcode,
          Address.uk       -> "true"
        ).map { case (atom, value) =>
          FormField(formComponentId.toAtomicFormComponentId(atom), value)
        }
      )
    }
  )

  def addressSelectionFor(formComponentId: FormComponentId): Option[String] = for {
    selections <- selectedAddresses
    addressId  <- selections.get(formComponentId)
  } yield addressId

  def updateDataRetrieve(dataRetrieveResult: Option[DataRetrieveResult]): ThirdPartyData = dataRetrieveResult match {
    case Some(drd @ DataRetrieveResult(id, _, _)) =>
      copy(dataRetrieve = dataRetrieve match {
        case None      => Some(Map(id -> drd))
        case Some(map) => Some(map + (id -> drd))
      })
    case None => this
  }

  def updatePostcodeLookup(postcodeLookupData: Option[(FormComponentId, AddressLookupResult)]): ThirdPartyData =
    postcodeLookupData.fold(this) { case (formComponentId, response) =>
      val updatedPostcodeLookup = postcodeLookup.getOrElse(Map.empty) + (formComponentId -> response)
      this.copy(postcodeLookup = Some(updatedPostcodeLookup))
    }

  def updateSelectedAddresses(formComponentId: FormComponentId, addressId: String): ThirdPartyData = {
    val updatedSelectedAddresses = selectedAddresses.getOrElse(Map.empty) + (formComponentId -> addressId)
    val updatedEnteredAddresses = enteredAddresses.map(_ - formComponentId)
    this.copy(selectedAddresses = Some(updatedSelectedAddresses), enteredAddresses = updatedEnteredAddresses)
  }

  def updateEnteredAddresses(formComponentId: FormComponentId, formData: FormData): ThirdPartyData = {
    val updatedEnteredAddresses = enteredAddresses.getOrElse(Map.empty) + (formComponentId -> formData)
    this.copy(enteredAddresses = Some(updatedEnteredAddresses))
  }

  def updateFrom(vr: Option[ValidatorsResult]): ThirdPartyData =
    vr match {
      case Some(ValidatorsResult(Some(desRegistrationResponse), m)) =>
        ThirdPartyData(
          Some(desRegistrationResponse),
          obligations,
          emailVerification ++ m,
          queryParams,
          reviewData,
          booleanExprCache,
          dataRetrieve,
          postcodeLookup,
          selectedAddresses,
          enteredAddresses
        )
      case Some(ValidatorsResult(None, m)) =>
        ThirdPartyData(
          desRegistrationResponse,
          obligations,
          emailVerification ++ m,
          queryParams,
          reviewData,
          booleanExprCache,
          dataRetrieve,
          postcodeLookup,
          selectedAddresses,
          enteredAddresses
        )
      case _ => this
    }

  def reviewComments: Option[String] = reviewData.flatMap(_.get("caseworkerComment"))
}

object ThirdPartyData {
  val empty =
    ThirdPartyData(None, NotChecked, Map.empty, QueryParams.empty, None, BooleanExprCache.empty, None, None, None, None)
  implicit val formatMap: Format[Map[EmailFieldId, EmailAndCode]] =
    JsonUtils.formatMap(a => emailFieldId(FormComponentId(a)), _.value)
  implicit val formatDataRetrieve: Format[Map[DataRetrieveId, DataRetrieveResult]] =
    JsonUtils.formatMap(a => DataRetrieveId(a), _.value)
  implicit val formatPostcodeLookup: Format[Map[FormComponentId, AddressLookupResult]] =
    JsonUtils.formatMap(a => FormComponentId(a), _.value)
  implicit val formatSelectedAddresses: Format[Map[FormComponentId, String]] =
    JsonUtils.formatMap(a => FormComponentId(a), _.value)
  implicit val formatEnteredAddresses: Format[Map[FormComponentId, FormData]] =
    JsonUtils.formatMap(a => FormComponentId(a), _.value)
  implicit val format: OFormat[ThirdPartyData] = Json.format
}
