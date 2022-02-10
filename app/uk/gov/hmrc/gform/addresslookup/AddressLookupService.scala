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

package uk.gov.hmrc.gform.addresslookup

import cats.implicits._
import scala.concurrent.{ ExecutionContext, Future }
import scala.language.higherKinds
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.AccessCode
import uk.gov.hmrc.gform.sharedmodel.form.{ Form, FormData, FormIdData, UserData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, NotFound, ServiceResponse }
import uk.gov.hmrc.http.HeaderCarrier

sealed trait AddressLookupService[F[_]] {
  def postcodeLookup(
    formComponent: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
  )(implicit
    hc: HeaderCarrier
  ): F[Option[AddressLookupResult]]

  def saveAddress(
    form: Form,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId,
    addressId: String
  )(implicit
    hc: HeaderCarrier
  ): F[Unit]

  def cleanAddress(
    form: Form,
    maybeAccessCode: Option[AccessCode],
    formComponentId: FormComponentId
  )(implicit
    hc: HeaderCarrier
  ): F[Unit]
}

object AddressLookupService {
  def apply(
    addressLookupConnector: AddressLookupConnector[Future],
    gformConnector: GformConnector
  )(implicit
    ex: ExecutionContext
  ): AddressLookupService[Future] = new AddressLookupService[Future] {
    def postcodeLookup(
      formComponent: FormComponent,
      formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Browser]
    )(implicit
      hc: HeaderCarrier
    ): Future[Option[AddressLookupResult]] = {

      val postcodeComponentId: ModelComponentId.Atomic =
        formComponent.atomicFormComponentId(formtemplate.PostcodeLookup.postcode)
      val filterComponentId: ModelComponentId.Atomic =
        formComponent.atomicFormComponentId(formtemplate.PostcodeLookup.filter)

      val maybePostcode: Option[String] = formModelVisibilityOptics.data.one(postcodeComponentId)
      val maybeFilter: Option[String] = formModelVisibilityOptics.data.one(filterComponentId)

      maybePostcode match {
        case None => Future.successful(None)
        case Some(postcode) =>
          val request: PostcodeLookup.Request = PostcodeLookup.Request(
            postcode,
            maybeFilter
          )

          addressLookupConnector.postcodeLookup(request).map {
            case ServiceResponse(postcodeLookup)   => Some(AddressLookupResult(request, postcodeLookup))
            case CannotRetrieveResponse | NotFound => throw new Exception("Cannot retrieve PostcodeLookup data")
          }
      }
    }
    def saveAddress(
      form: Form,
      maybeAccessCode: Option[AccessCode],
      formComponentId: FormComponentId,
      addressId: String
    )(implicit
      hc: HeaderCarrier
    ): Future[Unit] = {
      val isAddressIdValid: Boolean = form.thirdPartyData.postcodeLookup
        .flatMap(_.get(formComponentId))
        .flatMap(_.addresses)
        .fold(false)(_.exists(_.id === addressId))

      if (isAddressIdValid) {
        val updatedThirdPartyData = form.thirdPartyData.updateSelectedAddresses(formComponentId, addressId)
        val formIdData: FormIdData = FormIdData.fromForm(form, maybeAccessCode)
        val userData: UserData = UserData(
          formData = form.formData,
          formStatus = form.status,
          visitsIndex = form.visitsIndex,
          thirdPartyData = updatedThirdPartyData,
          componentIdToFileId = form.componentIdToFileId
        )
        gformConnector.updateUserData(formIdData, userData)
      } else
        throw new IllegalArgumentException(
          s"AddressId: $addressId for FormComponentId $formComponentId do not exists in stored data"
        )
    }

    def cleanAddress(
      form: Form,
      maybeAccessCode: Option[AccessCode],
      formComponentId: FormComponentId
    )(implicit
      hc: HeaderCarrier
    ): Future[Unit] = {

      val postcodeComponentId: ModelComponentId.Atomic =
        formComponentId.toAtomicFormComponentId(formtemplate.PostcodeLookup.postcode)
      val filterComponentId: ModelComponentId.Atomic =
        formComponentId.toAtomicFormComponentId(formtemplate.PostcodeLookup.filter)

      val formIdData: FormIdData = FormIdData.fromForm(form, maybeAccessCode)
      val userData: UserData = UserData(
        formData = FormData(
          form.formData.fields.filterNot(formField =>
            formField.id === postcodeComponentId || formField.id === filterComponentId
          )
        ),
        formStatus = form.status,
        visitsIndex = form.visitsIndex,
        thirdPartyData = form.thirdPartyData,
        componentIdToFileId = form.componentIdToFileId
      )
      gformConnector.updateUserData(formIdData, userData)
    }
  }

}