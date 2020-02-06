/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.validation
import cats.Monoid
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

class AddressValidation[D <: DataOrigin](implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) {

  val cvh = new ComponentsValidatorHelper()

  def validateAddress(
    fieldValue: FormComponent,
    address: Address
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): ValidatedType[Unit] = {
    val addressValueOf: Atom => Seq[String] = suffix =>
      formModelVisibilityOptics.data
        .get(fieldValue.atomicFormComponentId(suffix))
        .toSeq
        .flatMap(_.toSeq)

    def validateRequiredFieldSub(value: Atom, str: String) =
      validateRequiredField(value, str, fieldValue)(addressValueOf(value))

    def streetValidation(streetName: Atom) = lengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    def ukStreetValidation(streetName: Atom) = ukLengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    val validatedResult: List[ValidatedType[Unit]] = addressValueOf(Address.uk) match {
      case "true" :: Nil =>
        List(
          validateRequiredFieldSub(Address.street1, messages("ukAddress.line1.label")),
          ukStreetValidation(Address.street1),
          ukStreetValidation(Address.street2),
          ukStreetValidation(Address.street3),
          ukStreetValidation(Address.street4),
          validateRequiredFieldSub(Address.postcode, messages("ukAddress.postcode.label")),
          validateForbiddenField(Address.country, fieldValue)(addressValueOf(Address.country)),
          postcodeLengthValidation(Address.postcode, fieldValue)(addressValueOf(Address.postcode))
        )
      case _ =>
        List(
          validateRequiredFieldSub(Address.street1, messages("internationalAddress.line1.label")),
          streetValidation(Address.street1),
          streetValidation(Address.street2),
          streetValidation(Address.street3),
          streetValidation(Address.street4),
          validateForbiddenField(Address.postcode, fieldValue)(addressValueOf(Address.postcode)),
          validateRequiredFieldSub(Address.country, messages("internationalAddress.country.label")),
          countryLengthValidation(Address.country, fieldValue)(addressValueOf(Address.country))
        )
    }

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def validateRequiredField(value: Atom, errorPrefix: String, fieldValue: FormComponent) =
    cvh.validateRequired2(fieldValue, fieldValue.atomicFormComponentId(value), Some(errorPrefix)) _

  private def validateForbiddenField(value: Atom, fieldValue: FormComponent) =
    cvh.validateForbidden(fieldValue, fieldValue.atomicFormComponentId(value)) _

  private def lengthValidation(value: Atom, fieldValue: FormComponent) =
    addressLineValidation(fieldValue, fieldValue.atomicFormComponentId(value)) _

  private def ukLengthValidation(value: Atom, fieldValue: FormComponent) =
    ukAddressLineValidation(fieldValue, fieldValue.atomicFormComponentId(value)) _

  private def postcodeLengthValidation(value: Atom, fieldValue: FormComponent) =
    postcodeValidation(fieldValue, fieldValue.atomicFormComponentId(value)) _

  private def countryLengthValidation(value: Atom, fieldValue: FormComponent) =
    countryValidation(fieldValue, fieldValue.atomicFormComponentId(value)) _

  private def addressLineValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] = {
    def combineErrors(str: String, vars: List[String]) = mkErrors(fieldValue, atomicFcId)(str, vars)
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), atomicFcId.atom.value) match {
      case (Nil, _) => validationSuccess
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        val vars: List[String] = ValidationValues.addressLine4.toString :: Nil
        combineErrors("address.line4.error.maxLength", vars)
      case (value :: Nil, _) if value.length > ValidationValues.addressLine =>
        val vars: List[String] = atomicFcId.atom.value.takeRight(1) :: ValidationValues.addressLine.toString :: Nil
        combineErrors("address.line.error.maxLength", vars)
      case _ => validationSuccess
    }
  }

  private def ukAddressLineValidation(fieldValue: FormComponent, atomicFcId: ModelComponentId.Atomic)(
    xs: Seq[String]): ValidatedType[Unit] = {
    def combineErrors(str: String, vars: List[String]) = mkErrors(fieldValue, atomicFcId)(str, vars)
    val First = "[1]$".r.unanchored
    val Second = "[2]$".r.unanchored
    val Third = "[3]$".r.unanchored
    val Fourth = "[4]$".r.unanchored
    val addressLineVars: List[String] = ValidationValues.addressLine.toString :: Nil
    (xs.filterNot(_.isEmpty()), atomicFcId.atom.value) match {
      case (Nil, _) => validationSuccess
      case (value :: Nil, First()) if value.length > ValidationValues.addressLine =>
        combineErrors("ukAddress.line1.error.maxLength", addressLineVars)
      case (value :: Nil, Second()) if value.length > ValidationValues.addressLine =>
        combineErrors("ukAddress.line2.error.maxLength", addressLineVars)
      case (value :: Nil, Third()) if value.length > ValidationValues.addressLine =>
        combineErrors("ukAddress.line3.error.maxLength", addressLineVars)
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        val vars: List[String] = ValidationValues.addressLine4.toString :: Nil
        combineErrors("ukAddress.line4.error.maxLength", vars)
      case _ => validationSuccess
    }
  }

  private def mkErrors(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    str: String,
    vars: List[String]
  ): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](atomicFcId -> errors(fieldValue, str, Some(vars))).invalid

  private def postcodeValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    lengthLimitValidation(fieldValue, atomicFcId, ValidationValues.postcodeLimit, "ukAddress.postcode.error.maxLength")(
      xs)

  private def countryValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    lengthLimitValidation(
      fieldValue,
      atomicFcId,
      ValidationValues.countryLimit,
      "internationalAddress.country.error.maxLength")(xs)

  private def lengthLimitValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic,
    limit: Int,
    messageKey: String
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > limit =>
        val vars: List[String] = limit.toString :: Nil
        mkErrors(fieldValue, atomicFcId)(messageKey, vars)
      case _ => validationSuccess
    }
}
