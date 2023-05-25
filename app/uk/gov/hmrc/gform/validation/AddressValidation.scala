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

package uk.gov.hmrc.gform.validation
import cats.Monoid
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._

class AddressValidation[D <: DataOrigin](implicit messages: Messages, sse: SmartStringEvaluator) {

  val cvh = new ComponentsValidatorHelper()

  def validateAddress(
    fieldValue: FormComponent,
    address: Address
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): ValidatedType[Unit] = {

    val configurableMandatoryFields: Set[Atom] = address.configurableMandatoryAtoms

    val addressValueOf: Atom => Seq[String] = suffix =>
      formModelVisibilityOptics.data
        .get(fieldValue.atomicFormComponentId(suffix))
        .toSeq
        .flatMap(_.toSeq)

    def blankAtomicModelComponentId(atom: Atom): Option[ModelComponentId.Atomic] = {
      val atomicFcId = fieldValue.atomicFormComponentId(atom)
      addressValueOf(atom).filterNot(_.trim.isEmpty()).headOption.fold(Option(atomicFcId))(_ => Option.empty)
    }

    def validateRequiredFieldSub(value: Atom, str: String) =
      validateRequiredField(value, str, fieldValue)(addressValueOf(value))

    def validateRequiredAtom(atom: Atom, messageKey: String) =
      blankAtomicModelComponentId(atom).fold(validationSuccess) { id =>
        val placeholder = fieldValue.errorShortName
          .map(_.trasform(_ + " ", identity))
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.value())
        validationFailure(id, fieldValue, messageKey, Some(placeholder :: Nil), "")
      }

    def streetValidation(streetName: Atom) = lengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    def ukStreetValidation(streetName: Atom) = ukLengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    def validateCity(configurableMandatoryFields: Set[Atom]): ValidatedType[Unit] =
      if (configurableMandatoryFields(Address.street3)) {
        blankAtomicModelComponentId(Address.street3).fold(validationSuccess) { id =>
          val placeholder = fieldValue.errorShortName
            .map(_.trasform(_ + " ", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.value())
          validationFailure(id, fieldValue, "generic.error.address.town.city.required", Some(placeholder :: Nil), "")
        }
      } else {
        validationSuccess
      }

    val validatedResult: List[ValidatedType[Unit]] = addressValueOf(Address.uk) match {
      case "true" :: Nil =>
        List(
          validateRequiredAtom(Address.street1, "generic.error.address.building.street.required"),
          ukStreetValidation(Address.street1),
          ukStreetValidation(Address.street2),
          ukStreetValidation(Address.street3),
          ukStreetValidation(Address.street4),
          validateCity(configurableMandatoryFields),
          validateRequiredAtom(Address.postcode, "generic.error.address.postcode.required"),
          validateForbiddenField(Address.country, fieldValue)(addressValueOf(Address.country)),
          validatePostcode(Address.postcode, fieldValue)(addressValueOf(Address.postcode))
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

  private def validatePostcode(value: Atom, fieldValue: FormComponent) =
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
    xs: Seq[String]
  ): ValidatedType[Unit] = {
    def combineErrors(str: String, vars: List[String]) = mkErrors(fieldValue, atomicFcId)(str, vars)
    val First = "[1]$".r.unanchored
    val Second = "[2]$".r.unanchored
    val Third = "[3]$".r.unanchored
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), atomicFcId.atom.value) match {
      case (Nil, _) => validationSuccess
      case (value :: Nil, First()) if value.length > ValidationValues.addressLine =>
        val placeholder = fieldValue.errorShortNameStart
          .map(_.trasform(_ + " building and street", identity))
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.trasform(_ => "Building and street", _ => "").value())
        val vars: List[String] = placeholder :: ValidationValues.addressLine.toString :: Nil
        combineErrors("generic.error.address.building.street.maxLength", vars)
      case (value :: Nil, Second()) if value.length > ValidationValues.addressLine =>
        val placeholder = fieldValue.errorShortNameStart
          .map(_.trasform(_ + " building and street", identity))
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.trasform(_ => "Building and street", _ => "").value())
        val vars: List[String] = placeholder :: ValidationValues.addressLine.toString :: Nil
        combineErrors("generic.error.address.building.street.line2.maxLength", vars)
      case (value :: Nil, Third()) if value.length > ValidationValues.addressLine =>
        val placeholder = fieldValue.errorShortNameStart
          .map(_.trasform(_ + " town or city", identity))
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.trasform(_ => "Town or city", _ => "").value())
        val vars: List[String] = placeholder :: ValidationValues.addressLine.toString :: Nil
        combineErrors("generic.error.address.town.city.maxLength", vars)
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        val placeholder = fieldValue.errorShortNameStart
          .map(_.trasform(_ + " county", identity))
          .flatMap(_.nonBlankValue())
          .getOrElse(SmartString.blank.trasform(_ => "County", _ => "").value())
        val vars: List[String] = placeholder :: ValidationValues.addressLine4.toString :: Nil
        combineErrors("generic.error.address.county.maxLength", vars)
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
  ): ValidatedType[Unit] = {
    val placeholder = fieldValue.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
    stringValidator(
      !PostcodeLookupValidation.checkPostcode(_),
      mkErrors(fieldValue, atomicFcId)("generic.error.address.postcode.real", placeholder :: Nil)
    )(xs)
  }

  private def countryValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    stringValidator(
      _.length > ValidationValues.countryLimit,
      mkErrors(fieldValue, atomicFcId)(
        "internationalAddress.country.error.maxLength",
        ValidationValues.countryLimit.toString :: Nil
      )
    )(xs)

  private def stringValidator(
    predicate: String => Boolean,
    onError: ValidatedType[Unit]
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if predicate(value) => onError
      case _                                => validationSuccess
    }
}
