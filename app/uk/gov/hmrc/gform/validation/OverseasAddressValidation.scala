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

package uk.gov.hmrc.gform.validation

import cats.Monoid
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.lookup.{ LookupLabel, LookupRegistry }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.validationSuccess

class OverseasAddressValidation[D <: DataOrigin](
  formComponent: FormComponent,
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  lookupRegistry: LookupRegistry
)(implicit
  langADT: LangADT,
  messages: Messages,
  sse: SmartStringEvaluator
) {

  val cvh = new ComponentsValidatorHelper()

  private val addressValueOf: Atom => Seq[String] = suffix =>
    formModelVisibilityOptics.data
      .get(formComponent.atomicFormComponentId(suffix))
      .toSeq
      .flatMap(_.toSeq)

  def validateOverseasAddress(
    overseasAddress: OverseasAddress
  ): ValidatedType[Unit] = {

    val configurableMandatoryFields: Set[Atom] = overseasAddress.configurableMandatoryAtoms
    val configurableOptionalFields: Set[Atom] = overseasAddress.configurableOptionalAtoms

    val validatedResult: List[ValidatedType[Unit]] =
      List(
        validateLine1(),
        validateLine2(configurableMandatoryFields),
        validateLine3(),
        validateCity(configurableOptionalFields),
        validatePostcode(configurableMandatoryFields),
        if (overseasAddress.countryLookup) validateCountry() else validateCountryMandatory()
      )

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def runValidation(atom: Atom, validators: Seq[String] => ValidatedType[Unit]*): ValidatedType[Unit] = {
    val valueOf = addressValueOf(atom)
    validators.toList.map(_(valueOf)).combineAll
  }

  private def validateLine1(): ValidatedType[Unit] =
    validateMandatoryAndLength(
      OverseasAddress.line1,
      "overseasAddress.line1.label",
      true,
      ValidationValues.addressLine
    )

  private def validateLine2(configurableMandatoryFields: Set[Atom]): ValidatedType[Unit] =
    validateMandatoryAndLength(
      OverseasAddress.line2,
      "overseasAddress.line2.label",
      configurableMandatoryFields(OverseasAddress.line2),
      ValidationValues.addressLine
    )

  private def validateLine3(): ValidatedType[Unit] =
    validateMandatoryAndLength(
      OverseasAddress.line3,
      "overseasAddress.line3.label",
      false,
      ValidationValues.addressLine
    )

  private def validateCity(configurableOptionalFields: Set[Atom]): ValidatedType[Unit] =
    validateMandatoryAndLength(
      OverseasAddress.city,
      "overseasAddress.city.label",
      !configurableOptionalFields(OverseasAddress.city),
      ValidationValues.overseasCity
    )

  private def validatePostcode(configurableMandatoryFields: Set[Atom]): ValidatedType[Unit] =
    validateMandatoryAndLength(
      OverseasAddress.postcode,
      "overseasAddress.postcode.label",
      configurableMandatoryFields(OverseasAddress.postcode),
      ValidationValues.postcodeLimit
    )

  private def validateCountryMandatory(): ValidatedType[Unit] =
    validateMandatoryAndLength(
      OverseasAddress.country,
      "overseasAddress.country.label",
      true,
      ValidationValues.countryLimit
    )

  private def validateCountry(): ValidatedType[Unit] =
    List(
      validateCountryMandatory(),
      validateCountryLookupValue()
    ).combineAll

  private def validateCountryLookupValue() =
    runValidation(OverseasAddress.country, lookupCountryValidator)

  private def validateMandatoryAndLength(
    atom: Atom,
    labelMessageKey: String,
    isMandatory: Boolean,
    maxLength: Int
  ): ValidatedType[Unit] = {

    val requiredValidator: Seq[String] => ValidatedType[Unit] =
      if (isMandatory)
        validateRequiredField(atom, messages(labelMessageKey))
      else _ => validationSuccess

    val lengthValidator = lengthValidation(atom, maxLength, labelMessageKey)
    runValidation(atom, requiredValidator, lengthValidator)
  }

  private def validateRequiredField(value: Atom, errorPrefix: String) =
    cvh.validateRequired2(formComponent, formComponent.atomicFormComponentId(value), Some(errorPrefix)) _

  private def lengthValidation(atom: Atom, limit: Int, labelMessageKey: String) =
    lengthLimitValidation(atom, limit, labelMessageKey) _

  private def mkErrors(
    atomicFcId: ModelComponentId.Atomic
  )(
    str: String,
    partLabel: String,
    vars: List[String]
  ): ValidatedType[Unit] =
    Map[ModelComponentId, Set[String]](atomicFcId -> errors(formComponent, str, Some(vars), partLabel)).invalid

  private def lengthLimitValidation(
    atom: Atom,
    limit: Int,
    labelMessageKey: String
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > limit =>
        val vars: List[String] = messages(labelMessageKey) :: limit.toString :: Nil
        val atomicFcId: ModelComponentId.Atomic = formComponent.atomicFormComponentId(atom)
        mkErrors(atomicFcId)("overseasAddress.error.maxLength", "", vars)
      case _ => validationSuccess
    }

  private def lookupCountryValidator(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty).headOption.fold(validationSuccess) { value =>
      val atomicFcId: ModelComponentId.Atomic = formComponent.atomicFormComponentId(OverseasAddress.country)
      ComponentValidator
        .lookupValidation(
          formComponent,
          lookupRegistry,
          Lookup(Register.Country, None),
          LookupLabel(value),
          formModelVisibilityOptics
        )
        .fold(
          _ => mkErrors(atomicFcId)("generic.error.lookup", messages("overseasAddress.country.label"), List.empty),
          _ => validationSuccess
        )
    }

}
