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
import uk.gov.hmrc.gform.lookup.{ LookupLabel, LookupRegistry }
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.sharedmodel.SmartString

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

  def blankAtomicModelComponentId(atom: Atom)(xs: Seq[String]): Option[ModelComponentId.Atomic] = {
    val atomicFcId = formComponent.atomicFormComponentId(atom)
    xs.filterNot(_.trim.isEmpty()).headOption.fold(Option(atomicFcId))(_ => Option.empty)
  }

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
    runValidation(
      OverseasAddress.line1,
      validateMandatory(
        OverseasAddress.line1,
        "generic.error.overseas.line1.required",
        true,
        ValidationValues.addressLine
      ),
      lengthLimitValidation(
        OverseasAddress.line1,
        ValidationValues.addressLine,
        "generic.error.overseas.line1.maxLength",
        formComponent.errorShortNameStart
          .map(s => messages("generic.error.overseas.line1", s.value()))
          .getOrElse(messages("generic.error.overseas.Line1"))
      )
    )

  private def validateLine2(configurableMandatoryFields: Set[Atom]): ValidatedType[Unit] =
    runValidation(
      OverseasAddress.line2,
      validateMandatory(
        OverseasAddress.line2,
        "generic.error.overseas.line2.required",
        configurableMandatoryFields(OverseasAddress.line2),
        ValidationValues.addressLine
      ),
      lengthLimitValidation(
        OverseasAddress.line2,
        ValidationValues.addressLine,
        "generic.error.overseas.line2.maxLength",
        formComponent.errorShortNameStart
          .map(s => messages("generic.error.overseas.line2", s.value()))
          .getOrElse(messages("generic.error.overseas.Line2"))
      )
    )

  private def validateLine3(): ValidatedType[Unit] =
    runValidation(
      OverseasAddress.line3,
      validateMandatory(
        OverseasAddress.line3,
        "generic.error.overseas.line3.required",
        false,
        ValidationValues.addressLine
      ),
      lengthLimitValidation(
        OverseasAddress.line3,
        ValidationValues.addressLine,
        "generic.error.overseas.line3.maxLength",
        formComponent.errorShortNameStart
          .map(s => messages("generic.error.overseas.line3", s.value()))
          .getOrElse(messages("generic.error.overseas.Line3"))
      )
    )

  private def validateCity(configurableOptionalFields: Set[Atom]): ValidatedType[Unit] =
    runValidation(
      OverseasAddress.city,
      validateMandatory(
        OverseasAddress.city,
        "generic.error.overseas.town.city.required",
        !configurableOptionalFields(OverseasAddress.city),
        ValidationValues.overseasCity
      ),
      lengthLimitValidation(
        OverseasAddress.city,
        ValidationValues.overseasCity,
        "generic.error.overseas.town.city.maxLength",
        formComponent.errorShortNameStart
          .map(s => messages("generic.error.overseas.town.city", s.value()))
          .getOrElse(messages("generic.error.overseas.Town.City"))
      )
    )

  private def validatePostcode(configurableMandatoryFields: Set[Atom]): ValidatedType[Unit] =
    runValidation(
      OverseasAddress.postcode,
      validateMandatory(
        OverseasAddress.postcode,
        "generic.error.overseas.postcode.required",
        configurableMandatoryFields(OverseasAddress.postcode),
        ValidationValues.postcodeLimit
      ),
      lengthLimitValidation(
        OverseasAddress.postcode,
        ValidationValues.postcodeLimit,
        "generic.error.overseas.postcode.maxLength",
        formComponent.errorShortNameStart
          .map(s => messages("generic.error.overseas.postcode", s.value()))
          .getOrElse(messages("generic.error.overseas.Postcode"))
      )
    )

  private def validateCountryMandatory(): ValidatedType[Unit] =
    runValidation(
      OverseasAddress.country,
      validateMandatory(
        OverseasAddress.country,
        "generic.error.overseas.country.required",
        true,
        ValidationValues.countryLimit
      ),
      lengthLimitValidation(
        OverseasAddress.country,
        ValidationValues.countryLimit,
        "generic.error.overseas.country.maxLength",
        formComponent.errorShortNameStart
          .map(s => messages("generic.error.overseas.country", s.value()))
          .getOrElse(messages("generic.error.overseas.Country"))
      )
    )

  private def validateCountry(): ValidatedType[Unit] =
    List(
      validateCountryMandatory(),
      validateCountryLookupValue()
    ).combineAll

  private def validateCountryLookupValue() =
    runValidation(OverseasAddress.country, lookupCountryValidator)

  private def validateMandatory(
    atom: Atom,
    labelMessageKey: String,
    isMandatory: Boolean,
    maxLength: Int
  ): Seq[String] => ValidatedType[Unit] =
    if (isMandatory)
      validateRequiredField(atom, labelMessageKey) _
    else _ => validationSuccess

  private def validateRequiredField(atom: Atom, messageKey: String)(xs: Seq[String]) =
    blankAtomicModelComponentId(atom)(xs).fold(validationSuccess) { id =>
      val placeholder = formComponent.errorShortName
        .map(_.transform(_ + " ", identity))
        .flatMap(_.nonBlankValue())
        .getOrElse(SmartString.blank.value())
      validationFailure(id, formComponent, messageKey, Some(placeholder :: Nil), "")
    }

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
    labelMessageKey: String,
    placeholder: String
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > limit =>
        val vars: List[String] = placeholder :: limit.toString :: Nil
        val atomicFcId: ModelComponentId.Atomic = formComponent.atomicFormComponentId(atom)
        mkErrors(atomicFcId)(labelMessageKey, "", vars)
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
          _ =>
            mkErrors(atomicFcId)(
              "generic.error.overseas.nomatch",
              messages("overseasAddress.country.label"),
              List(value)
            ),
          _ => validationSuccess
        )
    }
}
