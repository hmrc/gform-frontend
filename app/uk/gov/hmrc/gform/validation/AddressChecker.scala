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
import play.api.i18n.Messages
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.BaseComponentId
import uk.gov.hmrc.gform.models.ids.IndexedComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.ids.ModelComponentId.Atomic
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.CheckerServiceHelper._
import uk.gov.hmrc.gform.validation.ComponentsValidatorHelper.errors
import uk.gov.hmrc.gform.validation.ValidationUtil.GformError

import ComponentChecker._

class AddressChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent = context.formComponent
    formComponent match {
      case IsAddress(address) =>
        val checker = new AddressCheckerHelper[D]()
        checker.validateAddress(context.formComponent, address)(context.formModelVisibilityOptics)
      case _ => throw new IllegalArgumentException("FormComponent is not a Address")
    }
  }
}

class AddressCheckerHelper[D <: DataOrigin](implicit messages: Messages, sse: SmartStringEvaluator) {

  implicit val atomicValueForReport = new ValueForReport[Atomic] {
    def valueForReport(): Atomic =
      Atomic(IndexedComponentId.Pure(BaseComponentId("reportBaseComponentIdValue")), Atom("reportAtomLabel"))
  }

  val cvh = new ComponentsValidatorHelper()

  def validateAddress(
    fieldValue: FormComponent,
    address: Address
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): CheckProgram[Unit] = {

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

    def validateRequiredAtom(atom: Atom, messageKey: String, defaultPlaceholder: String = SmartString.blank.value()) = {
      val maybeAtom = blankAtomicModelComponentId(atom)
      maybeAtom.foldProgram(
        onNone = successProgram(()),
        onSomeFun = { id =>
          val placeholder = fieldValue.errorShortName
            .map(_.transform(_ + " ", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(defaultPlaceholder)
          validationFailure(id, fieldValue, messageKey, Some(placeholder :: Nil), "")
        }
      )
    }

    def streetValidation(streetName: Atom) = lengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    def ukStreetValidation(streetName: Atom) = ukLengthValidation(streetName, fieldValue)(addressValueOf(streetName))

    def validateCity(configurableMandatoryFields: Set[Atom]): CheckProgram[Unit] =
      ifProgram(
        cond = configurableMandatoryFields(Address.street3),
        thenProgram = blankAtomicModelComponentId(Address.street3).fold(successProgram(())) { id =>
          val placeholder = fieldValue.errorShortName
            .map(_.transform(_ + " ", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.value())
          validationFailure(id, fieldValue, "generic.error.address.town.city.required", Some(placeholder :: Nil), "")

        },
        elseProgram = successProgram(())
      )

    ifProgram(
      cond = addressValueOf(Address.uk) == "true" :: Nil,
      thenProgram = List(
        validateRequiredAtom(
          Address.street1,
          "generic.error.address.building.street.required",
          SmartString.blank.transform(_ => "address ", _ => "").value()
        ),
        ukStreetValidation(Address.street1),
        ukStreetValidation(Address.street2),
        ukStreetValidation(Address.street3),
        ukStreetValidation(Address.street4),
        validateCity(configurableMandatoryFields),
        validateRequiredAtom(Address.postcode, "generic.error.address.postcode.required"),
        validateForbiddenField(Address.country, fieldValue)(addressValueOf(Address.country)),
        validatePostcode(Address.postcode, fieldValue)(addressValueOf(Address.postcode))
      ).nonShortCircuitProgram,
      elseProgram = List(
        validateRequiredFieldSub(Address.street1, messages("internationalAddress.line1.label")),
        streetValidation(Address.street1),
        streetValidation(Address.street2),
        streetValidation(Address.street3),
        streetValidation(Address.street4),
        validateForbiddenField(Address.postcode, fieldValue)(addressValueOf(Address.postcode)),
        validateRequiredFieldSub(Address.country, messages("internationalAddress.country.label")),
        countryLengthValidation(Address.country, fieldValue)(addressValueOf(Address.country))
      ).nonShortCircuitProgram
    )
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
  ): CheckProgram[Unit] = {
    def combineErrors(str: String, vars: List[String]) = errorProgram[Unit](mkErrors(fieldValue, atomicFcId)(str, vars))

    val (values, atom) = (xs.filterNot(_.isEmpty()), atomicFcId.atom.value)
    val theOnlyValue = values match {
      case value :: Nil => Some(value)
      case _            => None
    }
    switchProgram(
      switchCase(
        cond = values.isEmpty,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = atom.endsWith("4") && theOnlyValue.map(_.length > ValidationValues.addressLine4).getOrElse(false),
        thenProgram = {
          val vars: List[String] = ValidationValues.addressLine4.toString :: Nil
          combineErrors("address.line4.error.maxLength", vars)
        }
      ),
      switchCase(
        cond = theOnlyValue.map(_.length > ValidationValues.addressLine).getOrElse(false),
        thenProgram = {
          val vars: List[String] = atomicFcId.atom.value.takeRight(1) :: ValidationValues.addressLine.toString :: Nil
          combineErrors("address.line.error.maxLength", vars)
        }
      )
    )(elseProgram = successProgram(()))
  }

  private def ukAddressLineValidation(fieldValue: FormComponent, atomicFcId: ModelComponentId.Atomic)(
    xs: Seq[String]
  ): CheckProgram[Unit] = {
    def combineErrors(str: String, vars: List[String]) = errorProgram[Unit](mkErrors(fieldValue, atomicFcId)(str, vars))

    val (values, atom) = (xs.filterNot(_.isEmpty()), atomicFcId.atom.value)
    val theOnlyValue = values match {
      case value :: Nil => Some(value)
      case _            => None
    }

    switchProgram(
      switchCase(
        cond = values.isEmpty,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = atom.endsWith("1") && theOnlyValue.map(_.length > ValidationValues.addressLine).getOrElse(false),
        thenProgram = {
          val placeholder = fieldValue.errorShortNameStart
            .map(_.transform(_ + " line 1", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "Address line 1", _ => "").value())
          val vars: List[String] = placeholder :: ValidationValues.addressLine.toString :: Nil
          combineErrors("generic.error.address.building.street.maxLength", vars)
        }
      ),
      switchCase(
        cond = atom.endsWith("2") && theOnlyValue.map(_.length > ValidationValues.addressLine).getOrElse(false),
        thenProgram = {
          val placeholder = fieldValue.errorShortNameStart
            .map(_.transform(_ + " line 2", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "Address line 2", _ => "").value())
          val vars: List[String] = placeholder :: ValidationValues.addressLine.toString :: Nil
          combineErrors("generic.error.address.building.street.line2.maxLength", vars)
        }
      ),
      switchCase(
        cond = atom.endsWith("3") && theOnlyValue.map(_.length > ValidationValues.addressLine).getOrElse(false),
        thenProgram = {
          val placeholder = fieldValue.errorShortNameStart
            .map(_.transform(_ + " town or city", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "Town or city", _ => "").value())
          val vars: List[String] = placeholder :: ValidationValues.addressLine.toString :: Nil
          combineErrors("generic.error.address.town.city.maxLength", vars)
        }
      ),
      switchCase(
        cond = atom.endsWith("4") && theOnlyValue.map(_.length > ValidationValues.addressLine4).getOrElse(false),
        thenProgram = {
          val placeholder = fieldValue.errorShortNameStart
            .map(_.transform(_ + " county", identity))
            .flatMap(_.nonBlankValue())
            .getOrElse(SmartString.blank.transform(_ => "County", _ => "").value())
          val vars: List[String] = placeholder :: ValidationValues.addressLine4.toString :: Nil
          combineErrors("generic.error.address.county.maxLength", vars)
        }
      )
    )(elseProgram = successProgram(()))
  }

  private def mkErrors(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    str: String,
    vars: List[String]
  ): GformError =
    Map[ModelComponentId, Set[String]](atomicFcId -> errors(fieldValue, str, Some(vars)))

  private def postcodeValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): CheckProgram[Unit] = {
    val placeholder = fieldValue.errorExample.flatMap(_.nonBlankValue()).map(s => s", $s").getOrElse("")
    stringValidator(
      !PostcodeLookupValidation.checkPostcode(_),
      errorProgram(mkErrors(fieldValue, atomicFcId)("generic.error.address.postcode.real", placeholder :: Nil))
    )(xs)
  }

  private def countryValidation(
    fieldValue: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): CheckProgram[Unit] =
    stringValidator(
      _.length > ValidationValues.countryLimit,
      errorProgram(
        mkErrors(fieldValue, atomicFcId)(
          "internationalAddress.country.error.maxLength",
          ValidationValues.countryLimit.toString :: Nil
        )
      )
    )(xs)

  private def stringValidator(
    predicate: String => Boolean,
    onError: CheckProgram[Unit]
  )(
    xs: Seq[String]
  ): CheckProgram[Unit] = {
    val isValid = xs.filterNot(_.isEmpty) match {
      case value :: Nil if predicate(value) => false
      case _                                => true
    }
    ifProgram(
      cond = isValid,
      thenProgram = successProgram(()),
      elseProgram = onError
    )

  }
}
