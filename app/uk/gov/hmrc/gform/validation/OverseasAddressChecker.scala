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
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.sharedmodel.SmartString
import ComponentChecker._
import GformError._
import uk.gov.hmrc.gform.validation.ValidationUtil.GformError
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.lookup.LookupLabel
import uk.gov.hmrc.gform.sharedmodel.LangADT

class OverseasAddressChecker[D <: DataOrigin]() extends ComponentChecker[D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[GformError] = {
    val formComponent = context.formComponent
    formComponent match {
      case IsOverseasAddress(fc) => checkOverseasAddress(fc, context)
      case _                     => throw new IllegalArgumentException("FormComponent is not an OverseasAddress")
    }
  }

  private def checkOverseasAddress(overseasAddress: OverseasAddress, context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[GformError] = {

    val mandatoryFields: Set[Atom] = overseasAddress.configurableMandatoryAtoms
    val optionalFields: Set[Atom] = overseasAddress.configurableOptionalAtoms

    val errorShortNamePlaceholder = context.formComponent.errorShortName
      .map(_.transform(_ + " ", identity))
      .flatMap(_.nonBlankValue())
      .getOrElse(SmartString.blank.value())

    def line1RequiredOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line1)
      ifThenOp(
        cond = context.isAtomicValueBlank(atomicModelComponentId),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line1.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        )
      )
    }

    def line2RequiredOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line2)
      ifThenOp(
        cond = context.isAtomicValueBlank(atomicModelComponentId), //&& !mandatoryFields(OverseasAddress.line2),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line2.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        )
      )
    }

    val cityAtomicModelComponentId = context.atomicFcId(OverseasAddress.city)
    def cityRequiredOp() =
      ifThenOp(
        cond = context.isAtomicValueBlank(cityAtomicModelComponentId) || optionalFields(OverseasAddress.city),
        thenProgram = gformErrorOp(
          gformError(
            cityAtomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.town.city.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        )
      )

    def postcodeRequiredOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.postcode)
      ifThenOp(
        cond = context.isAtomicValueBlank(atomicModelComponentId) || !mandatoryFields(OverseasAddress.postcode),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.postcode.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        )
      )
    }

    val countryAtomicModelComponentId = context.atomicFcId(OverseasAddress.country)
    val countryRequiredOp =
      ifThenOp(
        cond = context.isAtomicValueBlank(countryAtomicModelComponentId),
        thenProgram = gformErrorOp(
          gformError(
            countryAtomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.country.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        )
      )

    def line1MaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line1)
      ifThenOp(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.addressLine),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line1.maxLength",
            Some(
              context.formComponent.errorShortNameStart
                .map(s => messages("generic.error.overseas.line1", s.value()))
                .getOrElse(messages("generic.error.overseas.Line1"))
                :: ValidationValues.addressLine.toString
                :: Nil
            )
          )
        )
      )
    }

    def line2MaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line2)
      ifThenOp(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.addressLine),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line2.maxLength",
            Some(
              context.formComponent.errorShortNameStart
                .map(s => messages("generic.error.overseas.line2", s.value()))
                .getOrElse(messages("generic.error.overseas.Line2"))
                :: ValidationValues.addressLine.toString
                :: Nil
            )
          )
        )
      )
    }

    val line3AtomicModelComponentId = context.atomicFcId(OverseasAddress.line3)
    def line3MaxLengthOp() =
      ifThenOp(
        cond = context.isAtomicValueExceedMaxLength(line3AtomicModelComponentId, ValidationValues.addressLine),
        thenProgram = gformErrorOp(
          gformError(
            line3AtomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line3.maxLength",
            Some(
              context.formComponent.errorShortNameStart
                .map(s => messages("generic.error.overseas.line3", s.value()))
                .getOrElse(messages("generic.error.overseas.Line3"))
                :: ValidationValues.addressLine.toString
                :: Nil
            )
          )
        )
      )

    def cityMaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.city)
      ifThenOp(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.overseasCity),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.town.city.maxLength",
            Some(
              context.formComponent.errorShortNameStart
                .map(s => messages("generic.error.overseas.town.city", s.value()))
                .getOrElse(messages("generic.error.overseas.Town.City"))
                :: ValidationValues.overseasCity.toString
                :: Nil
            )
          )
        )
      )
    }

    val postcodeAtomicModelComponentId = context.atomicFcId(OverseasAddress.postcode)
    def postcodeMaxLengthOp() =
      ifThenOp(
        cond = context.isAtomicValueExceedMaxLength(postcodeAtomicModelComponentId, ValidationValues.postcodeLimit),
        thenProgram = gformErrorOp(
          gformError(
            postcodeAtomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.postcode.maxLength",
            Some(
              context.formComponent.errorShortNameStart
                .map(s => messages("generic.error.overseas.postcode", s.value()))
                .getOrElse(messages("generic.error.overseas.Postcode"))
                :: ValidationValues.postcodeLimit.toString
                :: Nil
            )
          )
        )
      )

    def countryMaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.country)
      ifThenOp(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.countryLimit),
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.country.maxLength",
            Some(
              context.formComponent.errorShortNameStart
                .map(s => messages("generic.error.overseas.country", s.value()))
                .getOrElse(messages("generic.error.overseas.Country"))
                :: ValidationValues.countryLimit.toString
                :: Nil
            )
          )
        )
      )
    }

    def lookupCountryOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.country)
      val maybeValue = context.valueOf(atomicModelComponentId).filterNot(_.isEmpty).headOption
      ifThenOp(
        cond = maybeValue
          .fold(false) { value =>
            ComponentValidator
              .lookupValidation(
                context.formComponent,
                context.lookupRegistry,
                Lookup(Register.Country, None),
                LookupLabel(value),
                context.formModelVisibilityOptics
              )
              .isInvalid
          },
        thenProgram = gformErrorOp(
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.nomatch",
            Some(maybeValue.getOrElse("country") :: Nil)
          )
        )
      )
    }

    List(
      List(line1RequiredOp(), line1MaxLengthOp()).shortCircuitProgram,
      ifThenElseOp(
        cond = mandatoryFields(OverseasAddress.line2),
        thenProgram = List(line2RequiredOp(), line2MaxLengthOp()).shortCircuitProgram,
        elseProgram = ifThenOp(
          cond = context.nonBlankValueOf(line3AtomicModelComponentId).isDefined,
          thenProgram = line2MaxLengthOp()
        )
      ),
      ifThenOp(
        cond = context.nonBlankValueOf(line3AtomicModelComponentId).isDefined,
        thenProgram = line3MaxLengthOp()
      ),
      ifThenElseOp(
        cond = optionalFields(OverseasAddress.city),
        thenProgram = ifThenOp(
          cond = context.nonBlankValueOf(cityAtomicModelComponentId).isEmpty,
          thenProgram = cityMaxLengthOp()
        ),
        elseProgram = List(cityRequiredOp(), cityMaxLengthOp()).shortCircuitProgram
      ),
      ifThenElseOp(
        cond = mandatoryFields(OverseasAddress.postcode),
        thenProgram = List(postcodeRequiredOp(), postcodeMaxLengthOp()).shortCircuitProgram,
        elseProgram = ifThenOp(
          cond = context.nonBlankValueOf(postcodeAtomicModelComponentId).isDefined,
          thenProgram = postcodeMaxLengthOp()
        )
      ),
      ifThenElseOp(
        cond = overseasAddress.countryLookup,
        thenProgram = List(countryRequiredOp, countryMaxLengthOp(), lookupCountryOp()).shortCircuitProgram,
        elseProgram = List(countryRequiredOp, countryMaxLengthOp()).shortCircuitProgram
      )
    ).nonShortCircuitProgram
  }

}
