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
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.lookup.LookupLabel
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import ComponentChecker._
import GformError._

class OverseasAddressChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
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
  ): CheckProgram[Unit] = {

    val mandatoryFields: Set[Atom] = overseasAddress.configurableMandatoryAtoms
    val optionalFields: Set[Atom] = overseasAddress.configurableOptionalAtoms

    val maybeErrorShortName = context.formComponent.errorShortName
      .map(_.transform(_ + " ", identity))
      .flatMap(_.nonBlankValue())

    val errorShortNamePlaceholder = maybeErrorShortName.getOrElse(SmartString.blank.value())
    val errorShortNamePlaceholderLineReq = maybeErrorShortName
      .getOrElse(s"${messages("generic.error.overseas.validation.default")} ")

    def line1RequiredOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line1)
      ifProgram(
        cond = context.isAtomicValueBlank(atomicModelComponentId),
        thenProgram = errorProgram[Unit](
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line1.required",
            Some(errorShortNamePlaceholderLineReq :: Nil)
          )
        ),
        elseProgram = successProgram(())
      )
    }

    def line2RequiredOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line2)
      ifProgram(
        cond = context.isAtomicValueBlank(atomicModelComponentId),
        thenProgram = errorProgram[Unit](
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.line2.required",
            Some(errorShortNamePlaceholderLineReq :: Nil)
          )
        ),
        elseProgram = successProgram(())
      )
    }

    val cityAtomicModelComponentId = context.atomicFcId(OverseasAddress.city)
    def cityRequiredOp() =
      ifProgram(
        cond = context.isAtomicValueBlank(cityAtomicModelComponentId) || optionalFields(OverseasAddress.city),
        thenProgram = errorProgram[Unit](
          gformError(
            cityAtomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.town.city.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        ),
        elseProgram = successProgram(())
      )

    def postcodeRequiredOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.postcode)
      ifProgram(
        cond = context.isAtomicValueBlank(atomicModelComponentId) || !mandatoryFields(OverseasAddress.postcode),
        thenProgram = errorProgram[Unit](
          gformError(
            atomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.postcode.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        ),
        elseProgram = successProgram(())
      )
    }

    val countryAtomicModelComponentId = context.atomicFcId(OverseasAddress.country)
    val countryRequiredOp =
      ifProgram(
        cond = context.isAtomicValueBlank(countryAtomicModelComponentId),
        thenProgram = errorProgram[Unit](
          gformError(
            countryAtomicModelComponentId,
            context.formComponent,
            "generic.error.overseas.country.required",
            Some(errorShortNamePlaceholder :: Nil)
          )
        ),
        elseProgram = successProgram(())
      )

    def line1MaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line1)
      ifProgram(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.addressLine),
        thenProgram = errorProgram[Unit](
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
        ),
        elseProgram = successProgram(())
      )
    }

    def line2MaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.line2)
      ifProgram(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.addressLine),
        thenProgram = errorProgram[Unit](
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
        ),
        elseProgram = successProgram(())
      )
    }

    val line3AtomicModelComponentId = context.atomicFcId(OverseasAddress.line3)
    def line3MaxLengthOp() =
      ifProgram(
        cond = context.isAtomicValueExceedMaxLength(line3AtomicModelComponentId, ValidationValues.addressLine),
        thenProgram = errorProgram[Unit](
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
        ),
        elseProgram = successProgram(())
      )

    def cityMaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.city)
      ifProgram(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.overseasCity),
        thenProgram = errorProgram[Unit](
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
        ),
        elseProgram = successProgram(())
      )
    }

    val postcodeAtomicModelComponentId = context.atomicFcId(OverseasAddress.postcode)
    def postcodeMaxLengthOp() =
      ifProgram(
        cond = context.isAtomicValueExceedMaxLength(postcodeAtomicModelComponentId, ValidationValues.postcodeLimit),
        thenProgram = errorProgram[Unit](
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
        ),
        elseProgram = successProgram(())
      )

    def countryMaxLengthOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.country)
      ifProgram(
        cond = context.isAtomicValueExceedMaxLength(atomicModelComponentId, ValidationValues.countryLimit),
        thenProgram = errorProgram[Unit](
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
        ),
        elseProgram = successProgram(())
      )
    }

    def lookupCountryOp() = {
      val atomicModelComponentId = context.atomicFcId(OverseasAddress.country)
      val maybeValue = context.valueOf(atomicModelComponentId).filterNot(_.isEmpty).headOption
      val hasValue = maybeValue.isDefined
      ifProgram(
        cond = hasValue,
        thenProgram = TextChecker
          .lookupValidation(
            context.formComponent,
            context.lookupRegistry,
            Lookup(Register.Country, overseasAddress.selectionCriteria),
            LookupLabel(maybeValue.getOrElse("country")),
            context.formModelVisibilityOptics
          )
          .leftMap(error =>
            // discard the error of lookupValidation and replace it with a specific one
            if (error.isEmpty)
              error
            else
              gformError(
                atomicModelComponentId,
                context.formComponent,
                "generic.error.overseas.nomatch",
                Some(maybeValue.getOrElse("country") :: Nil)
              )
          ),
        elseProgram = successProgram(())
      )
    }

    List(
      List(line1RequiredOp(), line1MaxLengthOp()).shortCircuitProgram,
      ifProgram(
        andCond = mandatoryFields(OverseasAddress.line2),
        thenProgram = List(line2RequiredOp(), line2MaxLengthOp()).shortCircuitProgram,
        elseProgram = ifProgram(
          cond = context.nonBlankValueOf(line3AtomicModelComponentId).isDefined,
          thenProgram = line2MaxLengthOp(),
          elseProgram = successProgram(())
        )
      ),
      ifProgram(
        cond = context.nonBlankValueOf(line3AtomicModelComponentId).isDefined,
        thenProgram = line3MaxLengthOp(),
        elseProgram = successProgram(())
      ),
      ifProgram(
        andCond = optionalFields(OverseasAddress.city),
        thenProgram = ifProgram(
          cond = context.nonBlankValueOf(cityAtomicModelComponentId).isEmpty,
          thenProgram = cityMaxLengthOp(),
          elseProgram = successProgram(())
        ),
        elseProgram = List(cityRequiredOp(), cityMaxLengthOp()).shortCircuitProgram
      ),
      ifProgram(
        andCond = mandatoryFields(OverseasAddress.postcode),
        thenProgram = List(postcodeRequiredOp(), postcodeMaxLengthOp()).shortCircuitProgram,
        elseProgram = ifProgram(
          cond = context.nonBlankValueOf(postcodeAtomicModelComponentId).isDefined,
          thenProgram = postcodeMaxLengthOp(),
          elseProgram = successProgram(())
        )
      ),
      ifProgram(
        andCond = overseasAddress.countryLookup,
        thenProgram = List(countryRequiredOp, countryMaxLengthOp(), lookupCountryOp()).shortCircuitProgram,
        elseProgram = List(countryRequiredOp, countryMaxLengthOp()).shortCircuitProgram
      )
    ).nonShortCircuitProgram
  }

}
