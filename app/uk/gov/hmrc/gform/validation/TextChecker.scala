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

// /*
//  * Copyright 2023 HM Revenue & Customs
//  *
//  * Licensed under the Apache License, Version 2.0 (the "License");
//  * you may not use this file except in compliance with the License.
//  * You may obtain a copy of the License at
//  *
//  *     http://www.apache.org/licenses/LICENSE-2.0
//  *
//  * Unless required by applicable law or agreed to in writing, software
//  * distributed under the License is distributed on an "AS IS" BASIS,
//  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  * See the License for the specific language governing permissions and
//  * limitations under the License.
//  */

// package uk.gov.hmrc.gform.validation

// import cats.Monoid
// import cats.implicits._
// import play.api.i18n.Messages
// import uk.gov.hmrc.domain.Nino
// import uk.gov.hmrc.emailaddress.EmailAddress
// import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
// import uk.gov.hmrc.gform.lookup.LookupOptions.filterBySelectionCriteria
// import uk.gov.hmrc.gform.lookup._
// import uk.gov.hmrc.gform.models.optics.DataOrigin
// import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
// import uk.gov.hmrc.gform.sharedmodel.formtemplate._
// import uk.gov.hmrc.gform.sharedmodel.LangADT
// // import uk.gov.hmrc.gform.sharedmodel.LocalisedString
// import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
// import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }
// import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
// import uk.gov.hmrc.referencechecker.{ CorporationTaxReferenceChecker, VatReferenceChecker }

// import scala.util.matching.Regex
// import uk.gov.hmrc.gform.sharedmodel.SmartString

// import ComponentChecker._
// // import GformError._
// import uk.gov.hmrc.gform.validation.ValidationUtil.GformError
// import ComponentChecker._

// class TextChecker[D <: DataOrigin]() extends ComponentChecker[D] {

//   override protected def checkProgram(context: CheckerDependency[D])(implicit
//     langADT: LangADT,
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): CheckProgram[GformError] = {
//     val formComponent = context.formComponent
//     formComponent match {
//       case IsText(textType) => checkText(textType, context)
//       case _                => throw new IllegalArgumentException("FormComponent is not a Text")
//     }
//   }

//   private def checkText(formComponent: Text, context: CheckerDependency[D])(implicit
//     langADT: LangADT,
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): CheckProgram[GformError] = {
//     val value: Option[String] = context.formModelVisibilityOptics.data
//       .one(formComponent.modelComponentId)
//       .filterNot(_.isEmpty())
//     val formComponent = context.formComponent
//     val isMandatory = formComponent.mandatory
//     val mandatoryProgram = ???
//     val nonMandatoryProgram = ???

//     ifThenElseOp(
//       cond = isMandatory,
//       thenProgram = mandatoryProgram,
//       elseProgram = nonMandatoryProgram
//     )
//   }
// }

// object FooBar {
//   // format: off
//   val genericLongTextErrorPattern                            = "generic.longText.error.pattern"
//   val genericReferenceNumberErrorRequired                    = "generic.referenceNumber.error.required"
//   val genericReferenceNumberErrorPattern                     = "generic.referenceNumber.error.pattern"
//   val genericCrnErrorInvalid                                 = "generic.crn.error.invalid"
//   val genericEoriErrorPattern                                = "generic.eori.error.pattern"
//   val genericUkEoriErrorRequired                             = "generic.ukEori.error.required"
//   val genericUkEoriErrorPattern                              = "generic.ukEori.error.pattern"
//   val genericUkBankAccountErrorRequired                      = "generic.ukBankAccount.error.required"
//   val genericUkBankAccountErrorPattern                       = "generic.ukBankAccount.error.pattern"
//   val genericChildBenefitNumberErrorRequired                 = "generic.childBenefitNumber.error.required"
//   val genericChildBenefitNumberErrorPattern                  = "generic.childBenefitNumber.error.pattern"
//   val genericNonUKCountryCodeErrorPattern                    = "generic.nonUKCountryCode.error.pattern"
//   val genericCountryCodeErrorPattern                         = "generic.countryCode.error.pattern"
//   val genericTelephoneNumberErrorRequired                    = "generic.telephoneNumber.error.required"
//   val genericTelephoneNumberErrorPattern                     = "generic.telephoneNumber.error.pattern"
//   val genericShortTextErrorPattern                           = "generic.shortText.error.pattern"
//   val genericErrorLookup                                     = "generic.error.lookup"
//   val genericErrorRegistry                                   = "generic.error.registry"
//   val genericErrorRequired                                   = "generic.error.required"
//   val genericErrorParentSubmissionRefSameAsFormSubmissionRef = "generic.error.parentSubmissionRefSameAsFormSubmissionRef"
//   val genericErrorExactNumbers                               = "generic.error.exactNumbers"
//   val genericErrorSortCode                                   = "generic.error.sortCode"
//   val genericErrorSubmissionRef                              = "generic.error.submissionRef"
//   val genericErrorMaxWhole                                   = "generic.error.maxWhole"
//   val genericErrorPositiveNumber                             = "generic.error.positiveNumber"
//   val genericErrorMaxLengthNoDecimals                        = "generic.error.maxLength.noDecimals"
//   val genericErrorMaxLengthMaxDecimals                       = "generic.error.maxLength.maxDecimals"
//   val genericErrorWholeNumber                                = "generic.error.wholeNumber"
//   val genericErrorMaxDecimals                                = "generic.error.maxDecimals"
//   val genericErrorPositiveWholeNumber                        = "generic.error.positiveWholeNumber"
//   val genericErrorNumber                                     = "generic.error.number"
//   val genericErrorInvalid                                    = "generic.error.invalid"
//   val genericErrorMaxLength                                  = "generic.error.maxLength"
//   val genericErrorMinLength                                  = "generic.error.minLength"
//   val genericVrnErrorDigitCheck                              = "generic.vrn.error.digitcheck"
//   val genericGovernmentIdNotExist                            = "generic.governmentId.not.exist"
//   val genericGovernmentIdErrorPattern                        = "generic.governmentId.error.pattern"
//   val genericErrorEmail                                      = "generic.error.email"
//   val choiceErrorRequired                                    = "choice.error.required"
//   val timeErrorRequired                                      = "time.error.required"
//   val genericNinoErrorPattern                                = "generic.nino.error.pattern"
//   val genericNinoErrorRequired                               = "generic.nino.error.required"
//   val genericEmailErrorPattern                               = "generic.email.error.pattern"
//   val genericEmailErrorRequired                              = "generic.email.error.required"
//   val genericUtrErrorPattern                                 = "generic.utr.error.pattern"
//   val genericUtrErrorRequired                                = "generic.utr.error.required"
//   val genericUtrIdNotExist                                   = "generic.utr.not.exist"
//   val genericVrnErrorPattern                                 = "generic.vrn.error.pattern"
//   val genericVrnErrorRequired                                = "generic.vrn.error.required"
//   val genericPayeErrorPattern                                = "generic.paye.error.pattern"
//   val genericPayeErrorRequired                               = "generic.paye.error.required"

//   val genericNumberErrorRequired                             = "generic.number.error.required"
//   val genericNumberErrorPattern                              = "generic.number.error.pattern"
//   val genericNumberErrorMaxdecimalPattern                    = "generic.number.error.maxDecimal.pattern"
//   val genericNumberErrorMaxdigitPattern                      = "generic.number.error.maxDigit.pattern"
//   val genericNumberErrorWholePattern                         = "generic.number.error.whole.pattern"
//   val genericNumberErrorPositivePattern                      = "generic.number.error.positive.pattern"

//   val genericSterlingErrorRequired                           = "generic.sterling.error.required"
//   val genericSterlingErrorPattern                            = "generic.sterling.error.pattern"
//   val genericSterlingErrorPatternStart                       = "generic.sterling.error.pattern.start"
//   val genericSterlingErrorMaxdigitPattern                    = "generic.sterling.error.maxdigit.pattern"
//   val genericWholesterlingErrorPencePattern                  = "generic.whole.sterling.error.pence.pattern"
//   val genericPositiveSterlingErrorPositivePattern            = "generic.positive.sterling.error.positive.pattern"

//   val genericErrorTextRequired                               = "generic.error.text.required"
//   val genericErrorTextMaxLength                              = "generic.error.text.maxLength"
//   val genericErrorTextMinLength                              = "generic.error.text.minLength"
//   val genericErrorTextExactDigits                            = "generic.error.text.exactDigits"
//   val genericErrorTextValidChar                              = "generic.error.text.valid.char"
//   val genericErrorShortTextValidChar                         = "generic.error.shortText.valid.char"
//   // format: on

//   val ukSortCodeFormat = """^[^0-9]{0,2}\d{2}[^0-9]{0,2}\d{2}[^0-9]{0,2}\d{2}[^0-9]{0,2}$""".r

//   private def textData[D <: DataOrigin](
//     formModelVisibilityOptics: FormModelVisibilityOptics[D],
//     fieldValue: FormComponent
//   ): Option[String] =
//     formModelVisibilityOptics.data
//       .one(fieldValue.modelComponentId)
//       .filterNot(_.isEmpty())

//   // is used internally and OverseasAddressValidator
//   // TODO: GFORMS-2146: it might be refactored to be in some helper objects
//   def lookupValidation[D <: DataOrigin](
//     fieldValue: FormComponent,
//     lookupRegistry: LookupRegistry,
//     lookup: Lookup,
//     lookupLabel: LookupLabel,
//     formModelVisibilityOptics: FormModelVisibilityOptics[D]
//   )(implicit
//     messages: Messages,
//     l: LangADT,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val sSelectionCriteria: Option[List[SimplifiedSelectionCriteria]] = lookup.selectionCriteria map {
//       SimplifiedSelectionCriteria
//         .convertToSimplifiedSelectionCriteria(_, lookupRegistry, formModelVisibilityOptics)
//     }

//     val filteredLookuplabels =
//       (lookupRegistry.get(lookup.register), sSelectionCriteria) match {
//         case (Some(AjaxLookup(options, _, _)), Some(sc)) =>
//           val oLo = options.m.get(l).map(r => LookupOptions(filterBySelectionCriteria(sc, r.options)))
//           oLo
//             .map { s =>
//               if (s.options.nonEmpty)
//                 LocalisedLookupOptions(Map(l -> s)).process(_.keys.toList)
//               else
//                 Nil
//             }

//         case (Some(AjaxLookup(options, _, _)), None) =>
//           Some(options.process(_.keys.toList))

//         case _ =>
//           None
//       }

//     def lookupError: ValidatedType[Unit] = {
//       val vars: List[String] = lookupLabel.label :: Nil
//       validationFailure(fieldValue, genericErrorLookup, Some(vars))
//     }

//     def existsLabel(options: LookupOptions) =
//       if (filteredLookuplabels.isDefined && filteredLookuplabels.fold(false)(_.contains(lookupLabel)))
//         validationSuccess
//       else if (filteredLookuplabels.isEmpty && options.contains(lookupLabel))
//         validationSuccess
//       else
//         lookupError

//     lookupRegistry.get(lookup.register) match {
//       case Some(AjaxLookup(options, _, _)) => options.fold(lookupError)(existsLabel)
//       case Some(RadioLookup(options))      => options.fold(lookupError)(existsLabel)
//       case None =>
//         val vars: List[String] = lookup.register.toString :: Nil
//         validationFailure(fieldValue, genericErrorRegistry, Some(vars))
//     }
//   }

//   // GFORMS-2146: main validation method for all TextConstraint
//   //TODO: GFORMS-2146
//   def validateText[D <: DataOrigin](
//     fieldValue: FormComponent,
//     constraint: TextConstraint
//   )(
//     formModelVisibilityOptics: FormModelVisibilityOptics[D],
//     lookupRegistry: LookupRegistry
//   )(implicit
//     messages: Messages,
//     l: LangADT,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     (fieldValue.mandatory, textData(formModelVisibilityOptics, fieldValue), constraint) match {
//       case (true, None, TextWithRestrictions(_, _)) =>
//         validationFailure(
//           fieldValue,
//           genericErrorTextRequired,
//           (Some(errorShortNameWithFallback(fieldValue).pure[List]))
//         )
//       case (true, None, ShortText(_, _)) =>
//         validationFailure(
//           fieldValue,
//           genericErrorTextRequired,
//           (Some(errorShortNameWithFallback(fieldValue).pure[List]))
//         )
//       case (true, None, _) =>
//         fieldValue match {
//           case lookupRegistry.extractors.IsRadioLookup(_) => validationFailure(fieldValue, choiceErrorRequired, None)
//           case lookupRegistry.extractors.IsUkSortCode(_)  => validationFailure(fieldValue, genericErrorSortCode, None)
//           case IsText(Text(NINO, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericNinoErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//             )
//           case IsText(Text(Email, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericEmailErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
//             )
//           case IsText(Text(CtUTR, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericUtrErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//             )
//           case IsText(Text(UkVrn, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericVrnErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//             )
//           case IsText(Text(PayeReference, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericPayeErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//             )
//           case IsText(Text(UkEORI, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericUkEoriErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
//             )
//           case IsText(Text(UkBankAccountNumber, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericUkBankAccountErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(" " + _, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.value().pure[List]))
//             )
//           case IsText(Text(ChildBenefitNumber, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericChildBenefitNumberErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//             )
//           case IsText(Text(TelephoneNumber, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericTelephoneNumberErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.transform(identity, " " + _).value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//             )
//           case IsText(Text(_: ReferenceNumber, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericReferenceNumberErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]))
//             )
//           case IsText(Text(_: Number, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericNumberErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]))
//             )
//           case IsText(Text(_: PositiveNumber, _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericNumberErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]))
//             )
//           case IsText(Text(WholeSterling(true), _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericSterlingErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "an amount", _ => "swm").value().pure[List]))
//             )
//           case IsText(Text(Sterling(_, _), _, _, _, _, _)) =>
//             validationFailure(
//               fieldValue,
//               genericSterlingErrorRequired,
//               fieldValue.errorShortName
//                 .map(_.value().pure[List]) orElse
//                 (Some(SmartString.blank.transform(_ => "an amount", _ => "swm").value().pure[List]))
//             )
//           case _ => validationFailure(fieldValue, genericErrorRequired, None)

//         }
//       case (_, Some(value), lookup @ Lookup(_, _)) =>
//         lookupValidation(fieldValue, lookupRegistry, lookup, LookupLabel(value), formModelVisibilityOptics)
//       case (_, Some(value), ShortText(min, max)) => validateShortTextConstraint(fieldValue, value, min, max)
//       case (_, Some(value), TextWithRestrictions(min, max)) =>
//         validateTextConstraint(fieldValue, value, min, max)
//       case (_, Some(value), Sterling(_, isPositive)) =>
//         validateSterling(fieldValue, value, isPositive, false)
//       case (_, Some(value), WholeSterling(true)) =>
//         validateSterling(fieldValue, value, true, true)
//       case (_, Some(value), s: WholeSterling) =>
//         validateNumber(
//           fieldValue,
//           value,
//           ValidationValues.sterlingLength,
//           0,
//           s.positiveOnly
//         )
//       case (_, Some(value), ReferenceNumber(min, max)) => referenceNumberConstraints(fieldValue, value, min, max)
//       case (_, Some(value), UkBankAccountNumber)       => validateBankAccountFormat(fieldValue, value)
//       case (_, Some(value), UkSortCodeFormat)          => validateSortCodeFormat(fieldValue, value)
//       case (_, Some(value), SubmissionRefFormat)       => validateSubmissionRefFormat(fieldValue, value)
//       case (_, Some(value), SaUTR | CtUTR)             => checkUtr(fieldValue, value)
//       case (_, Some(value), NINO)                      => checkNino(fieldValue, value)
//       case (_, Some(value), UkVrn)                     => checkVrn(fieldValue, value)
//       case (_, Some(value), PayeReference)             => checkPayeReference(fieldValue, value)
//       case (_, Some(value), CompanyRegistrationNumber) => checkCompanyRegistrationNumber(fieldValue, value)
//       case (_, Some(value), EORI)                      => checkEORI(fieldValue, value)
//       case (_, Some(value), UkEORI)                    => checkUkEORI(fieldValue, value)
//       case (_, Some(value), ChildBenefitNumber)        => checkChildBenefitNumber(fieldValue, value)
//       case (_, Some(value), NonUkCountryCode)          => checkNonUkCountryCode(fieldValue, value)
//       case (_, Some(value), CountryCode)               => checkCountryCode(fieldValue, value)
//       case (_, Some(value), TelephoneNumber)           => validatePhoneNumber(fieldValue, value)
//       case (_, Some(value), Email | EmailVerifiedBy(_, _)) =>
//         Monoid.combine(
//           email(fieldValue, value),
//           textValidationWithConstraints(fieldValue, value, 0, ValidationValues.emailLimit)
//         )
//       case (_, Some(value), Number(maxWhole, maxFractional, _, _)) =>
//         validateNumeric(fieldValue, value, maxWhole, maxFractional, false)
//       case (_, Some(value), PositiveNumber(maxWhole, maxFractional, _, _)) =>
//         validateNumeric(fieldValue, value, maxWhole, maxFractional, true)
//       case (false, None, _) => validationSuccess
//     }

//   // GFORMS-2146: main method for validation submission
//   // Should probably be moved to a different file
//   def validateParentSubmissionRef[D <: DataOrigin](
//     fieldValue: FormComponent,
//     thisFormSubmissionRef: SubmissionRef
//   )(
//     formModelVisibilityOptics: FormModelVisibilityOptics[D]
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     (fieldValue.mandatory, textData(formModelVisibilityOptics, fieldValue)) match {
//       case (true, None) =>
//         validationFailure(fieldValue, genericErrorRequired, None)
//       case (_, Some(value)) =>
//         validateSubmissionRefFormat(fieldValue, value) andThen { _ =>
//           if (value === thisFormSubmissionRef.value)
//             validationFailure(fieldValue, genericErrorParentSubmissionRefSameAsFormSubmissionRef, None)
//           else validationSuccess
//         }
//       case _ => validationSuccess
//     }

//   private def validateBankAccountFormat(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val ukBankAccountFormat = s"[0-9]{${ValidationValues.bankAccountLength}}".r
//     val str = value.replace(" ", "")
//     str match {
//       case ukBankAccountFormat() => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericUkBankAccountErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(" " + _, " " + _).value().pure[List]) orElse
//             (Some(SmartString.blank.value().pure[List]))
//         )

//     }
//   }

//   private def validateSortCodeFormat(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     value match {
//       case ukSortCodeFormat() => validationSuccess
//       case _ =>
//         validationFailure(fieldValue, genericErrorSortCode, None)
//     }

//   private def validateSubmissionRefFormat(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val str = value.replace(" ", "")
//     if (SubmissionRef.verifyCheckChar(str)) validationSuccess
//     else validationFailure(fieldValue, genericErrorSubmissionRef, None)
//   }

//   private def validateNumber(
//     fieldValue: FormComponent,
//     value: String,
//     maxWhole: Int,
//     maxFractional: Int,
//     mustBePositive: Boolean
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
//     val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
//     (TextConstraint.filterNumberValue(value), maxFractional, mustBePositive) match {
//       case (WholeShape(_, whole, _), _, _) if surpassMaxLength(whole, maxWhole) =>
//         val vars: List[String] = maxWhole.toString :: Nil
//         validationFailure(fieldValue, genericErrorMaxWhole, Some(vars))
//       case (WholeShape("-", _, _), 0, true) =>
//         validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
//       case (WholeShape("-", _, _), _, true) =>
//         validationFailure(fieldValue, genericErrorPositiveNumber, None)
//       case (WholeShape(_, _, _), _, _) => validationSuccess
//       case (FractionalShape(_, _, _, _), 0, true) =>
//         validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
//       case (FractionalShape(_, whole, _, fractional), 0, _)
//           if surpassMaxLength(whole, maxWhole) && lessThanMinLength(fractional, 0) =>
//         val vars: List[String] = maxWhole.toString :: Nil
//         validationFailure(fieldValue, genericErrorMaxLengthNoDecimals, Some(vars))
//       case (FractionalShape(_, whole, _, fractional), _, _)
//           if surpassMaxLength(whole, maxWhole) && surpassMaxLength(fractional, maxFractional) =>
//         val vars: List[String] = maxWhole.toString :: maxFractional.toString :: Nil
//         validationFailure(fieldValue, genericErrorMaxLengthMaxDecimals, Some(vars))
//       case (FractionalShape(_, whole, _, _), _, _) if surpassMaxLength(whole, maxWhole) =>
//         val vars: List[String] = maxWhole.toString :: Nil
//         validationFailure(fieldValue, genericErrorMaxWhole, Some(vars))
//       case (FractionalShape(_, _, _, fractional), 0, _) if lessThanMinLength(fractional, 0) =>
//         validationFailure(fieldValue, genericErrorWholeNumber, None)
//       case (FractionalShape(_, _, _, fractional), _, _) if surpassMaxLength(fractional, maxFractional) =>
//         val vars: List[String] = maxFractional.toString :: Nil
//         validationFailure(fieldValue, genericErrorMaxDecimals, Some(vars))
//       case (FractionalShape("-", _, _, _), _, true) =>
//         validationFailure(fieldValue, genericErrorPositiveNumber, None)
//       case (FractionalShape(_, _, _, _), _, _) => validationSuccess
//       case (_, 0, true)                        => validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
//       case (_, _, true)                        => validationFailure(fieldValue, genericErrorPositiveNumber, None)
//       case (_, 0, false)                       => validationFailure(fieldValue, genericErrorWholeNumber, None)
//       case _                                   => validationFailure(fieldValue, genericErrorNumber, None)
//     }
//   }

//   private def validateNumeric(
//     fieldValue: FormComponent,
//     value: String,
//     maxWhole: Int,
//     maxFractional: Int,
//     mustBePositive: Boolean
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
//     val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
//     TextConstraint.filterNumberValue(value) match {
//       case WholeShape("-", _, _) if mustBePositive         => positiveNumberFailure(fieldValue, value)
//       case FractionalShape("-", _, _, _) if mustBePositive => positiveNumberFailure(fieldValue, value)
//       case FractionalShape(_, _, _, fractional) if maxFractional == 0 && mustBePositive =>
//         wholeNumberFailure(fieldValue, value)
//       case FractionalShape(_, _, _, fractional) if surpassMaxLength(fractional, maxFractional) =>
//         maxFractionFailure(fieldValue, value, maxFractional)
//       case FractionalShape(_, whole, _, _) if surpassMaxLength(whole, maxWhole) =>
//         maxDigitFailure(fieldValue, value, maxWhole)
//       case WholeShape(_, whole, _) if surpassMaxLength(whole, maxWhole) =>
//         maxDigitFailure(fieldValue, value, maxWhole)
//       case WholeShape(_, _, _)         => validationSuccess
//       case FractionalShape(_, _, _, _) => validationSuccess
//       case _                           => nonNumericFailure(fieldValue, value)
//     }
//   }

//   private def nonNumericFailure(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     fieldValue.errorShortNameStart match {
//       case None =>
//         validationFailure(
//           fieldValue,
//           genericNumberErrorRequired,
//           Some(List(SmartString.blank.transform(_ => "a number", _ => "rif").value(), ""))
//         )
//       case Some(errorShortNameStart) =>
//         validationFailure(
//           fieldValue,
//           genericNumberErrorPattern,
//           Some(List(errorShortNameStart.value(), fieldValue.errorExampleWithCommaOrBlank.value()))
//         )
//     }

//   private def maxFractionFailure(
//     fieldValue: FormComponent,
//     value: String,
//     maxFractional: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericNumberErrorMaxdecimalPattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value(),
//           maxFractional.toString
//         )
//       )
//     )

//   private def maxDigitFailure(
//     fieldValue: FormComponent,
//     value: String,
//     maxWhole: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericNumberErrorMaxdigitPattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value(),
//           maxWhole.toString
//         )
//       )
//     )

//   private def wholeNumberFailure(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericNumberErrorWholePattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value()
//         )
//       )
//     )

//   private def positiveNumberFailure(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericNumberErrorPositivePattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value()
//         )
//       )
//     )

//   private def validateSterling(
//     fieldValue: FormComponent,
//     value: String,
//     isPositive: Boolean,
//     isWhole: Boolean
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val maxWhole = ValidationValues.sterlingLength
//     val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
//     val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
//     TextConstraint.filterNumberValue(value) match {
//       case FractionalShape(_, _, _, fractional) if !isWhole && fractional.length > 2 =>
//         nonNumericSterlingFailure(fieldValue, value)
//       case FractionalShape(_, _, _, fractional) if isWhole =>
//         wholeSterlingFailure(fieldValue, value)
//       case WholeShape(_, whole, _) if surpassMaxLength(whole, maxWhole) =>
//         maxDigitSterlingFailure(fieldValue, value, maxWhole)
//       case WholeShape("-", _, _) if isPositive         => positiveSterlingFailure(fieldValue, value)
//       case FractionalShape("-", _, _, _) if isPositive => positiveSterlingFailure(fieldValue, value)
//       case WholeShape(_, _, _)                         => validationSuccess
//       case FractionalShape(_, _, _, _) if !isWhole     => validationSuccess
//       case _                                           => nonNumericSterlingFailure(fieldValue, value)
//     }
//   }

//   private def nonNumericSterlingFailure(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     (fieldValue.errorShortName, fieldValue.errorShortNameStart) match {
//       case (None, None) =>
//         validationFailure(
//           fieldValue,
//           genericSterlingErrorPattern,
//           Some(
//             List(
//               SmartString.blank.transform(_ => "an amount", _ => "swm").value(),
//               fieldValue.errorExampleWithCommaOrBlank.value()
//             )
//           )
//         )

//       case (Some(errorShortName), None) =>
//         validationFailure(
//           fieldValue,
//           genericSterlingErrorPattern,
//           Some(List(errorShortName.value(), fieldValue.errorExampleWithCommaOrBlank.value()))
//         )
//       case (_, Some(errorShortNameStart)) =>
//         validationFailure(
//           fieldValue,
//           genericSterlingErrorPatternStart,
//           Some(List(errorShortNameStart.value(), fieldValue.errorExampleWithCommaOrBlank.value()))
//         )
//     }

//   private def maxDigitSterlingFailure(
//     fieldValue: FormComponent,
//     value: String,
//     maxWhole: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericSterlingErrorMaxdigitPattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Amount", _ => "swm").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value(),
//           maxWhole.toString
//         )
//       )
//     )

//   private def wholeSterlingFailure(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericWholesterlingErrorPencePattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Amount", _ => "swm").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value()
//         )
//       )
//     )

//   private def positiveSterlingFailure(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] =
//     validationFailure(
//       fieldValue,
//       genericPositiveSterlingErrorPositivePattern,
//       Some(
//         List(
//           fieldValue.errorShortNameStart match {
//             case None                      => SmartString.blank.transform(_ => "Amount", _ => "swm").value()
//             case Some(errorShortNameStart) => errorShortNameStart.value()
//           },
//           fieldValue.errorExampleWithCommaOrBlank.value()
//         )
//       )
//     )
//   private[validation] def textValidationWithConstraints(
//     fieldValue: FormComponent,
//     value: String,
//     min: Int,
//     max: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     invalidCharactersValidator(fieldValue, value, validTextPattern, genericLongTextErrorPattern)
//       .andThen { _ =>
//         sharedTextComponentValidator(fieldValue, value, min, max, validTextPattern, genericLongTextErrorPattern)
//       }

//   private lazy val validTextPattern: Regex = {
//     val validChars = Set('(', ')', ',', '’', '“', '”', '%', '•', '-', '.', 'r', 's', '£', '+', ';', ':', '*', '?', '=',
//       '/', '&', '!', '@', '#', '$', '€', '`', '~', '"', '<', '>', '_', '§', '±', '[', ']', '{', '}', '–', '—', '‘', '’',
//       '“', '”', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Ā', 'Ă', 'Ą', 'Ǻ', 'Æ', 'Ǽ', 'Ç', 'Ć', 'Ĉ', 'Ċ', 'Č', 'Þ', 'Ď', 'Đ', 'È',
//       'É', 'Ê', 'Ë', 'Ē', 'Ĕ', 'Ė', 'Ę', 'Ě', 'Ĝ', 'Ğ', 'Ġ', 'Ģ', 'Ĥ', 'Ħ', 'Ì', 'Í', 'Î', 'Ï', 'Ĩ', 'Ī', 'Ĭ', 'Į', 'İ',
//       'Ĵ', 'Ķ', 'Ĺ', 'Ļ', 'Ľ', 'Ł', 'Ñ', 'Ń', 'Ņ', 'Ň', 'Ŋ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ō', 'Ŏ', 'Ő', 'Ǿ', 'Œ', 'Ŕ',
//       'Ŗ', 'Ř', 'Ś', 'Ŝ', 'Ş', 'Š', 'Ţ', 'Ť', 'Ŧ', 'Ù', 'Ú', 'Û', 'Ü', 'Ũ', 'Ū', 'Ŭ', 'Ů', 'Ű', 'Ų', 'Ŵ', 'Ẁ', 'Ẃ', 'Ẅ',
//       'Ỳ', 'Ý', 'Ŷ', 'Ÿ', 'Ź', 'Ż', 'Ž', '·', 'à', 'á', 'â', 'ã', 'ä', 'å', 'ā', 'ă', 'ą', 'ǻ', 'æ', 'ǽ', 'ç', 'ć', 'ĉ',
//       'ċ', 'č', 'þ', 'ď', 'đ', 'è', 'é', 'ê', 'ë', 'ē', 'ĕ', 'ė', 'ę', 'ě', 'ĝ', 'ğ', 'ġ', 'ģ', 'ĥ', 'ħ', 'ì', 'í', 'î',
//       'ï', 'ĩ', 'ī', 'ĭ', 'į', 'ĵ', 'ķ', 'ĺ', 'ļ', 'ľ', 'ł', 'ñ', 'ń', 'ņ', 'ň', 'ŋ', 'ò', 'ó', 'ô', 'õ', 'ö', 'ø', 'ō',
//       'ŏ', 'ő', 'ǿ', 'œ', 'ŕ', 'ŗ', 'ř', 'ś', 'ŝ', 'ş', 'š', 't', 'ţ', 'ť', 'ŧ', 'ù', 'ú', 'û', 'ü', 'ũ', 'ū', 'ŭ', 'ů',
//       'ű', 'ų', 'ŵ', 'ẁ', 'ẃ', 'ẅ', 'ỳ', 'ý', 'ŷ', 'ÿ', 'ź', 'ż', 'ž', '«', '»', '¥', '…')
//     validChars.mkString("[A-Za-z0-9\\\\n\\̇\\'\\", "\\", "]+").r
//   }

//   private def referenceNumberConstraints(fieldValue: FormComponent, value: String, min: Int, max: Int)(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidReferenceNumber = s"[0-9]{$min,$max}".r
//     val str = value.replace(" ", "")
//     str match {
//       case ValidReferenceNumber() => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericReferenceNumberErrorPattern,
//           fieldValue.errorShortName
//             .map(_.value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]))
//         )

//     }
//   }

//   private def email(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     if (EmailAddress.isValid(value)) validationSuccess
//     else
//       validationFailure(
//         fieldValue,
//         genericEmailErrorPattern,
//         fieldValue.errorShortName
//           .map(_.transform(identity, _ + " ").value().pure[List]) orElse
//           (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
//       )

//   private def checkVrn(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val Standard = "(GB|XI)?([0-9]{9})".r
//     val Branch = "GB[0-9]{12}".r
//     val Government = "GBGD[0-4][0-9]{2}".r
//     val Health = "GBHA[5-9][0-9]{2}".r
//     val str = value.replace(" ", "")
//     str match {
//       case Standard(_, s) if VatReferenceChecker.isValid(s) => validationSuccess
//       case Standard(_, s)                                   => validationFailure(fieldValue, genericVrnErrorDigitCheck, None)
//       case Branch()                                         => validationSuccess
//       case Government()                                     => validationSuccess
//       case Health()                                         => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericVrnErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, _ + " ").value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//         )
//     }
//   }

//   private def checkCompanyRegistrationNumber(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidCRN = "[A-Z]{2}[0-9]{6}|[0-9]{8}".r
//     val str = value.replace(" ", "")
//     sharedTextComponentValidator(fieldValue, str, 8, 8, ValidCRN, genericCrnErrorInvalid)
//   }

//   private def checkEORI(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidEORI = "^[A-Z]{2}[0-9A-Z]{7,15}$".r
//     val str = value.replace(" ", "")
//     sharedTextComponentValidator(fieldValue, str, 9, 17, ValidEORI, genericEoriErrorPattern)
//   }

//   private def checkUkEORI(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidUkEORI = "^GB[0-9]{12}$".r
//     val ValidUkEORINumbers = "^[0-9]{14}$".r
//     val str = value.replace(" ", "")

//     str match {
//       case ValidUkEORI()        => validationSuccess
//       case ValidUkEORINumbers() => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericUkEoriErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, _ + " ").value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
//         )

//     }
//   }
//   private def checkChildBenefitNumber(fieldValue: FormComponent, value: String)(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidChildBenefitNumber = "^CHB[0-9]{8}[A-Z]{2}$".r
//     val str = value.replace(" ", "")

//     str match {
//       case ValidChildBenefitNumber() => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericChildBenefitNumberErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, " " + _).value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//         )

//     }
//   }

//   private def checkNonUkCountryCode(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidCountryCode = "[A-Z]+".r
//     if (value == "UK") validationFailure(fieldValue, genericNonUKCountryCodeErrorPattern, None)
//     else sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, genericNonUKCountryCodeErrorPattern)
//   }

//   private def checkCountryCode(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidCountryCode = "[A-Z]+".r
//     sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, genericCountryCodeErrorPattern)
//   }

//   private def checkUtr(fieldValue: FormComponent, value: String)(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val UTRFormat = "[0-9]{10}".r

//     value match {
//       case UTRFormat() if CorporationTaxReferenceChecker.isValid(value) =>
//         validationSuccess
//       case UTRFormat() if !CorporationTaxReferenceChecker.isValid(value) =>
//         validationFailure(fieldValue, genericUtrIdNotExist, None)
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericUtrErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, _ + " ").value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//         )
//     }
//   }

//   private def checkNino(fieldValue: FormComponent, value: String)(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     value match {
//       case x if Nino.isValid(x) => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericNinoErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, _ + " ").value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//         )
//     }

//   private def checkPayeReference(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val ValidPaye = "^[0-9]{3}/[0-9A-Z]{1,10}$".r
//     val str = value.replace(" ", "")
//     str match {
//       case ValidPaye() => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericPayeErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, _ + " ").value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//         )

//     }
//   }

//   private def validatePhoneNumber(
//     fieldValue: FormComponent,
//     value: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ): ValidatedType[Unit] = {
//     val str = value.replace(" ", "")
//     str match {
//       case TelephoneNumber.phoneNumberValidation() => validationSuccess
//       case _ =>
//         validationFailure(
//           fieldValue,
//           genericTelephoneNumberErrorPattern,
//           fieldValue.errorShortName
//             .map(_.transform(identity, " " + _).value().pure[List]) orElse
//             (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
//         )

//     }
//   }

//   private[validation] def textLengthValidation(
//     fieldValue: FormComponent,
//     value: String,
//     min: Int,
//     max: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     value match {
//       case exact if max == min && exact.length != max =>
//         val vars: List[String] = errorShortNameStartWithFallback(fieldValue) :: max.toString :: Nil
//         validationFailure(fieldValue, genericErrorTextExactDigits, Some(vars))
//       case tooLong if tooLong.length > max =>
//         val vars: List[String] = errorShortNameStartWithFallback(fieldValue) :: max.toString :: Nil
//         validationFailure(fieldValue, genericErrorTextMaxLength, Some(vars))
//       case tooShort if tooShort.length < min =>
//         val vars: List[String] = errorShortNameStartWithFallback(fieldValue) :: min.toString :: Nil
//         validationFailure(fieldValue, genericErrorTextMinLength, Some(vars))
//       case _ => validationSuccess
//     }

//   private[validation] def validateShortTextConstraint(
//     fieldValue: FormComponent,
//     value: String,
//     min: Int,
//     max: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     textLengthValidation(fieldValue, value, min, max)
//       .andThen { _ =>
//         val ValidShortText = """[A-Za-z0-9\'\-\.\&\s]+""".r
//         value match {
//           case ValidShortText() => validationSuccess
//           case _ =>
//             validationFailure(
//               fieldValue,
//               genericErrorShortTextValidChar,
//               Some(List(errorShortNameStartWithFallback(fieldValue)))
//             )
//         }
//       }

//   private[validation] def validateTextConstraint(
//     fieldValue: FormComponent,
//     value: String,
//     min: Int,
//     max: Int
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     textLengthValidation(fieldValue, value, min, max)
//       .andThen { _ =>
//         invalidCharactersValidator(
//           fieldValue,
//           value,
//           validTextPattern,
//           genericErrorTextValidChar,
//           List(errorShortNameStartWithFallback(fieldValue))
//         )
//       }

//   private def errorShortNameStartWithFallback(fieldValue: FormComponent)(implicit
//     sse: SmartStringEvaluator
//   ): String =
//     fieldValue.errorShortNameStart.flatMap(_.nonBlankValue()) orElse
//       fieldValue.shortName.flatMap(_.nonBlankValue()) getOrElse
//       fieldValue.label.value()

//   private def errorShortNameWithFallback(fieldValue: FormComponent)(implicit
//     sse: SmartStringEvaluator
//   ): String =
//     fieldValue.errorShortName.flatMap(_.nonBlankValue()) orElse
//       fieldValue.shortName.flatMap(_.nonBlankValue()) getOrElse
//       fieldValue.label.value()

//   private def surpassMaxLength(wholeOrFractional: String, maxLength: Int): Boolean =
//     filterCommas(wholeOrFractional).length > maxLength

//   private def lessThanMinLength(wholeOrFractional: String, minLength: Int): Boolean =
//     wholeOrFractional.length > minLength

//   private def filterCommas(number: String) = number.filterNot(c => c == ',')

//   private def sharedTextComponentValidator(
//     fieldValue: FormComponent,
//     value: String,
//     minChars: Int,
//     maxChars: Int,
//     regex: Regex,
//     messageKey: String
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) =
//     value match {
//       case tooLong if tooLong.length > maxChars =>
//         val vars: List[String] = maxChars.toString :: Nil
//         validationFailure(fieldValue, genericErrorMaxLength, Some(vars))
//       case tooShort if tooShort.length < minChars =>
//         val vars: List[String] = minChars.toString :: Nil
//         val errorMinLength = fieldValue match {
//           case _ => genericErrorMinLength
//         }
//         validationFailure(fieldValue, errorMinLength, Some(vars))
//       case regex() => validationSuccess
//       case _       => validationFailure(fieldValue, messageKey, None)
//     }

//   private def invalidCharactersValidator(
//     fieldValue: FormComponent,
//     value: String,
//     regex: Regex,
//     messageKey: String,
//     messageArgs: List[String] = List()
//   )(implicit
//     messages: Messages,
//     sse: SmartStringEvaluator
//   ) = {
//     val vars = value
//       .foldLeft(Set[Char]()) { (acc, c) =>
//         c match {
//           case regex() => acc
//           case _       => acc + c
//         }
//       }
//       .map(_.toString)
//       .mkString(" ")

//     if (vars.isEmpty)
//       validationSuccess
//     else
//       validationFailure(fieldValue, messageKey, Some(List(vars) ++ messageArgs))
//   }
// }
