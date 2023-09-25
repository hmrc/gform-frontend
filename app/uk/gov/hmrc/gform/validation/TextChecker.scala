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

import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluationSyntax
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.lookup.LookupOptions.filterBySelectionCriteria
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.SmartString
import uk.gov.hmrc.gform.sharedmodel.SubmissionRef
import uk.gov.hmrc.gform.sharedmodel.form.EnvelopeId
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.CheckerServiceHelper.validationFailure
import uk.gov.hmrc.referencechecker.CorporationTaxReferenceChecker
import uk.gov.hmrc.referencechecker.VatReferenceChecker

import scala.util.matching.Regex

import ComponentChecker._

class TextChecker[D <: DataOrigin]() extends ComponentChecker[Unit, D] {

  override protected def checkProgram(context: CheckerDependency[D])(implicit
    langADT: LangADT,
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val formComponent = context.formComponent
    formComponent match {
      case IsText(text) =>
        TextChecker.validateText(
          context.formComponent,
          text.constraint,
          context.cache.formTemplate,
          context.cache.envelopeId
        )(
          context.formModelVisibilityOptics,
          context.lookupRegistry
        )
      case IsTextArea(text) =>
        TextChecker.validateText(
          context.formComponent,
          text.constraint,
          context.cache.formTemplate,
          context.cache.envelopeId
        )(
          context.formModelVisibilityOptics,
          context.lookupRegistry
        )
      case _ => throw new IllegalArgumentException("FormComponent is not a Text")
    }
  }

}

object TextChecker {
  // format: off
  val genericLongTextErrorPattern                            = "generic.longText.error.pattern"
  val genericReferenceNumberErrorRequired                    = "generic.referenceNumber.error.required"
  val genericReferenceNumberErrorPattern                     = "generic.referenceNumber.error.pattern"
  val genericCrnErrorInvalid                                 = "generic.crn.error.invalid"
  val genericEoriErrorRequired                               = "generic.eori.error.required"
  val genericEoriErrorPattern                                = "generic.eori.error.pattern"
  val genericCrnErrorRequired                                = "generic.companyRegistrationNumber.error.required"
  val genericCrnErrorPattern                                 = "generic.companyRegistrationNumber.error.pattern"
  val genericUkEoriErrorRequired                             = "generic.ukEori.error.required"
  val genericUkEoriErrorPattern                              = "generic.ukEori.error.pattern"
  val genericUkBankAccountErrorRequired                      = "generic.ukBankAccount.error.required"
  val genericUkBankAccountErrorPattern                       = "generic.ukBankAccount.error.pattern"
  val genericChildBenefitNumberErrorRequired                 = "generic.childBenefitNumber.error.required"
  val genericChildBenefitNumberErrorPattern                  = "generic.childBenefitNumber.error.pattern"
  val genericNonUKCountryCodeErrorPattern                    = "generic.nonUKCountryCode.error.pattern"
  val genericCountryCodeErrorPattern                         = "generic.countryCode.error.pattern"
  val genericTelephoneNumberErrorRequired                    = "generic.telephoneNumber.error.required"
  val genericTelephoneNumberErrorPattern                     = "generic.telephoneNumber.error.pattern"
  val genericShortTextErrorPattern                           = "generic.shortText.error.pattern"
  val genericErrorLookup                                     = "generic.error.lookup"
  val genericErrorRegistry                                   = "generic.error.registry"
  val genericErrorRequired                                   = "generic.error.required"
  val genericErrorParentSubmissionRefSameAsFormSubmissionRef = "generic.error.parentSubmissionRefSameAsFormSubmissionRef"
  val genericErrorExactNumbers                               = "generic.error.exactNumbers"
  val genericErrorSortCode                                   = "generic.error.sortCode"
  val genericErrorSubmissionRef                              = "generic.error.submissionRef"
  val genericErrorMaxWhole                                   = "generic.error.maxWhole"
  val genericErrorPositiveNumber                             = "generic.error.positiveNumber"
  val genericErrorMaxLengthNoDecimals                        = "generic.error.maxLength.noDecimals"
  val genericErrorMaxLengthMaxDecimals                       = "generic.error.maxLength.maxDecimals"
  val genericErrorWholeNumber                                = "generic.error.wholeNumber"
  val genericErrorMaxDecimals                                = "generic.error.maxDecimals"
  val genericErrorPositiveWholeNumber                        = "generic.error.positiveWholeNumber"
  val genericErrorNumber                                     = "generic.error.number"
  val genericErrorInvalid                                    = "generic.error.invalid"
  val genericErrorMaxLength                                  = "generic.error.maxLength"
  val genericErrorMinLength                                  = "generic.error.minLength"
  val genericVrnErrorDigitCheck                              = "generic.vrn.error.digitcheck"
  val genericGovernmentIdNotExist                            = "generic.governmentId.not.exist"
  val genericGovernmentIdErrorPattern                        = "generic.governmentId.error.pattern"
  val genericErrorEmail                                      = "generic.error.email"
  val choiceErrorRequired                                    = "choice.error.required"
  val timeErrorRequired                                      = "time.error.required"
  val genericNinoErrorPattern                                = "generic.nino.error.pattern"
  val genericNinoErrorRequired                               = "generic.nino.error.required"
  val genericEmailErrorPattern                               = "generic.email.error.pattern"
  val genericEmailErrorRequired                              = "generic.email.error.required"
  val genericUtrErrorPattern                                 = "generic.utr.error.pattern"
  val genericUtrErrorRequired                                = "generic.utr.error.required"
  val genericUtrIdNotExist                                   = "generic.utr.not.exist"
  val genericVrnErrorPattern                                 = "generic.vrn.error.pattern"
  val genericVrnErrorRequired                                = "generic.vrn.error.required"
  val genericPayeErrorPattern                                = "generic.paye.error.pattern"
  val genericPayeErrorRequired                               = "generic.paye.error.required"

  val genericNumberErrorRequired                             = "generic.number.error.required"
  val genericNumberErrorPattern                              = "generic.number.error.pattern"
  val genericNumberErrorMaxdecimalPattern                    = "generic.number.error.maxDecimal.pattern"
  val genericNumberErrorMaxdigitPattern                      = "generic.number.error.maxDigit.pattern"
  val genericNumberErrorWholePattern                         = "generic.number.error.whole.pattern"
  val genericNumberErrorPositivePattern                      = "generic.number.error.positive.pattern"

  val genericSterlingErrorRequired                           = "generic.sterling.error.required"
  val genericSterlingErrorPattern                            = "generic.sterling.error.pattern"
  val genericSterlingErrorPatternStart                       = "generic.sterling.error.pattern.start"
  val genericSterlingErrorMaxdigitPattern                    = "generic.sterling.error.maxdigit.pattern"
  val genericWholesterlingErrorPencePattern                  = "generic.whole.sterling.error.pence.pattern"
  val genericPositiveSterlingErrorPositivePattern            = "generic.positive.sterling.error.positive.pattern"

  val genericErrorTextRequired                               = "generic.error.text.required"
  val genericErrorTextMaxLength                              = "generic.error.text.maxLength"
  val genericErrorTextMinLength                              = "generic.error.text.minLength"
  val genericErrorTextExactDigits                            = "generic.error.text.exactDigits"
  val genericErrorTextValidChar                              = "generic.error.text.valid.char"
  val genericErrorShortTextValidChar                         = "generic.error.shortText.valid.char"
  val genericErrorYearPattern                                = "generic.error.yearformat.real"
  val genericErrorYearRequired                               = "generic.error.yearformat.required"
  // format: on

  val ukSortCodeFormat = """^[^0-9]{0,2}\d{2}[^0-9]{0,2}\d{2}[^0-9]{0,2}\d{2}[^0-9]{0,2}$""".r

  private def textData[D <: DataOrigin](
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    fieldValue: FormComponent
  ): Option[String] =
    formModelVisibilityOptics.data
      .one(fieldValue.modelComponentId)
      .filterNot(_.isEmpty())

  def lookupValidation[D <: DataOrigin](
    fieldValue: FormComponent,
    lookupRegistry: LookupRegistry,
    lookup: Lookup,
    lookupLabel: LookupLabel,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val sSelectionCriteria: Option[List[SimplifiedSelectionCriteria]] = lookup.selectionCriteria map {
      SimplifiedSelectionCriteria
        .convertToSimplifiedSelectionCriteria(_, lookupRegistry, formModelVisibilityOptics)
    }

    val filteredLookuplabels =
      (lookupRegistry.get(lookup.register), sSelectionCriteria) match {
        case (Some(AjaxLookup(options, _, _)), Some(sc)) =>
          val oLo = options.m.get(l).map(r => LookupOptions(filterBySelectionCriteria(sc, r.options)))
          oLo
            .map { s =>
              if (s.options.nonEmpty)
                LocalisedLookupOptions(Map(l -> s)).process(_.keys.toList)
              else
                Nil
            }

        case (Some(AjaxLookup(options, _, _)), None) =>
          Some(options.process(_.keys.toList))

        case _ =>
          None
      }

    def lookupError: CheckProgram[Unit] = {
      val vars: List[String] = lookupLabel.label :: Nil
      validationFailure(fieldValue, genericErrorLookup, Some(vars))
    }

    def existsLabel(options: LookupOptions) =
      switchProgram(
        switchCase(
          cond = filteredLookuplabels.isDefined && filteredLookuplabels.fold(false)(_.contains(lookupLabel)),
          thenProgram = successProgram(())
        ),
        switchCase(
          cond = filteredLookuplabels.isEmpty && options.contains(lookupLabel),
          thenProgram = successProgram(())
        )
      )(
        elseProgram = lookupError
      )

    lookupRegistry.get(lookup.register) match {
      case Some(AjaxLookup(options, _, _)) => options.fold(lookupError)(existsLabel)
      case Some(RadioLookup(options))      => options.fold(lookupError)(existsLabel)
      case None =>
        val vars: List[String] = lookup.register.toString :: Nil
        validationFailure(fieldValue, genericErrorRegistry, Some(vars))
    }
  }

  def validateText[D <: DataOrigin](
    fieldValue: FormComponent,
    constraint: TextConstraint,
    formTemplate: FormTemplate,
    envelopeId: EnvelopeId
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    lookupRegistry: LookupRegistry
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val isMandatory = fieldValue.mandatory
    val inputText: String = textData(formModelVisibilityOptics, fieldValue).getOrElse("")
    val isInputTextEmpty = inputText.isEmpty

    def numberCheck(c: Number): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericNumberErrorRequired,
        fieldValue.errorShortName.map(_.value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]
        ))
      ),
      nonEmptyCheck = validateNumeric(fieldValue, inputText, c.maxWholeDigits, c.maxFractionalDigits, false)
    )

    def positiveNumberCheck(c: PositiveNumber): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericNumberErrorRequired,
        fieldValue.errorShortName.map(_.value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]
        ))
      ),
      nonEmptyCheck = validateNumeric(fieldValue, inputText, c.maxWholeDigits, c.maxFractionalDigits, true)
    )
    def shortTextCheck(c: ShortText): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericErrorTextRequired,
        (Some(errorShortNameWithFallback(fieldValue).pure[List]))
      ),
      nonEmptyCheck = validateShortTextConstraint(fieldValue, inputText, c.min, c.max),
      nonEmptyCheckIfMandatory = Some(
        validateShortTextConstraint(fieldValue, inputText, c.min, c.max, true)
      )
    )
    def lookupCheck(c: Lookup): CheckProgram[Unit] =
      conditionalMandatoryCheck(
        mandatoryFailure = validationFailure(
          fieldValue,
          genericErrorTextRequired,
          (Some(errorShortNameWithFallback(fieldValue).pure[List]))
        ),
        nonEmptyCheck = lookupValidation(
          fieldValue,
          lookupRegistry,
          c,
          LookupLabel(inputText),
          formModelVisibilityOptics
        ),
        nonEmptyCheckIfMandatory = Some(
          lookupValidation(fieldValue, lookupRegistry, c, LookupLabel(inputText), formModelVisibilityOptics)
        )
      )
    def textWithRestrictionsCheck(c: TextWithRestrictions): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericErrorTextRequired,
        (Some(errorShortNameWithFallback(fieldValue).pure[List]))
      ),
      nonEmptyCheck = validateTextConstraint(fieldValue, inputText, c.min, c.max, None),
      nonEmptyCheckIfMandatory = Some(
        validateTextConstraint(fieldValue, inputText, c.min, c.max, None)
      )
    )
    def sterlingCheck(c: Sterling): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericSterlingErrorRequired,
        fieldValue.errorShortName.map(_.value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "an amount", _ => "swm").value().pure[List]
        ))
      ),
      nonEmptyCheck = validateSterling(fieldValue, inputText, c.positiveOnly, false)
    )

    def wholeSterlingCheck(c: WholeSterling): CheckProgram[Unit] =
      validateNumber(fieldValue, inputText, ValidationValues.sterlingLength, 0, c.positiveOnly)

    def positiveWholeSterlingCheck(c: WholeSterling): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericSterlingErrorRequired,
        fieldValue.errorShortName.map(_.value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "an amount", _ => "swm").value().pure[List]
        ))
      ),
      nonEmptyCheck = validateSterling(fieldValue, inputText, true, true)
    )
    def referenceNumberCheck(c: ReferenceNumber): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericReferenceNumberErrorRequired,
        fieldValue.errorShortName.map(_.value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]
        ))
      ),
      nonEmptyCheck = referenceNumberConstraints(fieldValue, inputText, c.min, c.max)
    )

    def ukBankAccountNumberCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericUkBankAccountErrorRequired,
        fieldValue.errorShortName.map(_.transform(" " + _, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.value().pure[List]
        ))
      ),
      nonEmptyCheck = validateBankAccountFormat(fieldValue, inputText)
    )

    def ukSortCodeFormatCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(fieldValue, genericErrorSortCode, None),
      nonEmptyCheck = validateSortCodeFormat(fieldValue, inputText)
    )

    def submissionRefFormatCheck(): CheckProgram[Unit] =
      ifProgram(
        cond = formTemplate.parentFormSubmissionRefs.contains(fieldValue.id),
        thenProgram = validateParentSubmissionRef(fieldValue, SubmissionRef(envelopeId))(formModelVisibilityOptics),
        elseProgram = validateSubmissionRefFormat(fieldValue, inputText)
      )

    def telephoneNumberCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericTelephoneNumberErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = validatePhoneNumber(fieldValue, inputText)
    )

    def emailCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericEmailErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "an", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = List(
        validateTextConstraint(
          fieldValue,
          inputText,
          0,
          ValidationValues.emailLimit,
          Some(emailErrorFirstPlaceholder()),
          true
        ),
        email(fieldValue, inputText)
      ).shortCircuitProgram
    )
    def emailVerifiedByCheck(c: EmailVerifiedBy): CheckProgram[Unit] = List(
      validateTextConstraint(
        fieldValue,
        inputText,
        0,
        ValidationValues.emailLimit,
        Some(emailErrorFirstPlaceholder())
      ),
      email(fieldValue, inputText)
    ).shortCircuitProgram

    def emailErrorFirstPlaceholder(): String = fieldValue.errorShortNameStart
      .flatMap(_.nonBlankValue())
      .map(s => SmartString.blank.transform(_ => s + " email", _ => "gyfeiriad e-bost " + s).value())
      .getOrElse(
        SmartString.blank.transform(_ => "Email", _ => "gyfeiriad e-bost").value()
      )

    def utrCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericUtrErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkUtr(fieldValue, inputText)
    )

    def ninoCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericNinoErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkNino(fieldValue, inputText)
    )

    def payeReferenceCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericPayeErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkPayeReference(fieldValue, inputText)
    )

    def ukVrnCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericVrnErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkVrn(fieldValue, inputText)
    )

    def countryCodeCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = checkCountryCode(fieldValue, inputText),
      nonEmptyCheck = checkCountryCode(fieldValue, inputText)
    )
    def nonUkCountryCodeCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = checkNonUkCountryCode(fieldValue, inputText),
      nonEmptyCheck = checkNonUkCountryCode(fieldValue, inputText)
    )
    def companyRegistrationNumberCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericCrnErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkCompanyRegistrationNumber(fieldValue, inputText)
    )

    def eoriCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericEoriErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "an", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkEORI(fieldValue, inputText)
    )

    def ukEoriCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericUkEoriErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "an", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkUkEORI(fieldValue, inputText)
    )

    def childBenefitNumberCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericChildBenefitNumberErrorRequired,
        fieldValue.errorShortName.map(_.transform(identity, " " + _).value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", identity).value().pure[List]
        ))
      ),
      nonEmptyCheck = checkChildBenefitNumber(fieldValue, inputText)
    )

    def radioLookupCheck(): CheckProgram[Unit] = validationFailure(fieldValue, choiceErrorRequired, None)

    def yearFormatCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(
        fieldValue,
        genericErrorYearRequired,
        fieldValue.errorShortName.map(_.value().pure[List]) orElse (Some(
          SmartString.blank.transform(_ => "a", _ => "").value().pure[List]
        ))
      ),
      nonEmptyCheck = validateYear(fieldValue, inputText)
    )
    def validateYear(fieldValue: FormComponent, yearStr: String): CheckProgram[Unit] = ifProgram(
      cond = yearStr.toIntOption.map(y => y >= 1900 && y <= 2099).getOrElse(false),
      thenProgram = successProgram(()),
      elseProgram = {
        val placeholder =
          fieldValue.errorShortName.map(_.transform(identity, w => s" $w ").value().pure[List]) orElse (Some(
            SmartString.blank.transform(_ => "a", _ => "").value().pure[List]
          ))
        validationFailure(fieldValue, genericErrorYearPattern, placeholder)
      }
    )
    def catchAllCheck(): CheckProgram[Unit] = conditionalMandatoryCheck(
      mandatoryFailure = validationFailure(fieldValue, genericErrorRequired, None),
      nonEmptyCheck = successProgram(())
    )

    def conditionalMandatoryCheck(
      mandatoryFailure: => CheckProgram[Unit],
      nonEmptyCheck: => CheckProgram[Unit],
      nonEmptyCheckIfMandatory: => Option[CheckProgram[Unit]] = None
    ): CheckProgram[Unit] =
      ifProgram(
        andCond = isMandatory,
        thenProgram = ifProgram(
          cond = isInputTextEmpty,
          thenProgram = mandatoryFailure,
          elseProgram = nonEmptyCheckIfMandatory.getOrElse(nonEmptyCheck)
        ),
        elseProgram = ifProgram(
          cond = isInputTextEmpty,
          thenProgram = successProgram(()),
          elseProgram = nonEmptyCheck
        )
      )

    constraint match {
      case c: Number                                  => numberCheck(c)
      case c: PositiveNumber                          => positiveNumberCheck(c)
      case c: ShortText                               => shortTextCheck(c)
      case c: TextWithRestrictions                    => textWithRestrictionsCheck(c)
      case c: Sterling                                => sterlingCheck(c)
      case c @ WholeSterling(true)                    => positiveWholeSterlingCheck(c)
      case c: WholeSterling                           => wholeSterlingCheck(c)
      case c: ReferenceNumber                         => referenceNumberCheck(c)
      case UkBankAccountNumber                        => ukBankAccountNumberCheck()
      case UkSortCodeFormat                           => ukSortCodeFormatCheck()
      case SubmissionRefFormat                        => submissionRefFormatCheck()
      case TelephoneNumber                            => telephoneNumberCheck()
      case Email                                      => emailCheck()
      case c: EmailVerifiedBy                         => emailVerifiedByCheck(c)
      case SaUTR | CtUTR                              => utrCheck()
      case NINO                                       => ninoCheck()
      case PayeReference                              => payeReferenceCheck()
      case UkVrn                                      => ukVrnCheck()
      case CountryCode                                => countryCodeCheck()
      case NonUkCountryCode                           => nonUkCountryCodeCheck()
      case CompanyRegistrationNumber                  => companyRegistrationNumberCheck()
      case EORI                                       => eoriCheck() //done until this line
      case UkEORI                                     => ukEoriCheck()
      case ChildBenefitNumber                         => childBenefitNumberCheck()
      case lookupRegistry.extractors.IsRadioLookup(_) => radioLookupCheck()
      case c: Lookup                                  => lookupCheck(c)
      case YearFormat                                 => yearFormatCheck()
      case _                                          => catchAllCheck()
    }
  }

  private def validateBankAccountFormat(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val UkBankAccountFormat = s"[0-9]{${ValidationValues.bankAccountLength}}".r
    val transformedValue = value.replace(" ", "")
    val isUKBankAccount = transformedValue match {
      case UkBankAccountFormat() => true
      case _                     => false
    }

    ifProgram(
      cond = isUKBankAccount,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(
        fieldValue,
        genericUkBankAccountErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(" " + _, " " + _).value().pure[List]) orElse
          (Some(SmartString.blank.value().pure[List]))
      )
    )
  }

  private def validateSortCodeFormat(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {

    val isSortCodeValid = value match {
      case ukSortCodeFormat() => true
      case _                  => false
    }
    ifProgram(
      cond = isSortCodeValid,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(fieldValue, genericErrorSortCode, None)
    )
  }

  private def validateSubmissionRefFormat(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val str = value.replace(" ", "")
    ifProgram(
      cond = SubmissionRef.verifyCheckChar(str),
      thenProgram = successProgram(()),
      elseProgram = validationFailure(fieldValue, genericErrorSubmissionRef, None)
    )
  }

  def validateParentSubmissionRef[D <: DataOrigin](
    fieldValue: FormComponent,
    thisFormSubmissionRef: SubmissionRef
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val isMandatory = fieldValue.mandatory
    val value = textData(formModelVisibilityOptics, fieldValue)
    switchProgram(
      switchCase(
        cond = isMandatory && value.isEmpty,
        thenProgram = validationFailure(fieldValue, genericErrorRequired, None)
      ),
      switchCase(
        cond = value.nonEmpty,
        thenProgram = List(
          validateSubmissionRefFormat(fieldValue, value.getOrElse("")),
          ifProgram(
            cond = value.get === thisFormSubmissionRef.value,
            thenProgram = validationFailure(fieldValue, genericErrorParentSubmissionRefSameAsFormSubmissionRef, None),
            elseProgram = successProgram(())
          )
        ).shortCircuitProgram
      )
    )(
      elseProgram = successProgram(())
    )
  }

  private def validateNumber(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int,
    maxFractional: Int,
    mustBePositive: Boolean
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val filteredValue = TextConstraint.filterNumberValue(value)
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r

    val (signWS, wholeWS, _) = filteredValue match {
      case WholeShape(sign, whole, _) => (Some(sign), Some(whole), Some(""))
      case _                          => (None, None, None)
    }

    val (signFS, wholeFS, _, fractionFS) = filteredValue match {
      case FractionalShape(sign, whole, _, fraction) => (Some(sign), Some(whole), Some(""), Some(fraction))
      case _                                         => (None, None, None, None)
    }

    val isNegativeWhole = signWS.contains("-")
    val isNegativeFraction = signFS.contains("-")
    val hasFraction = fractionFS.isDefined
    val exceedsMaxFractional = fractionFS.exists(fractional => surpassMaxLength(fractional, maxFractional))
    val exceedsWholeFractional = wholeFS.exists(whole => surpassMaxLength(whole, maxWhole))
    val exceedsWholeWhole = wholeWS.exists(whole => surpassMaxLength(whole, maxWhole))
    val requiresWholeNumber = maxFractional == 0
    val isFractionBelowMin = fractionFS.exists(fractional => lessThanMinLength(fractional, 0))

    switchProgram(
      switchCase(
        cond = exceedsWholeWhole,
        thenProgram = validationFailure(fieldValue, genericErrorMaxWhole, Some(List(maxWhole.toString)))
      ),
      switchCase(
        cond = isNegativeWhole,
        andCond = requiresWholeNumber,
        thenProgram = validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
      ),
      switchCase(
        cond = isNegativeWhole,
        andCond = mustBePositive,
        thenProgram = validationFailure(fieldValue, genericErrorPositiveNumber, None)
      ),
      switchCase(
        cond = isNegativeFraction,
        andCond = requiresWholeNumber,
        thenProgram = validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
      ),
      switchCase(
        cond = exceedsWholeFractional && isFractionBelowMin,
        andCond = requiresWholeNumber,
        thenProgram = validationFailure(fieldValue, genericErrorMaxLengthNoDecimals, Some(List(maxWhole.toString)))
      ),
      switchCase(
        cond = exceedsWholeFractional && exceedsMaxFractional,
        thenProgram = validationFailure(
          fieldValue,
          genericErrorMaxLengthMaxDecimals,
          Some(List(maxWhole.toString, maxFractional.toString))
        )
      ),
      switchCase(
        cond = exceedsWholeFractional,
        thenProgram = validationFailure(fieldValue, genericErrorMaxWhole, Some(List(maxWhole.toString)))
      ),
      switchCase(
        cond = hasFraction && isFractionBelowMin,
        andCond = requiresWholeNumber,
        thenProgram = validationFailure(fieldValue, genericErrorWholeNumber, None)
      ),
      switchCase(
        cond = exceedsMaxFractional,
        thenProgram = validationFailure(fieldValue, genericErrorMaxDecimals, Some(List(maxFractional.toString)))
      ),
      switchCase(
        cond = isNegativeFraction,
        andCond = mustBePositive,
        thenProgram = validationFailure(fieldValue, genericErrorPositiveNumber, None)
      ),
      switchCase(
        cond = wholeWS.isDefined,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = fractionFS.isDefined,
        thenProgram = successProgram(())
      )
    )(
      elseProgram = validationFailure(fieldValue, genericErrorNumber, None)
    )
  }

  private def validateNumeric(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int,
    maxFractional: Int,
    mustBePositive: Boolean
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val filteredValue = TextConstraint.filterNumberValue(value)
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r

    val (signWS, wholeWS, _) = filteredValue match {
      case WholeShape(sign, whole, _) => (Some(sign), Some(whole), Some(""))
      case _                          => (None, None, None)
    }

    val (signFS, wholeFS, _, fractionFS) = filteredValue match {
      case FractionalShape(sign, whole, _, fraction) => (Some(sign), Some(whole), Some(""), Some(fraction))
      case _                                         => (None, None, None, None)
    }

    val isNegativeWhole = signWS.contains("-")
    val isNegativeFraction = signFS.contains("-")
    val hasFraction = fractionFS.isDefined
    val exceedsMaxFractional = fractionFS.exists(fractional => surpassMaxLength(fractional, maxFractional))
    val exceedsWholeFractional = wholeFS.exists(whole => surpassMaxLength(whole, maxWhole))
    val exceedsWholeWhole = wholeWS.exists(whole => surpassMaxLength(whole, maxWhole))
    val requiresWholeNumber = maxFractional == 0

    switchProgram(
      switchCase(
        cond = isNegativeWhole && mustBePositive,
        thenProgram = positiveNumberFailure(fieldValue, value)
      ),
      switchCase(
        cond = isNegativeFraction,
        andCond = mustBePositive,
        thenProgram = positiveNumberFailure(fieldValue, value)
      ),
      switchCase(
        cond = hasFraction,
        andCond = requiresWholeNumber,
        thenProgram = wholeNumberFailure(fieldValue, value)
      ),
      switchCase(
        cond = exceedsMaxFractional,
        thenProgram = maxFractionFailure(fieldValue, value, maxFractional)
      ),
      switchCase(
        cond = exceedsWholeFractional,
        thenProgram = maxDigitFailure(fieldValue, value, maxWhole)
      ),
      switchCase(
        cond = exceedsWholeWhole,
        thenProgram = maxDigitFailure(fieldValue, value, maxWhole)
      ),
      switchCase(
        cond = wholeWS.isDefined,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = fractionFS.isDefined,
        thenProgram = successProgram(())
      )
    )(
      elseProgram = nonNumericFailure(fieldValue, value)
    )
  }

  private def nonNumericFailure(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    fieldValue.errorShortNameStart match {
      case None =>
        validationFailure(
          fieldValue,
          genericNumberErrorRequired,
          Some(List(SmartString.blank.transform(_ => "a number", _ => "rif").value(), ""))
        )
      case Some(errorShortNameStart) =>
        validationFailure(
          fieldValue,
          genericNumberErrorPattern,
          Some(List(errorShortNameStart.value(), fieldValue.errorExampleWithCommaOrBlank.value()))
        )
    }

  private def maxFractionFailure(
    fieldValue: FormComponent,
    value: String,
    maxFractional: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericNumberErrorMaxdecimalPattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value(),
          maxFractional.toString
        )
      )
    )

  private def maxDigitFailure(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericNumberErrorMaxdigitPattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value(),
          maxWhole.toString
        )
      )
    )

  private def wholeNumberFailure(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericNumberErrorWholePattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value()
        )
      )
    )

  private def positiveNumberFailure(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericNumberErrorPositivePattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Number", _ => "rhif").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value()
        )
      )
    )

  private def validateSterling(
    fieldValue: FormComponent,
    value: String,
    isPositive: Boolean,
    isWhole: Boolean
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val filteredValue = TextConstraint.filterNumberValue(value)
    val maxWhole = ValidationValues.sterlingLength
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r

    val (signWS, wholeWS, _) = filteredValue match {
      case WholeShape(sign, whole, _) => (Some(sign), Some(whole), Some(""))
      case _                          => (None, None, None)
    }

    val (signFS, wholeFS, _, fractionFS) = filteredValue match {
      case FractionalShape(sign, whole, _, fraction) => (Some(sign), Some(whole), Some(""), Some(fraction))
      case _                                         => (None, None, None, None)
    }

    val isNegativeWhole = signWS.contains("-")
    val isNegativeFraction = signFS.contains("-")
    val hasFractionMoreThanTwoDigits = fractionFS.exists(_.length > 2)
    val isFractionPresent = fractionFS.isDefined
    val exceedsWholeLimit = wholeWS.exists(whole => surpassMaxLength(whole, maxWhole))
    val exceedsWholeLimitFS = wholeFS.exists(whole => surpassMaxLength(whole, maxWhole))

    switchProgram(
      switchCase(
        cond = hasFractionMoreThanTwoDigits,
        andCond = !isWhole,
        thenProgram = nonNumericSterlingFailure(fieldValue, value)
      ),
      switchCase(
        cond = isFractionPresent,
        andCond = isWhole,
        thenProgram = wholeSterlingFailure(fieldValue, value)
      ),
      switchCase(
        cond = exceedsWholeLimit,
        thenProgram = maxDigitSterlingFailure(fieldValue, value, maxWhole)
      ),
      switchCase(
        cond = exceedsWholeLimitFS,
        thenProgram = maxDigitSterlingFailure(fieldValue, value, maxWhole)
      ),
      switchCase(
        cond = isNegativeWhole,
        andCond = isPositive,
        thenProgram = positiveSterlingFailure(fieldValue, value)
      ),
      switchCase(
        cond = isNegativeFraction,
        andCond = isPositive,
        thenProgram = positiveSterlingFailure(fieldValue, value)
      ),
      switchCase(
        cond = wholeWS.isDefined,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = fractionFS.isDefined,
        andCond = !isWhole,
        thenProgram = successProgram(())
      )
    )(
      elseProgram = nonNumericSterlingFailure(fieldValue, value)
    )
  }

  private def nonNumericSterlingFailure(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    (fieldValue.errorShortName, fieldValue.errorShortNameStart) match {
      case (None, None) =>
        validationFailure(
          fieldValue,
          genericSterlingErrorPattern,
          Some(
            List(
              SmartString.blank.transform(_ => "an amount", _ => "swm").value(),
              fieldValue.errorExampleWithCommaOrBlank.value()
            )
          )
        )

      case (Some(errorShortName), None) =>
        validationFailure(
          fieldValue,
          genericSterlingErrorPattern,
          Some(List(errorShortName.value(), fieldValue.errorExampleWithCommaOrBlank.value()))
        )
      case (_, Some(errorShortNameStart)) =>
        validationFailure(
          fieldValue,
          genericSterlingErrorPatternStart,
          Some(List(errorShortNameStart.value(), fieldValue.errorExampleWithCommaOrBlank.value()))
        )
    }

  private def maxDigitSterlingFailure(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericSterlingErrorMaxdigitPattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Amount", _ => "swm").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value(),
          maxWhole.toString
        )
      )
    )

  private def wholeSterlingFailure(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericWholesterlingErrorPencePattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Amount", _ => "swm").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value()
        )
      )
    )

  private def positiveSterlingFailure(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] =
    validationFailure(
      fieldValue,
      genericPositiveSterlingErrorPositivePattern,
      Some(
        List(
          fieldValue.errorShortNameStart match {
            case None                      => SmartString.blank.transform(_ => "Amount", _ => "swm").value()
            case Some(errorShortNameStart) => errorShortNameStart.value()
          },
          fieldValue.errorExampleWithCommaOrBlank.value()
        )
      )
    )

  private[validation] def textValidationWithConstraints(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {

    val invalidCharactersCheck =
      invalidCharactersValidator(fieldValue, value, validTextPattern, genericLongTextErrorPattern)

    List(
      invalidCharactersCheck,
      sharedTextComponentValidator(fieldValue, value, min, max, validTextPattern, genericLongTextErrorPattern)
    ).shortCircuitProgram
  }

  private lazy val validTextPattern: Regex = {
    val validChars = Set('(', ')', ',', '’', '“', '”', '%', '•', '-', '.', 'r', 's', '£', '+', ';', ':', '*', '?', '=',
      '/', '&', '!', '@', '#', '$', '€', '`', '~', '"', '<', '>', '_', '§', '±', '[', ']', '{', '}', '–', '—', '‘', '’',
      '“', '”', 'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Ā', 'Ă', 'Ą', 'Ǻ', 'Æ', 'Ǽ', 'Ç', 'Ć', 'Ĉ', 'Ċ', 'Č', 'Þ', 'Ď', 'Đ', 'È',
      'É', 'Ê', 'Ë', 'Ē', 'Ĕ', 'Ė', 'Ę', 'Ě', 'Ĝ', 'Ğ', 'Ġ', 'Ģ', 'Ĥ', 'Ħ', 'Ì', 'Í', 'Î', 'Ï', 'Ĩ', 'Ī', 'Ĭ', 'Į', 'İ',
      'Ĵ', 'Ķ', 'Ĺ', 'Ļ', 'Ľ', 'Ł', 'Ñ', 'Ń', 'Ņ', 'Ň', 'Ŋ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', 'Ø', 'Ō', 'Ŏ', 'Ő', 'Ǿ', 'Œ', 'Ŕ',
      'Ŗ', 'Ř', 'Ś', 'Ŝ', 'Ş', 'Š', 'Ţ', 'Ť', 'Ŧ', 'Ù', 'Ú', 'Û', 'Ü', 'Ũ', 'Ū', 'Ŭ', 'Ů', 'Ű', 'Ų', 'Ŵ', 'Ẁ', 'Ẃ', 'Ẅ',
      'Ỳ', 'Ý', 'Ŷ', 'Ÿ', 'Ź', 'Ż', 'Ž', '·', 'à', 'á', 'â', 'ã', 'ä', 'å', 'ā', 'ă', 'ą', 'ǻ', 'æ', 'ǽ', 'ç', 'ć', 'ĉ',
      'ċ', 'č', 'þ', 'ď', 'đ', 'è', 'é', 'ê', 'ë', 'ē', 'ĕ', 'ė', 'ę', 'ě', 'ĝ', 'ğ', 'ġ', 'ģ', 'ĥ', 'ħ', 'ì', 'í', 'î',
      'ï', 'ĩ', 'ī', 'ĭ', 'į', 'ĵ', 'ķ', 'ĺ', 'ļ', 'ľ', 'ł', 'ñ', 'ń', 'ņ', 'ň', 'ŋ', 'ò', 'ó', 'ô', 'õ', 'ö', 'ø', 'ō',
      'ŏ', 'ő', 'ǿ', 'œ', 'ŕ', 'ŗ', 'ř', 'ś', 'ŝ', 'ş', 'š', 't', 'ţ', 'ť', 'ŧ', 'ù', 'ú', 'û', 'ü', 'ũ', 'ū', 'ŭ', 'ů',
      'ű', 'ų', 'ŵ', 'ẁ', 'ẃ', 'ẅ', 'ỳ', 'ý', 'ŷ', 'ÿ', 'ź', 'ż', 'ž', '«', '»', '¥', '…')
    validChars.mkString("[A-Za-z0-9\\\\n\\̇\\'\\", "\\", "]+").r
  }

  private def referenceNumberConstraints(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val filteredValue = value.replace(" ", "")
    val ValidReferenceNumber = s"[0-9]{$min,$max}".r

    val isReferenceNumber = filteredValue match {
      case ValidReferenceNumber() => true
      case _                      => false
    }

    val defaultError = fieldValue.errorShortName
      .map(_.value().pure[List])
      .orElse(Some(SmartString.blank.transform(_ => "a number", _ => "rif").value().pure[List]))

    ifProgram(
      cond = isReferenceNumber,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(fieldValue, genericReferenceNumberErrorPattern, defaultError)
    )
  }

  private def email(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) =
    ifProgram(
      cond = EmailAddress.isValid(value),
      thenProgram = successProgram(()),
      elseProgram = validationFailure(
        fieldValue,
        genericEmailErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(identity, _ + " ").value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
      )
    )

  private def checkVrn(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val Standard = "(GB|XI)?([0-9]{9})".r
    val Branch = "GB[0-9]{12}".r
    val Government = "GBGD[0-4][0-9]{2}".r
    val Health = "GBHA[5-9][0-9]{2}".r
    val str = value.replace(" ", "")

    val (prefixStd, numberStd) = str match {
      case Standard(prefix, number) => (Some(prefix), Some(number))
      case _                        => (None, None)
    }

    val isBranch = str match {
      case Branch() => true
      case _        => false
    }

    val isGovernment = str match {
      case Government() => true
      case _            => false
    }

    val isHealth = str match {
      case Health() => true
      case _        => false
    }

    val isVatReferenceValid = numberStd.exists(s => VatReferenceChecker.isValid(s))

    switchProgram(
      switchCase(
        cond = isBranch || isGovernment || isHealth,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = prefixStd.isDefined && isVatReferenceValid,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = prefixStd.isDefined,
        thenProgram = validationFailure(fieldValue, genericVrnErrorDigitCheck, None)
      )
    )(
      elseProgram = validationFailure(
        fieldValue,
        genericVrnErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(identity, _ + " ").value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
      )
    )
  }

  private def checkCompanyRegistrationNumber(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val str = value.replace(" ", "")
    val ValidCRN = "[A-Z]{2}[0-9]{6}|[0-9]{8}".r

    val isCRN = str match {
      case ValidCRN() => true
      case _          => false
    }

    ifProgram(
      cond = isCRN,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(
        fieldValue,
        genericCrnErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(identity, _ + " ").value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
      )
    )
  }

  private def checkEORI(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val str = value.replace(" ", "")
    val ValidEORI = "^[A-Z]{2}[0-9A-Z]{7,15}$".r

    val isEORI = str match {
      case ValidEORI() => true
      case _           => false
    }

    ifProgram(
      cond = isEORI,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(
        fieldValue,
        genericEoriErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(identity, _ + " ").value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
      )
    )
  }

  private def checkUkEORI(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val str = value.replace(" ", "")
    val ValidUkEORI = "^GB[0-9]{12}$".r
    val ValidUkEORINumbers = "^[0-9]{14}$".r

    val isUkEORI = str match {
      case ValidUkEORI() => true
      case _             => false
    }

    val isUkEORINumbers = str match {
      case ValidUkEORINumbers() => true
      case _                    => false
    }

    ifProgram(
      cond = isUkEORI || isUkEORINumbers,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(
        fieldValue,
        genericUkEoriErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(identity, _ + " ").value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "an", identity).value().pure[List]))
      )
    )
  }

  private def checkChildBenefitNumber(fieldValue: FormComponent, value: String)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidChildBenefitNumber = "^CHB[0-9]{8}[A-Z]{2}$".r
    val str = value.replace(" ", "")
    val isValidChildBenefitNumber = str match {
      case ValidChildBenefitNumber() => true
      case _                         => false
    }

    ifProgram(
      cond = isValidChildBenefitNumber,
      thenProgram = successProgram(()),
      elseProgram = {
        val errorShortName = fieldValue.errorShortName
          .map(_.transform(identity, " " + _).value().pure[List])
          .orElse(Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
        validationFailure(fieldValue, genericChildBenefitNumberErrorPattern, errorShortName)
      }
    )
  }

  private def checkNonUkCountryCode(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidCountryCode = "[A-Z]+".r
    ifProgram(
      cond = value == "UK",
      thenProgram = validationFailure(fieldValue, genericNonUKCountryCodeErrorPattern, None),
      elseProgram =
        sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, genericNonUKCountryCodeErrorPattern)
    )
  }

  private def checkCountryCode(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidCountryCode = "[A-Z]+".r
    sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, genericCountryCodeErrorPattern)
  }

  private def checkUtr(fieldValue: FormComponent, value: String)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val UTRFormat = "[0-9]{10}".r
    val isUtrFormat = UTRFormat.matches(value)
    val isValidUtr = CorporationTaxReferenceChecker.isValid(value)
    val errorMessage = fieldValue.errorShortName
      .map(_.transform(identity, _ + " ").value().pure[List])
      .getOrElse(SmartString.blank.transform(_ => "a", identity).value().pure[List])

    switchProgram(
      switchCase(
        cond = isUtrFormat && isValidUtr,
        thenProgram = successProgram(())
      ),
      switchCase(
        cond = isUtrFormat && !isValidUtr,
        thenProgram = validationFailure(fieldValue, genericUtrIdNotExist, None)
      )
    )(
      elseProgram = validationFailure(fieldValue, genericUtrErrorPattern, Some(errorMessage))
    )
  }

  private def checkNino(fieldValue: FormComponent, value: String)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val isNinoValid = Nino.isValid(value)

    ifProgram(
      cond = isNinoValid,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(
        fieldValue,
        genericNinoErrorPattern,
        fieldValue.errorShortName
          .map(_.transform(identity, _ + " ").value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))
      )
    )
  }

  private def checkPayeReference(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidPaye = "^[0-9]{3}/[0-9A-Z]{1,10}$".r
    val str = value.replace(" ", "")

    val isValidPaye = str match {
      case ValidPaye() => true
      case _           => false
    }

    ifProgram(
      cond = isValidPaye,
      thenProgram = successProgram(()),
      elseProgram = {
        val defaultErrorName = Some(SmartString.blank.transform(_ => "a", identity).value().pure[List])
        val errorName =
          fieldValue.errorShortName.map(_.transform(identity, _ + " ").value().pure[List]) orElse defaultErrorName
        validationFailure(fieldValue, genericPayeErrorPattern, errorName)
      }
    )
  }

  def validatePhoneNumber(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val str = value.replace(" ", "")
    val isValidPhoneNumber = str match {
      case TelephoneNumber.phoneNumberValidation() => true
      case _                                       => false
    }
    ifProgram(
      cond = !isValidPhoneNumber,
      thenProgram = {
        val errorShortName = fieldValue.errorShortName
          .map(_.transform(identity, " " + _).value().pure[List]) orElse
          (Some(SmartString.blank.transform(_ => "a", identity).value().pure[List]))

        validationFailure(fieldValue, genericTelephoneNumberErrorPattern, errorShortName)
      },
      elseProgram = successProgram(())
    )

  }

  def textLengthValidation(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int,
    placeholder: Option[String],
    noTooShortValidation: Boolean = false
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): CheckProgram[Unit] = {
    val textLength = value.length

    val isExact = max == min
    val notMax = textLength != max
    val isTooLong = textLength > max
    val isTooShort = textLength < min

    val firstPlaceholder = placeholder.getOrElse(errorShortNameStartWithFallback(fieldValue))
    val varsExact: List[String] = firstPlaceholder :: max.toString :: Nil
    val varsTooLong: List[String] = firstPlaceholder :: max.toString :: Nil
    val varsTooShort: List[String] = firstPlaceholder :: min.toString :: Nil

    switchProgram(
      switchCase(
        cond = notMax,
        andCond = isExact,
        thenProgram = validationFailure(fieldValue, genericErrorTextExactDigits, Some(varsExact))
      ),
      switchCase(
        cond = isTooLong,
        thenProgram = validationFailure(fieldValue, genericErrorTextMaxLength, Some(varsTooLong))
      ),
      switchCase(
        cond = isTooShort,
        andCond = !noTooShortValidation || min >= 2,
        thenProgram = validationFailure(fieldValue, genericErrorTextMinLength, Some(varsTooShort))
      )
    )(
      elseProgram = successProgram(())
    )
  }

  def validateShortTextConstraint(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int,
    noTooShortValidation: Boolean = false
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val lengthValidationResult = textLengthValidation(fieldValue, value, min, max, None, noTooShortValidation)
    val ValidShortText = """[A-Za-z0-9\'\-\.\&\s]+""".r
    val isValidShortText = value match {
      case ValidShortText() => true
      case _                => false
    }
    List(
      lengthValidationResult,
      ifProgram(
        cond = isValidShortText,
        thenProgram = successProgram(()),
        elseProgram = validationFailure(
          fieldValue,
          genericErrorShortTextValidChar,
          Some(List(errorShortNameStartWithFallback(fieldValue)))
        )
      )
    ).shortCircuitProgram
  }

  private[validation] def validateTextConstraint(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int,
    placeholder: Option[String],
    noTooShortValidation: Boolean = false
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val lengthValidationResult = textLengthValidation(fieldValue, value, min, max, placeholder, noTooShortValidation)
    List(
      lengthValidationResult,
      invalidCharactersValidator(
        fieldValue,
        value,
        validTextPattern,
        genericErrorTextValidChar,
        List(placeholder.getOrElse(errorShortNameStartWithFallback(fieldValue)))
      )
    ).shortCircuitProgram
  }

  private def errorShortNameStartWithFallback(fieldValue: FormComponent)(implicit
    sse: SmartStringEvaluator
  ): String =
    fieldValue.errorShortNameStart.flatMap(_.nonBlankValue()) orElse
      fieldValue.shortName.flatMap(_.nonBlankValue()) getOrElse
      fieldValue.label.value()

  private def errorShortNameWithFallback(fieldValue: FormComponent)(implicit
    sse: SmartStringEvaluator
  ): String =
    fieldValue.errorShortName.flatMap(_.nonBlankValue()) orElse
      fieldValue.shortName.flatMap(_.nonBlankValue()) getOrElse
      fieldValue.label.value()

  private def surpassMaxLength(wholeOrFractional: String, maxLength: Int): Boolean =
    filterCommas(wholeOrFractional).length > maxLength

  private def lessThanMinLength(wholeOrFractional: String, minLength: Int): Boolean =
    wholeOrFractional.length > minLength

  private def filterCommas(number: String) = number.filterNot(c => c == ',')

  private def sharedTextComponentValidator(
    fieldValue: FormComponent,
    value: String,
    minChars: Int,
    maxChars: Int,
    regex: Regex,
    messageKey: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val valueLength = value.length
    val exceedsMaxChars = valueLength > maxChars
    val tooShort = valueLength < minChars
    val matchesRegex = regex.matches(value)

    switchProgram(
      switchCase(
        cond = exceedsMaxChars,
        thenProgram = {
          val vars: List[String] = maxChars.toString :: Nil
          validationFailure(fieldValue, genericErrorMaxLength, Some(vars))
        }
      ),
      switchCase(
        cond = tooShort,
        thenProgram = {
          val vars: List[String] = minChars.toString :: Nil
          val errorMinLength = genericErrorMinLength
          validationFailure(fieldValue, errorMinLength, Some(vars))
        }
      ),
      switchCase(
        cond = matchesRegex,
        thenProgram = successProgram(())
      )
    )(
      elseProgram = validationFailure(fieldValue, messageKey, None)
    )
  }

  private def invalidCharactersValidator(
    fieldValue: FormComponent,
    value: String,
    regex: Regex,
    messageKey: String,
    messageArgs: List[String] = List()
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val vars = value
      .foldLeft(Set[Char]()) { (acc, c) =>
        c match {
          case regex() => acc
          case _       => acc + c
        }
      }
      .map(_.toString)
      .mkString(" ")

    ifProgram(
      cond = vars.isEmpty,
      thenProgram = successProgram(()),
      elseProgram = validationFailure(fieldValue, messageKey, Some(List(vars) ++ messageArgs))
    )
  }
}
