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

package uk.gov.hmrc.gform.validation

import cats.Monoid
import cats.implicits._
import org.typelevel.ci._
import play.api.i18n.Messages
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.eval.smartstring.{ SmartStringEvaluationSyntax, SmartStringEvaluator }
import uk.gov.hmrc.gform.lookup.LookupOptions.filterBySelectionCriteria
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.form.ThirdPartyData
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SubmissionRef }
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.referencechecker.{ CorporationTaxReferenceChecker, VatReferenceChecker }

import scala.util.matching.Regex

object ComponentValidator {
  // format: off
  val genericLongTextErrorPattern                            = "generic.longText.error.pattern"
  val genericReferenceNumberErrorPattern                     = "generic.referenceNumber.error.pattern"
  val genericCrnErrorInvalid                                 = "generic.crn.error.invalid"
  val genericEoriErrorPattern                                = "generic.eori.error.pattern"
  val genericUkEoriErrorPattern                              = "generic.ukEori.error.pattern"
  val genericChildBenefitNumberErrorPattern                  = "generic.childBenefitNumber.error.pattern"
  val genericNonUKCountryCodeErrorPattern                    = "generic.nonUKCountryCode.error.pattern"
  val genericCountryCodeErrorPattern                         = "generic.countryCode.error.pattern"
  val genericErrorTelephoneNumber                            = "generic.error.telephoneNumber"
  val genericErrorTelephoneNumberMinLength                   = "generic.error.telephoneNumber.minLength"
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
  val genericVrnErrorPattern                                 = "generic.vrn.error.pattern"
  val genericVrnErrorDigitCheck                              = "generic.vrn.error.digitcheck"
  val genericGovernmentIdNotExist                            = "generic.governmentId.not.exist"
  val genericGovernmentIdErrorPattern                        = "generic.governmentId.error.pattern"
  val genericErrorEmail                                      = "generic.error.email"
  val choiceErrorRequired                                    = "choice.error.required"
  val timeErrorRequired                                      = "time.error.required"
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
  ): ValidatedType[Unit] = {
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

    def lookupError: ValidatedType[Unit] = {
      val vars: List[String] = lookupLabel.label :: Nil
      validationFailure(fieldValue, genericErrorLookup, Some(vars))
    }

    def existsLabel(options: LookupOptions) =
      if (filteredLookuplabels.isDefined && filteredLookuplabels.fold(false)(_.contains(lookupLabel)))
        validationSuccess
      else if (filteredLookuplabels.isEmpty && options.contains(lookupLabel))
        validationSuccess
      else
        lookupError

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
    constraint: TextConstraint
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    lookupRegistry: LookupRegistry
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    (fieldValue.mandatory, textData(formModelVisibilityOptics, fieldValue), constraint) match {
      case (true, None, _) =>
        val key = fieldValue match {
          case lookupRegistry.extractors.IsRadioLookup(_) => choiceErrorRequired
          case lookupRegistry.extractors.IsUkSortCode(_)  => genericErrorSortCode
          case _                                          => genericErrorRequired
        }
        validationFailure(fieldValue, key, None)
      case (_, Some(value), lookup @ Lookup(_, _)) =>
        lookupValidation(fieldValue, lookupRegistry, lookup, LookupLabel(value), formModelVisibilityOptics)
      case (_, Some(value), ShortText(min, max)) => shortTextValidation(fieldValue, value, min, max)
      case (_, Some(value), TextWithRestrictions(min, max)) =>
        textValidationWithConstraints(fieldValue, value, min, max)
      case (_, Some(value), s: Sterling) =>
        validateNumber(
          fieldValue,
          value,
          ValidationValues.sterlingLength,
          TextConstraint.defaultFractionalDigits,
          s.positiveOnly
        )
      case (_, Some(value), s: WholeSterling) =>
        validateNumber(
          fieldValue,
          value,
          ValidationValues.sterlingLength,
          0,
          s.positiveOnly
        )
      case (_, Some(value), ReferenceNumber(min, max)) => referenceNumberConstraints(fieldValue, value, min, max)
      case (_, Some(value), UkBankAccountNumber)       => validateBankAccountFormat(fieldValue, value)
      case (_, Some(value), UkSortCodeFormat)          => validateSortCodeFormat(fieldValue, value)
      case (_, Some(value), SubmissionRefFormat)       => validateSubmissionRefFormat(fieldValue, value)
      case (_, Some(value), UTR)                       => checkUtr(fieldValue, value)
      case (_, Some(value), NINO)                      => checkNino(fieldValue, value)
      case (_, Some(value), UkVrn)                     => checkVrn(fieldValue, value)
      case (_, Some(value), PayeReference)             => checkPayeReference(fieldValue, value)
      case (_, Some(value), CompanyRegistrationNumber) => checkCompanyRegistrationNumber(fieldValue, value)
      case (_, Some(value), EORI)                      => checkEORI(fieldValue, value)
      case (_, Some(value), UkEORI)                    => checkUkEORI(fieldValue, value)
      case (_, Some(value), ChildBenefitNumber)        => checkChildBenefitNumber(fieldValue, value)
      case (_, Some(value), NonUkCountryCode)          => checkNonUkCountryCode(fieldValue, value)
      case (_, Some(value), CountryCode)               => checkCountryCode(fieldValue, value)
      case (_, Some(value), TelephoneNumber)           => validatePhoneNumber(fieldValue, value)
      case (_, Some(value), Email | EmailVerifiedBy(_, _)) =>
        Monoid.combine(
          email(fieldValue, value),
          textValidationWithConstraints(fieldValue, value, 0, ValidationValues.emailLimit)
        )
      case (_, Some(value), Number(maxWhole, maxFractional, _, _)) =>
        validateNumber(fieldValue, value, maxWhole, maxFractional, false)
      case (_, Some(value), PositiveNumber(maxWhole, maxFractional, _, _)) =>
        validateNumber(fieldValue, value, maxWhole, maxFractional, true)
      case (false, None, _) => validationSuccess
    }

  def validateParentSubmissionRef[D <: DataOrigin](
    fieldValue: FormComponent,
    thisFormSubmissionRef: SubmissionRef
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    (fieldValue.mandatory, textData(formModelVisibilityOptics, fieldValue)) match {
      case (true, None) =>
        validationFailure(fieldValue, genericErrorRequired, None)
      case (_, Some(value)) =>
        validateSubmissionRefFormat(fieldValue, value) andThen { _ =>
          if (value === thisFormSubmissionRef.value)
            validationFailure(fieldValue, genericErrorParentSubmissionRefSameAsFormSubmissionRef, None)
          else validationSuccess
        }
      case _ => validationSuccess
    }

  private def validateBankAccountFormat(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val ukBankAccountFormat = s"[0-9]{${ValidationValues.bankAccountLength}}".r
    val str = value.replace(" ", "")
    str match {
      case ukBankAccountFormat() => validationSuccess
      case _ =>
        val vars: List[String] = ValidationValues.bankAccountLength.toString :: Nil
        validationFailure(fieldValue, genericErrorExactNumbers, Some(vars))
    }
  }

  private def validateSortCodeFormat(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    value match {
      case ukSortCodeFormat() => validationSuccess
      case _ =>
        validationFailure(fieldValue, genericErrorSortCode, None)
    }

  private def validateSubmissionRefFormat(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val str = value.replace(" ", "")
    if (SubmissionRef.verifyCheckChar(str)) validationSuccess
    else validationFailure(fieldValue, genericErrorSubmissionRef, None)
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
  ): ValidatedType[Unit] = {
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
    (TextConstraint.filterNumberValue(value), maxFractional, mustBePositive) match {
      case (WholeShape(_, whole, _), _, _) if surpassMaxLength(whole, maxWhole) =>
        val vars: List[String] = maxWhole.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxWhole, Some(vars))
      case (WholeShape("-", _, _), 0, true) =>
        validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
      case (WholeShape("-", _, _), _, true) =>
        validationFailure(fieldValue, genericErrorPositiveNumber, None)
      case (WholeShape(_, _, _), _, _) => validationSuccess
      case (FractionalShape(_, _, _, _), 0, true) =>
        validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
      case (FractionalShape(_, whole, _, fractional), 0, _)
          if surpassMaxLength(whole, maxWhole) && lessThanMinLength(fractional, 0) =>
        val vars: List[String] = maxWhole.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxLengthNoDecimals, Some(vars))
      case (FractionalShape(_, whole, _, fractional), _, _)
          if surpassMaxLength(whole, maxWhole) && surpassMaxLength(fractional, maxFractional) =>
        val vars: List[String] = maxWhole.toString :: maxFractional.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxLengthMaxDecimals, Some(vars))
      case (FractionalShape(_, whole, _, _), _, _) if surpassMaxLength(whole, maxWhole) =>
        val vars: List[String] = maxWhole.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxWhole, Some(vars))
      case (FractionalShape(_, _, _, fractional), 0, _) if lessThanMinLength(fractional, 0) =>
        validationFailure(fieldValue, genericErrorWholeNumber, None)
      case (FractionalShape(_, _, _, fractional), _, _) if surpassMaxLength(fractional, maxFractional) =>
        val vars: List[String] = maxFractional.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxDecimals, Some(vars))
      case (FractionalShape("-", _, _, _), _, true) =>
        validationFailure(fieldValue, genericErrorPositiveNumber, None)
      case (FractionalShape(_, _, _, _), _, _) => validationSuccess
      case (_, 0, true)                        => validationFailure(fieldValue, genericErrorPositiveWholeNumber, None)
      case (_, _, true)                        => validationFailure(fieldValue, genericErrorPositiveNumber, None)
      case (_, 0, false)                       => validationFailure(fieldValue, genericErrorWholeNumber, None)
      case _                                   => validationFailure(fieldValue, genericErrorNumber, None)
    }
  }

  private[validation] def textValidationWithConstraints(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) =
    invalidCharactersValidator(fieldValue, value, validTextPattern, genericLongTextErrorPattern)
      .andThen { _ =>
        sharedTextComponentValidator(fieldValue, value, min, max, validTextPattern, genericLongTextErrorPattern)
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
      'ű', 'ų', 'ŵ', 'ẁ', 'ẃ', 'ẅ', 'ỳ', 'ý', 'ŷ', 'ÿ', 'ź', 'ż', 'ž', '«', '»', '¥')
    validChars.mkString("[A-Za-z0-9\\\\n\\̇\\'\\", "\\", "]+").r
  }

  private def referenceNumberConstraints(fieldValue: FormComponent, value: String, min: Int, max: Int)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidReferenceNumber = s"[0-9]{$min,$max}".r
    val str = value.replace(" ", "")
    sharedTextComponentValidator(fieldValue, str, min, max, ValidReferenceNumber, genericReferenceNumberErrorPattern)
  }

  private def email(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) =
    if (EmailAddress.isValid(value)) validationSuccess
    else validationFailure(fieldValue, genericErrorInvalid, None)

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
    str match {
      case tooLong if tooLong.length > 14 =>
        val vars: List[String] = 14.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxLength, Some(vars))
      case tooShort if tooShort.length < 7 =>
        val vars: List[String] = 7.toString :: Nil
        validationFailure(fieldValue, genericErrorMinLength, Some(vars))
      case Standard(_, s) if VatReferenceChecker.isValid(s) => validationSuccess
      case Standard(_, s)                                   => validationFailure(fieldValue, genericVrnErrorDigitCheck, None)
      case Branch()                                         => validationSuccess
      case Government()                                     => validationSuccess
      case Health()                                         => validationSuccess
      case _                                                => validationFailure(fieldValue, genericVrnErrorPattern, None)
    }
  }

  private def checkCompanyRegistrationNumber(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidCRN = "[A-Z]{2}[0-9]{6}|[0-9]{8}".r
    val str = value.replace(" ", "")
    sharedTextComponentValidator(fieldValue, str, 8, 8, ValidCRN, genericCrnErrorInvalid)
  }

  private def checkEORI(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidEORI = "^[A-Z]{2}[0-9A-Z]{7,15}$".r
    val str = value.replace(" ", "")
    sharedTextComponentValidator(fieldValue, str, 9, 17, ValidEORI, genericEoriErrorPattern)
  }

  private def checkUkEORI(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidUkEORI = "^GB[0-9]{12}$".r
    val str = value.replace(" ", "")
    sharedTextComponentValidator(fieldValue, str, 14, 14, ValidUkEORI, genericUkEoriErrorPattern)
  }
  private def checkChildBenefitNumber(fieldValue: FormComponent, value: String)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidChildBenefitNumber = "^CHB[0-9]{8}[A-Z]{2}$".r
    val str = value.replace(" ", "")
    sharedTextComponentValidator(
      fieldValue,
      str,
      13,
      13,
      ValidChildBenefitNumber,
      genericChildBenefitNumberErrorPattern
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
    if (value == "UK") validationFailure(fieldValue, genericNonUKCountryCodeErrorPattern, None)
    else sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, genericNonUKCountryCodeErrorPattern)
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

    value match {
      case UTRFormat() if CorporationTaxReferenceChecker.isValid(value) =>
        validationSuccess
      case UTRFormat() if !CorporationTaxReferenceChecker.isValid(value) =>
        validationFailure(fieldValue, genericGovernmentIdNotExist, None)
      case _ =>
        validationFailure(fieldValue, genericGovernmentIdErrorPattern, None)
    }
  }

  private def checkNino(fieldValue: FormComponent, value: String)(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) =
    value match {
      case x if Nino.isValid(x) => validationSuccess
      case _                    => validationFailure(fieldValue, genericGovernmentIdErrorPattern, None)
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
    sharedTextComponentValidator(fieldValue, str, 5, 14, ValidPaye, genericGovernmentIdErrorPattern)
  }

  def validatePhoneNumber(
    fieldValue: FormComponent,
    value: String
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] =
    sharedTextComponentValidator(
      fieldValue,
      value,
      TelephoneNumber.minimumLength,
      TelephoneNumber.maximumLength,
      TelephoneNumber.phoneNumberValidation,
      genericErrorTelephoneNumber
    )

  private[validation] def shortTextValidation(
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ) = {
    val ValidShortText = """[A-Za-z0-9\'\-\.\&\s]+""".r
    sharedTextComponentValidator(fieldValue, value, min, max, ValidShortText, genericShortTextErrorPattern)
  }

  def validateChoice[D <: DataOrigin](
    fieldValue: FormComponent
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val choiceValue = formModelVisibilityOptics.data
      .get(fieldValue.modelComponentId)
      .toSeq
      .flatMap(_.toSeq)
      .filterNot(_.isEmpty)

    if (fieldValue.mandatory && choiceValue.isEmpty) validationFailure(fieldValue, choiceErrorRequired, None)
    else validationSuccess
  }

  def validateChoiceNoneError[D <: DataOrigin](
    fieldValue: FormComponent,
    noneChoice: NoneChoice,
    error: LocalisedString
  )(
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit l: LangADT): ValidatedType[Unit] = {
    val choiceValue = formModelVisibilityOptics.data
      .many(fieldValue.modelComponentId)
      .toSeq
      .flatten
      .filterNot(_.isEmpty)

    if (choiceValue.contains(noneChoice.selection) && choiceValue.length > 1)
      Map(fieldValue.modelComponentId -> Set(error.value)).invalid
    else validationSuccess
  }

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
  ) =
    value match {
      case tooLong if tooLong.length > maxChars =>
        val vars: List[String] = maxChars.toString :: Nil
        validationFailure(fieldValue, genericErrorMaxLength, Some(vars))
      case tooShort if tooShort.length < minChars =>
        val vars: List[String] = minChars.toString :: Nil
        val errorMinLength = fieldValue match {
          case IsTelephone() => genericErrorTelephoneNumberMinLength
          case _             => genericErrorMinLength
        }
        validationFailure(fieldValue, errorMinLength, Some(vars))
      case regex() => validationSuccess
      case _       => validationFailure(fieldValue, messageKey, None)
    }

  def validateEmailCode[D <: DataOrigin](
    formComponent: FormComponent,
    emailFieldId: EmailFieldId,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    thirdPartyData: ThirdPartyData
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val expectedCode = thirdPartyData.emailVerification.get(emailFieldId).map(_.code)
    val maybeCode: Option[CIString] =
      formModelVisibilityOptics.data.one(formComponent.modelComponentId).map(c => ci"$c")

    val emailError = validationFailure(formComponent, genericErrorEmail, None)

    if (maybeCode === expectedCode.map(_.code)) validationSuccess else emailError

  }

  def validateTime[D <: DataOrigin](
    formComponent: FormComponent,
    time: Time,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    sse: SmartStringEvaluator
  ): ValidatedType[Unit] = {
    val timeValue = formModelVisibilityOptics.data.one(formComponent.modelComponentId).filterNot(_.isEmpty)

    (formComponent.mandatory, timeValue) match {
      case (true | false, Some(vTime)) if !(Range.timeSlots(time) contains vTime) =>
        validationFailure(
          formComponent,
          messages(genericErrorInvalid, formComponent.shortName.getOrElse(formComponent.label).value),
          None
        )
      case (true, None) =>
        validationFailure(
          formComponent,
          messages(timeErrorRequired, formComponent.shortName.getOrElse(formComponent.label).value),
          None
        )
      case _ => validationSuccess
    }
  }

  private def invalidCharactersValidator(
    fieldValue: FormComponent,
    value: String,
    regex: Regex,
    messageKey: String
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

    if (vars.isEmpty)
      validationSuccess
    else
      validationFailure(fieldValue, messageKey, Some(List(vars)))
  }
}
