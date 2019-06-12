/*
 * Copyright 2019 HM Revenue & Customs
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
import cats.data.Validated
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.lookup.{ AjaxLookup, LookupId, LookupLabel, LookupRegistry, RadioLookup }
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

import scala.util.matching.Regex

object ComponentValidator {

  private def textData(formData: FormDataRecalculated, fieldValue: FormComponent) =
    formData.data.get(fieldValue.id) match {
      case Some(s) => s.filterNot(_.isEmpty()).toList
      case None    => Nil
    }

  private def lookupValidation(
    fieldValue: FormComponent,
    lookupRegistry: LookupRegistry,
    register: Register,
    lookupLabel: LookupLabel)(implicit messages: Messages, l: LangADT) = {

    def existsLabel(options: Map[LookupLabel, LookupId]) =
      if (options.contains(lookupLabel))
        validationSuccess
      else {
        val vars: List[String] = lookupLabel.label :: Nil
        validationFailure(fieldValue, "generic.error.lookup", Some(vars))
      }

    lookupRegistry.get(register) match {
      case Some(AjaxLookup(options, _, _)) => existsLabel(options)
      case Some(RadioLookup(options))      => existsLabel(options)
      case None =>
        val vars: List[String] = register.toString :: Nil
        validationFailure(fieldValue, "generic.error.registry", Some(vars))
    }
  }

  def validateText(fieldValue: FormComponent, constraint: TextConstraint, retrievals: MaterialisedRetrievals)(
    data: FormDataRecalculated,
    lookupRegistry: LookupRegistry)(implicit messages: Messages, l: LangADT): ValidatedType[Unit] =
    (fieldValue.mandatory, textData(data, fieldValue), constraint) match {
      case (true, Nil, _) =>
        val key = fieldValue match {
          case lookupRegistry.extractors.IsRadioLookup(_) => "choice.error.required"
          case _                                          => "generic.error.required"
        }
        validationFailure(fieldValue, key, None)
      case (_, _, AnyText) => validationSuccess
      case (_, value :: Nil, Lookup(register)) =>
        lookupValidation(fieldValue, lookupRegistry, register, LookupLabel(value))
      case (_, value :: Nil, ShortText(min, max)) => shortTextValidation(fieldValue, value, min, max)
      case (_, value :: Nil, BasicText)           => textValidation(fieldValue, value)
      case (_, value :: Nil, TextWithRestrictions(min, max)) =>
        textValidationWithConstraints(fieldValue, value, min, max)
      case (_, value :: Nil, Sterling(_)) =>
        validateNumber(fieldValue, value, ValidationValues.sterlingLength, TextConstraint.defaultFactionalDigits, false)
      case (_, value :: Nil, UkBankAccountNumber) =>
        validateBankAccountFormat(fieldValue, value)
      case (_, value :: Nil, UTR)                       => checkId(fieldValue, value)
      case (_, value :: Nil, NINO)                      => checkId(fieldValue, value)
      case (_, value :: Nil, UkVrn)                     => checkVrn(fieldValue, value)
      case (_, value :: Nil, CompanyRegistrationNumber) => checkCompanyRegistrationNumber(fieldValue, value)
      case (_, value :: Nil, EORI)                      => checkEORI(fieldValue, value)
      case (_, value :: Nil, NonUkCountryCode)          => checkNonUkCountryCode(fieldValue, value)
      case (_, value :: Nil, CountryCode)               => checkCountryCode(fieldValue, value)
      case (_, value :: Nil, TelephoneNumber) =>
        validatePhoneNumber(fieldValue, value)
      case (_, value :: Nil, Email) =>
        Monoid.combine(
          email(fieldValue, value),
          textValidationWithConstraints(fieldValue, value, 0, ValidationValues.emailLimit))
      case (_, value :: Nil, Number(maxWhole, maxFractional, _, _)) =>
        validateNumber(fieldValue, value, maxWhole, maxFractional, false)
      case (_, value :: Nil, PositiveNumber(maxWhole, maxFractional, _, _)) =>
        validateNumber(fieldValue, value, maxWhole, maxFractional, true)
      case (false, Nil, _)       => validationSuccess
      case (_, value :: rest, _) => validationSuccess // we don't support multiple values yet
    }

  private def validateBankAccountFormat(fieldValue: FormComponent, value: String)(
    implicit messages: Messages,
    l: LangADT): ValidatedType[Unit] = {
    val ukBankAccountFormat = s"[0-9]{${ValidationValues.bankAccountLength}}".r
    val str = value.replace(" ", "")
    str match {
      case ukBankAccountFormat() => validationSuccess
      case _ =>
        val vars: List[String] = ValidationValues.bankAccountLength.toString :: Nil
        validationFailure(fieldValue, "generic.error.exactNumbers", Some(vars))
    }
  }

  private def validateNumber(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int,
    maxFractional: Int,
    mustBePositive: Boolean)(implicit messages: Messages, l: LangADT): ValidatedType[Unit] = {
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
    (TextConstraint.filterNumberValue(value), maxFractional, mustBePositive) match {
      case (WholeShape(_, whole, _), _, _) if surpassMaxLength(whole, maxWhole) =>
        val vars: List[String] = maxWhole.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxWhole", Some(vars))
      case (WholeShape("-", _, _), _, true) =>
        validationFailure(fieldValue, "generic.error.positiveNumber", None)
      case (WholeShape(_, _, _), _, _) => validationSuccess
      case (FractionalShape(_, whole, _, fractional), 0, _)
          if surpassMaxLength(whole, maxWhole) && lessThanMinLength(fractional, 0) =>
        val vars: List[String] = maxWhole.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxLength.noDecimals", Some(vars))
      case (FractionalShape(_, whole, _, fractional), _, _)
          if surpassMaxLength(whole, maxWhole) && surpassMaxLength(fractional, maxFractional) =>
        val vars: List[String] = maxWhole.toString :: maxFractional.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxLength.maxDecimals", Some(vars))
      case (FractionalShape(_, whole, _, _), _, _) if surpassMaxLength(whole, maxWhole) =>
        val vars: List[String] = maxWhole.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxWhole", Some(vars))
      case (FractionalShape(_, _, _, fractional), 0, _) if lessThanMinLength(fractional, 0) =>
        validationFailure(fieldValue, "generic.error.wholeNumber", None)
      case (FractionalShape(_, _, _, fractional), _, _) if surpassMaxLength(fractional, maxFractional) =>
        val vars: List[String] = maxFractional.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxDecimals", Some(vars))
      case (FractionalShape("-", _, _, _), _, true) =>
        validationFailure(fieldValue, "generic.error.positiveNumber", None)
      case (FractionalShape(_, _, _, _), _, _) => validationSuccess
      case (_, 0, true)                        => validationFailure(fieldValue, "generic.error.positiveWholeNumber", None)
      case (_, _, true)                        => validationFailure(fieldValue, "generic.error.positiveNumber", None)
      case (_, 0, false)                       => validationFailure(fieldValue, "generic.error.wholeNumber", None)
      case _                                   => validationFailure(fieldValue, "generic.error.number", None)
    }
  }

  private def textValidationWithConstraints(fieldValue: FormComponent, value: String, min: Int, max: Int)(
    implicit messages: Messages,
    l: LangADT) = {
    val ValidText = ".+".r
    val errorMSG = messages("generic.error.invalid")
    sharedTextComponentValidator(fieldValue, value, min, max, ValidText, errorMSG)
  }

  private def email(fieldValue: FormComponent, value: String)(implicit messages: Messages, l: LangADT) =
    if (EmailAddress.isValid(value)) validationSuccess
    else validationFailure(fieldValue, "generic.error.invalid", None)

  private def checkVrn(fieldValue: FormComponent, value: String)(implicit messages: Messages, l: LangADT) = {
    val Standard = "GB[0-9]{9}".r
    val Branch = "GB[0-9]{12}".r
    val Government = "GBGD[0-4][0-9]{2}".r
    val Health = "GBHA[5-9][0-9]{2}".r
    val str = value.replace(" ", "")
    str match {
      case tooLong if tooLong.length > 14 =>
        val vars: List[String] = 14.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxLength", Some(vars))
      case tooShort if tooShort.length < 7 =>
        val vars: List[String] = 7.toString :: Nil
        validationFailure(fieldValue, "generic.error.minLength", Some(vars))
      case Standard()   => validationSuccess
      case Branch()     => validationSuccess
      case Government() => validationSuccess
      case Health()     => validationSuccess
      case _            => validationFailure(fieldValue, "generic.vrn.error.pattern", None)
    }
  }

  private def checkCompanyRegistrationNumber(fieldValue: FormComponent, value: String)(
    implicit messages: Messages,
    l: LangADT) = {
    val ValidCRN = "[A-Z]{2}[0-9]{6}|[0-9]{8}".r
    val str = value.replace(" ", "")
    val errorMSG = messages("generic.crn.error.invalid")
    sharedTextComponentValidator(fieldValue, str, 8, 8, ValidCRN, errorMSG)
  }

  private def checkEORI(fieldValue: FormComponent, value: String)(implicit messages: Messages, l: LangADT) = {
    val ValidEORI = "^[A-Z]{2}[0-9A-Z]{7,15}$".r
    val str = value.replace(" ", "")
    val errorMSG = messages("generic.eori.error.pattern")
    sharedTextComponentValidator(fieldValue, str, 9, 17, ValidEORI, errorMSG)
  }

  private def checkNonUkCountryCode(fieldValue: FormComponent, value: String)(
    implicit messages: Messages,
    l: LangADT) = {
    val ValidCountryCode = "[A-Z]+".r
    val messageKey = "generic.nonUKCountryCode.error.pattern"
    if (value == "UK") validationFailure(fieldValue, messageKey, None)
    else sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, messageKey)
  }

  private def checkCountryCode(fieldValue: FormComponent, value: String)(implicit messages: Messages, l: LangADT) = {
    val ValidCountryCode = "[A-Z]+".r
    val messageKey = "generic.countryCode.error.pattern"
    sharedTextComponentValidator(fieldValue, value, 2, 2, ValidCountryCode, messageKey)
  }

  private def checkId(fieldValue: FormComponent, value: String)(implicit messages: Messages, l: LangADT) = {
    val ValidUTR = "[0-9]{10}".r
    value match {
      case ValidUTR()           => validationSuccess
      case x if Nino.isValid(x) => validationSuccess
      case _                    => validationFailure(fieldValue, "generic.governmentId.error.pattern", None)
    }
  }

  def validatePhoneNumber(fieldValue: FormComponent, value: String)(
    implicit messages: Messages,
    l: LangADT): Validated[Map[FormComponentId, Set[String]], Unit] = {
    val messageKey = "generic.error.telephoneNumber"
    sharedTextComponentValidator(
      fieldValue,
      value,
      TelephoneNumber.minimumLength,
      TelephoneNumber.maximumLength,
      TelephoneNumber.phoneNumberValidation,
      messageKey)
  }

  private[validation] def shortTextValidation(fieldValue: FormComponent, value: String, min: Int, max: Int)(
    implicit messages: Messages,
    l: LangADT) = {
    val ValidShortText = """[A-Za-z0-9\'\-\.\&\s]+""".r
    val messageKey = "generic.shortText.error.pattern"
    sharedTextComponentValidator(fieldValue, value, min, max, ValidShortText, messageKey)
  }

  private def textValidation(fieldValue: FormComponent, value: String)(implicit messages: Messages, l: LangADT) = {
    val ValidText = """[A-Za-z0-9\(\)\,\'\-\.\r\s\£\\n\+\;\:\*\?\=\/\&\!\@\#\$\€\`\~\"\<\>\_\§\±\[\]\{\}]+""".r
    val messageKey = "generic.longText.error.pattern"
    sharedTextComponentValidator(fieldValue, value, 0, 100000, ValidText, messageKey)
  }

  def validateChoice(fieldValue: FormComponent)(
    data: FormDataRecalculated)(implicit messages: Messages, l: LangADT): ValidatedType[Unit] = {
    val choiceValue = data.data.get(fieldValue.id).toList.flatten.headOption

    (fieldValue.mandatory, choiceValue) match {
      case (true, None | Some("")) =>
        validationFailure(fieldValue, "choice.error.required", None)
      case _ => validationSuccess
    }
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
    messageKey: String)(implicit messages: Messages, l: LangADT) =
    value match {
      case tooLong if tooLong.length > maxChars =>
        val vars: List[String] = maxChars.toString :: Nil
        validationFailure(fieldValue, "generic.error.maxLength", Some(vars))
      case tooShort if tooShort.length < minChars =>
        val vars: List[String] = minChars.toString :: Nil
        validationFailure(fieldValue, "generic.error.minLength", Some(vars))
      case regex() => validationSuccess
      case _       => validationFailure(fieldValue, messageKey, None)
    }
}
