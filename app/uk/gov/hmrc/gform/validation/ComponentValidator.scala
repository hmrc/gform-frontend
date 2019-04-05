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
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }

object ComponentValidator {

  private def textData(formData: FormDataRecalculated, fieldValue: FormComponent) =
    formData.data.get(fieldValue.id) match {
      case Some(s) => s.filterNot(_.isEmpty()).toList
      case None    => Nil
    }

  def validateText(fieldValue: FormComponent, constraint: TextConstraint, retrievals: MaterialisedRetrievals)(
    data: FormDataRecalculated): ValidatedType[Unit] =
    (fieldValue.mandatory, textData(data, fieldValue), constraint) match {
      case (true, Nil, _)               => validationFailure(fieldValue, "must be entered")
      case (_, _, AnyText)              => validationSuccess
      case (_, value :: Nil, ShortText) => shortTextValidation(fieldValue, value)
      case (_, value :: Nil, BasicText) => textValidation(fieldValue, value)
      case (_, value :: Nil, TextWithRestrictions(min, max)) =>
        textValidationWithConstraints(fieldValue, value, min, max)
      case (_, value :: Nil, Sterling(_)) =>
        validateNumber(fieldValue, value, ValidationValues.sterlingLength, TextConstraint.defaultFactionalDigits, false)
      case (_, value :: Nil, UkBankAccountNumber) =>
        SortCodeValidation.checkLength(fieldValue, value, ValidationValues.bankAccountLength)
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

  private def validateNumber(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int,
    maxFractional: Int,
    mustBePositive: Boolean): ValidatedType[Unit] = {
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
    (TextConstraint.filterNumberValue(value), maxFractional, mustBePositive) match {
      case (WholeShape(_, whole, _), _, _) if surpassMaxLength(whole, maxWhole) =>
        validationFailure(fieldValue, s"must be at most $maxWhole digits")
      case (WholeShape("-", _, _), _, true) =>
        validationFailure(fieldValue, "must be a positive number")
      case (WholeShape(_, _, _), _, _) => validationSuccess
      case (FractionalShape(_, whole, _, fractional), 0, _)
          if surpassMaxLength(whole, maxWhole) && lessThanMinLength(fractional, 0) =>
        validationFailure(fieldValue, s"must be at most $maxWhole whole digits and no decimal fraction")
      case (FractionalShape(_, whole, _, fractional), _, _)
          if surpassMaxLength(whole, maxWhole) && surpassMaxLength(fractional, maxFractional) =>
        validationFailure(
          fieldValue,
          s"must be at most $maxWhole whole digits and decimal fraction must be at most $maxFractional digits")
      case (FractionalShape(_, whole, _, _), _, _) if surpassMaxLength(whole, maxWhole) =>
        validationFailure(fieldValue, s"must be at most $maxWhole whole digits")
      case (FractionalShape(_, _, _, fractional), 0, _) if lessThanMinLength(fractional, 0) =>
        validationFailure(fieldValue, "must be a whole number")
      case (FractionalShape(_, _, _, fractional), _, _) if surpassMaxLength(fractional, maxFractional) =>
        validationFailure(fieldValue, s"must be at most $maxFractional digits")
      case (FractionalShape("-", _, _, _), _, true) =>
        validationFailure(fieldValue, "must be a positive number")
      case (FractionalShape(_, _, _, _), _, _) => validationSuccess
      case (_, 0, true)                        => validationFailure(fieldValue, "must be a positive whole number")
      case (_, _, true)                        => validationFailure(fieldValue, f"must be a positive number")
      case (_, 0, false)                       => validationFailure(fieldValue, "must be a whole number")
      case _                                   => validationFailure(fieldValue, "must be a number")
    }
  }

  private def textValidationWithConstraints(fieldValue: FormComponent, value: String, min: Int, max: Int) =
    ComponentsValidator.validatorHelper(value.length, fieldValue, value, min, max)

  private def email(fieldValue: FormComponent, value: String) =
    if (EmailAddress.isValid(value)) validationSuccess
    else validationFailure(fieldValue, "is not valid")

  private def checkVrn(fieldValue: FormComponent, value: String) = {
    val Standard = "GB[0-9]{9}".r
    val Branch = "GB[0-9]{12}".r
    val Government = "GBGD[0-4][0-9]{2}".r
    val Health = "GBHA[5-9][0-9]{2}".r
    val str = value.replace(" ", "")
    str match {
      case Standard()   => validationSuccess
      case Branch()     => validationSuccess
      case Government() => validationSuccess
      case Health()     => validationSuccess
      case _            => validationFailure(fieldValue, "is not a valid VRN")
    }
  }

  private def checkCompanyRegistrationNumber(fieldValue: FormComponent, value: String) = {
    val ValidCRN = "[A-Z]{2}[0-9]{6}|[0-9]{8}".r
    val str = value.replace(" ", "")
    str match {
      case ValidCRN() => validationSuccess
      case _          => validationFailure(fieldValue, "is not a valid Company Registration Number")
    }
  }

  private def checkEORI(fieldValue: FormComponent, value: String) = {
    val ValidCRN = "^[A-Z]{2}[0-9A-Z]{7,15}$".r
    val str = value.replace(" ", "")
    str match {
      case ValidCRN() => validationSuccess
      case _          => validationFailure(fieldValue, "is not a valid EORI")
    }
  }

  private def checkNonUkCountryCode(fieldValue: FormComponent, value: String) = {
    val countryCode = "[A-Z]{2}".r
    value match {
      case countryCode() if value != "UK" => validationSuccess
      case _                              => validationFailure(fieldValue, "is not a valid non UK country code")
    }
  }

  private def checkCountryCode(fieldValue: FormComponent, value: String) = {
    val countryCode = "[A-Z]{2}".r
    value match {
      case countryCode() => validationSuccess
      case _             => validationFailure(fieldValue, "is not a valid country code")
    }
  }

  private def checkId(fieldValue: FormComponent, value: String) = {
    val UTR = "[0-9]{10}".r
    value match {
      case UTR()                => validationSuccess
      case x if Nino.isValid(x) => validationSuccess
      case _                    => validationFailure(fieldValue, "is not a valid Id")
    }
  }

  def validatePhoneNumber(
    fieldValue: FormComponent,
    value: String): Validated[Map[FormComponentId, Set[String]], Unit] =
    value.length match {
      case tooLong if tooLong > TelephoneNumber.maximumLength =>
        validationFailure(fieldValue, s"has more than ${TelephoneNumber.maximumLength} characters")
      case tooShort if tooShort < TelephoneNumber.minimumLength =>
        validationFailure(fieldValue, s"has fewer than ${TelephoneNumber.minimumLength} characters")
      case _ => validatePhoneNumberContent(value, fieldValue)
    }

  private def validatePhoneNumberContent(value: String, fieldValue: FormComponent) =
    value match {
      case TelephoneNumber.phoneNumberValidation() => validationSuccess
      case _ =>
        validationFailure(
          fieldValue,
          "can only contain numbers, plus signs, a hash key, uppercase letters, spaces, asterisks, round brackets, and hyphens")
    }

  private def shortTextValidation(fieldValue: FormComponent, value: String) = {
    val ShortTextValidation = """[A-Za-z0-9\'\-\.\&\s]{0,1000}""".r
    value match {
      case ShortTextValidation() => validationSuccess
      case _ =>
        validationFailure(
          fieldValue,
          "can only include letters, numbers, spaces, hyphens, ampersands and apostrophes"
        )
    }
  }

  private def textValidation(fieldValue: FormComponent, value: String) = {
    val TextValidation =
      """[A-Za-z0-9\(\)\,\'\-\.\r\s\£\\n\+\;\:\*\?\=\/\&\!\@\#\$\€\`\~\"\<\>\_\§\±\[\]\{\}]{0,100000}""".r
    value match {
      case TextValidation() => validationSuccess
      case _ =>
        validationFailure(
          fieldValue,
          "can only include letters, numbers, spaces and round, square, angled or curly brackets, apostrophes, hyphens, dashes, periods, pound signs, plus signs, semi-colons, colons, asterisks, question marks, equal signs, forward slashes, ampersands, exclamation marks, @ signs, hash signs, dollar signs, euro signs, back ticks, tildes, double quotes and underscores"
        )
    }
  }

  def validateChoice(fieldValue: FormComponent)(data: FormDataRecalculated): ValidatedType[Unit] = {
    val choiceValue = data.data.get(fieldValue.id).toList.flatten.headOption

    (fieldValue.mandatory, choiceValue) match {
      case (true, None | Some("")) =>
        validationFailure(fieldValue, "must be selected")
      case _ => validationSuccess
    }
  }

  private def surpassMaxLength(wholeOrFractional: String, maxLength: Int): Boolean =
    filterCommas(wholeOrFractional).length > maxLength

  private def lessThanMinLength(wholeOrFractional: String, minLength: Int): Boolean =
    wholeOrFractional.length > minLength

  private def filterCommas(number: String) = number.filterNot(c => c == ',')

}
