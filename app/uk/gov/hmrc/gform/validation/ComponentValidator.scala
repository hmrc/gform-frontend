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
import cats.implicits._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.form.FormDataRecalculated
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ComponentsValidator.getError
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType

case class ComponentValidator(fieldValue: FormComponent, constraint: TextConstraint)(data: FormDataRecalculated) {

  def validateText(fieldValue: FormComponent, constraint: TextConstraint, retrievals: MaterialisedRetrievals)(
    data: FormDataRecalculated): ValidatedType[Unit] = {
    val textData = data.data.get(fieldValue.id) match { // extract
      case Some(s) => s.filterNot(_.isEmpty()).toList //
      case None    => Nil
    }
    (fieldValue.mandatory, textData, constraint) match {
      case (true, Nil, _)                                    => getError(fieldValue, "must be entered")
      case (_, _, AnyText)                                   => ().valid
      case (_, value :: Nil, ShortText)                      => shortTextValidation(fieldValue, value)
      case (_, value :: Nil, BasicText)                      => ComponentValidator.textValidation(fieldValue, value)
      case (_, value :: Nil, TextWithRestrictions(min, max)) => textValidator(fieldValue, value, min, max)
      case (_, value :: Nil, Sterling(_)) =>
        validateNumber(fieldValue, value, ValidationValues.sterlingLength, TextConstraint.defaultFactionalDigits, false)
      case (_, value :: Nil, UkBankAccountNumber) =>
        SortCodeValidation(data).checkLength(fieldValue, value, ValidationValues.bankAccountLength)
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
        Monoid.combine(email(fieldValue, value), textValidator(fieldValue, value, 0, ValidationValues.emailLimit))
      case (_, value :: Nil, Number(maxWhole, maxFractional, _, _)) =>
        validateNumber(fieldValue, value, maxWhole, maxFractional, false)
      case (_, value :: Nil, PositiveNumber(maxWhole, maxFractional, _, _)) =>
        validateNumber(fieldValue, value, maxWhole, maxFractional, true)
      case (false, Nil, _)       => ().valid
      case (_, value :: rest, _) => ().valid // we don't support multiple values yet
    }
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
      case (WholeShape(_, whole, _), _, _) if filterCommas(whole).length > maxWhole =>
        getError(fieldValue, s"must be at most $maxWhole digits")
      case (WholeShape("-", _, _), _, true) =>
        getError(fieldValue, "must be a positive number")
      case (WholeShape(_, _, _), _, _) => ().valid
      case (FractionalShape(_, whole, _, fractional), 0, _)
          if filterCommas(whole).length > maxWhole && fractional.length > 0 =>
        getError(fieldValue, s"must be at most $maxWhole whole digits and no decimal fraction")
      case (FractionalShape(_, whole, _, fractional), _, _)
          if filterCommas(whole).length > maxWhole && fractional.length > maxFractional =>
        getError(
          fieldValue,
          s"must be at most $maxWhole whole digits and decimal fraction must be at most $maxFractional digits")
      case (FractionalShape(_, whole, _, _), _, _) if filterCommas(whole).length > maxWhole =>
        getError(fieldValue, s"must be at most $maxWhole whole digits")
      case (FractionalShape(_, _, _, fractional), 0, _) if fractional.length > 0 =>
        getError(fieldValue, "must be a whole number")
      case (FractionalShape(_, _, _, fractional), _, _) if fractional.length > maxFractional =>
        getError(fieldValue, s"must be at most $maxFractional digits")
      case (FractionalShape("-", _, _, _), _, true) =>
        getError(fieldValue, "must be a positive number")
      case (FractionalShape(_, _, _, _), _, _) => ().valid
      case (_, 0, true)                        => getError(fieldValue, "must be a positive whole number")
      case (_, _, true)                        => getError(fieldValue, "must be a positive number")
      case (_, 0, false)                       => getError(fieldValue, "must be a whole number")
      case _                                   => getError(fieldValue, "must be a number")
    }
  }

  private def filterCommas(number: String) = number.filterNot(c => c == ',')

  private def textValidator(fieldValue: FormComponent, value: String, min: Int, max: Int) =
    ComponentsValidator.validatorHelper(value.length, fieldValue, value, min, max)

  private def validatePhoneNumber(fieldValue: FormComponent, value: String) =
    ComponentsValidator.validatorHelper(
      value.replace("+", "").length,
      fieldValue,
      value,
      TelephoneNumber.minimumLength,
      TelephoneNumber.maximumLength)

  private def email(fieldValue: FormComponent, value: String) =
    if (EmailAddress.isValid(value)) ().valid
    else getError(fieldValue, "is not valid")

  private def checkVrn(fieldValue: FormComponent, value: String) = {
    val Standard = "GB[0-9]{9}".r
    val Branch = "GB[0-9]{12}".r
    val Government = "GBGD[0-4][0-9]{2}".r
    val Health = "GBHA[5-9][0-9]{2}".r
    val str = value.replace(" ", "")
    str match {
      case Standard()   => ().valid
      case Branch()     => ().valid
      case Government() => ().valid
      case Health()     => ().valid
      case _            => getError(fieldValue, "is not a valid VRN")
    }
  }

  private def checkCompanyRegistrationNumber(fieldValue: FormComponent, value: String) = {
    val ValidCRN = "[A-Z]{2}[0-9]{6}|[0-9]{8}".r
    val str = value.replace(" ", "")
    str match {
      case ValidCRN() => ().valid
      case _          => getError(fieldValue, "is not a valid Company Registration Number")
    }
  }

  private def checkEORI(fieldValue: FormComponent, value: String) = {
    val ValidCRN = "^[A-Z]{2}[0-9A-Z]{7,15}$".r
    val str = value.replace(" ", "")
    str match {
      case ValidCRN() => ().valid
      case _          => getError(fieldValue, "is not a valid EORI")
    }
  }

  private def checkNonUkCountryCode(fieldValue: FormComponent, value: String) = {
    val countryCode = "[A-Z]{2}".r
    value match {
      case countryCode() if value != "UK" => ().valid
      case _                              => getError(fieldValue, "is not a valid non UK country code")
    }
  }

  private def checkCountryCode(fieldValue: FormComponent, value: String) = {
    val countryCode = "[A-Z]{2}".r
    value match {
      case countryCode() => ().valid
      case _             => getError(fieldValue, "is not a valid country code")
    }
  }

  private def checkId(fieldValue: FormComponent, value: String) = {
    val UTR = "[0-9]{10}".r
    value match {
      case UTR()                => ().valid
      case x if Nino.isValid(x) => ().valid
      case _                    => getError(fieldValue, "is not a valid Id")
    }
  }

  def shortTextValidation(fieldValue: FormComponent, value: String) = {
    val ShortTextValidation = """[A-Za-z0-9\'\-\.\&\s]{0,1000}""".r
    value match {
      case ShortTextValidation() => ().valid
      case _ =>
        getError(
          fieldValue,
          "can only include letters, numbers, spaces, hyphens, ampersands and apostrophes"
        )
    }
  }
}

case object ComponentValidator {
  def textValidation(fieldValue: FormComponent, value: String) = {
    val TextValidation =
      """[A-Za-z0-9\(\)\,\'\-\.\r\s\£\\n\+\;\:\*\?\=\/\&\!\@\#\$\€\`\~\"\<\>\_\§\±\[\]\{\}]{0,100000}""".r
    value match {
      case TextValidation() => ().valid
      case _ =>
        getError(
          fieldValue,
          "can only include letters, numbers, spaces and round, square, angled or curly brackets, apostrophes, hyphens, dashes, periods, pound signs, plus signs, semi-colons, colons, asterisks, question marks, equal signs, forward slashes, ampersands, exclamation marks, @ signs, hash signs, dollar signs, euro signs, back ticks, tildes, double quotes and underscores"
        )
    }
  }

  def validateChoice(fieldValue: FormComponent)(data: FormDataRecalculated): ValidatedType[Unit] = {
    val choiceValue = data.data.get(fieldValue.id).toList.flatten.headOption

    (fieldValue.mandatory, choiceValue) match {
      case (true, None | Some("")) =>
        getError(fieldValue, "must be selected")
      case _ => ().valid
    }
  }
}
