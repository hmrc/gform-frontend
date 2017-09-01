/*
 * Copyright 2017 HM Revenue & Customs
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

import java.time.LocalDate

import cats.Semigroup
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import cats.instances.all._
import cats.kernel.Monoid
import cats.syntax.cartesian._
import cats.syntax.validated._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.fileupload.{ Error, File, FileUploadService }
import uk.gov.hmrc.gform.fileupload.{ Error, File, FileUploadService, Infected }
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.models.ValidationUtil._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ UkSortCode, _ }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.play.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Failure, Success, Try }

//TODO: this validation must be performed on gform-backend site. Or else we will not able provide API for 3rd party services

class ValidationService(fileUploadService: FileUploadService, gformConnector: GformConnector) {

  def validateComponents(fieldValue: FieldValue, data: Map[FieldId, Seq[String]], envelopeId: EnvelopeId)(implicit hc: HeaderCarrier): Future[ValidatedType] =
    new ComponentsValidator(data, fileUploadService, envelopeId).validate(fieldValue)

  def validateUsingSectionValidators(v: SectionValidator, data: Map[FieldId, Seq[String]])(implicit hc: HeaderCarrier): Future[ValidatedType] = v match {
    case HMRCUTRPostcodeCheckValidator(errorMessage, utr, postcode) => gformConnector.validatePostCodeUtr(data.get(FieldId(utr.value)).toList.flatten.headOption.getOrElse(""), data.get(FieldId(postcode.value)).toList.flatten.headOption.getOrElse("")).map(if (_) Valid(()) else Invalid(Map(utr.toFieldId -> Set(errorMessage), postcode.toFieldId -> Set(errorMessage))))
  }

}

class ComponentsValidator(data: Map[FieldId, Seq[String]], fileUploadService: FileUploadService, envelopeId: EnvelopeId) {

  def validate(fieldValue: FieldValue)(implicit hc: HeaderCarrier): Future[ValidatedType] = fieldValue.`type` match {
    case sortCode @ UkSortCode(_) => validateSortCode(fieldValue, sortCode, fieldValue.mandatory)(data)
    case date @ Date(_, _, _) => validateDate(fieldValue, date)
    case text @ Text(_, _) => validateText(fieldValue, text)(data)
    case address @ Address(_) => validateAddress(fieldValue, address)(data)
    case Choice(_, _, _, _, _) => validateChoice(fieldValue)(data)
    case Group(_, _, _, _, _, _) => validF //a group is read-only
    case FileUpload() => validateFileUpload(fieldValue)
    case InformationMessage(_, _) => validF
  }

  private lazy val validF = Future.successful(Valid(()))

  private def validateDate(fieldValue: FieldValue, date: Date): Future[ValidatedType] = Future.successful {
    val reqFieldValidResult = validateDateRequiredField(fieldValue)(data)
    val otherRulesValidResult = validateDateImpl(fieldValue, date)(data)
    Monoid[ValidatedType]
    Monoid[ValidatedType].combineAll(List(reqFieldValidResult, otherRulesValidResult))
  }

  private lazy val dataGetter: FieldValue => String => Seq[String] = fv => suffix => data.get(fv.id.withSuffix(suffix)).toList.flatten
  private def validateDateRequiredField(fieldValue: FieldValue)(data: Map[FieldId, Seq[String]]): ValidatedType = {
    val dateValueOf = dataGetter(fieldValue)

    val validatedResult = fieldValue.mandatory match {
      case true =>
        List(
          validateRF(fieldValue, "day")(dateValueOf("day")),
          validateRF(fieldValue, "month")(dateValueOf("month")),
          validateRF(fieldValue, "year")(dateValueOf("year"))
        )
      case false => List(Valid(()))
    }
    Monoid[ValidatedType].combineAll(validatedResult)
  }

  private def validateDateImpl(fieldValue: FieldValue, date: Date)(data: Map[FieldId, Seq[String]]): ValidatedType = {
    val dateWithOffset = (localDate: LocalDate, offset: OffsetDate) => localDate.plusDays(offset.value)
    date.constraintType match {
      case AnyDate => validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data).andThen(lDate => Valid(()))
      case DateConstraints(dateConstraintList) =>

        val result = dateConstraintList.map {
          case DateConstraint(beforeOrAfter, dateConstrInfo, offsetDate) =>

            (beforeOrAfter, dateConstrInfo, offsetDate) match {

              case (Before, Today, offset) =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(inputDate =>
                    validateToday(fieldValue, inputDate,
                      offset, Map(fieldValue.id -> errors(fieldValue, "Date should be before Today")))(isBeforeToday))

              case (Before, concreteDate: ConcreteDate, offset) =>
                validateConcreteDate(concreteDate, Map(fieldValue.id -> errors(fieldValue, "enter a valid date")))
                  .andThen { concreteDate =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(inputDate =>
                        validateConcreteDate(fieldValue, inputDate,
                          concreteDate, offset,
                          Map(fieldValue.id ->
                            errors(fieldValue, s"Date should be before ${dateWithOffset(concreteDate, offset)}")))(isBeforeConcreteDate))
                  }

              case (beforeOrAfter @ _, DateField(fieldId), offset) => {

                lazy val validateOtherDate = validateInputDate(fieldValue, fieldId, None, data)

                lazy val validatedThisDate = validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)

                val beforeOrAfterString = beforeOrAfter match {
                  case After => "after"
                  case Before => "before"
                }

                val beforeOrAfterFunction = beforeOrAfter match {
                  case After => isAfterConcreteDate _
                  case Before => isBeforeConcreteDate _
                }

                validateOtherDate.andThen {
                  otherLocalDate =>
                    validatedThisDate.andThen {
                      thisLocalDate =>
                        validateConcreteDate(fieldValue, thisLocalDate,
                          otherLocalDate, offset,
                          Map(fieldValue.id ->
                            errors(fieldValue, s"Date should be ${beforeOrAfterString} ${dateWithOffset(otherLocalDate, offset)}")))(beforeOrAfterFunction)
                    }
                }
              }

              //              case (Before, AnyWord(value)) =>
              // case (Before, AnyWord(FieldId)) =>

              case (After, Today, offset) =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(inputDate =>
                    validateToday(fieldValue, inputDate,
                      offset, Map(fieldValue.id -> errors(fieldValue, "Date should be after today")))(isAfterToday))

              case (After, concreteDate: ConcreteDate, offset) =>

                validateConcreteDate(concreteDate, Map(fieldValue.id -> errors(fieldValue, "enter a valid date")))
                  .andThen { concreteDate =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(inputDate =>
                        validateConcreteDate(fieldValue, inputDate,
                          concreteDate, offset,
                          Map(fieldValue.id -> errors(fieldValue, s"Date should be after ${dateWithOffset(concreteDate, offset)}")))(isAfterConcreteDate))
                  }

              //              case (After, AnyWord(value)) =>

            }

        }
        Monoid[ValidatedType].combineAll(result)
    }
  }

  //TODO: this will be called many times per one form. Maybe there is a way to optimise it?
  private def validateFileUpload(fieldValue: FieldValue)(implicit hc: HeaderCarrier): Future[ValidatedType] = fileUploadService
    .getEnvelope(envelopeId).map { envelope =>

      val fileId = FileId(fieldValue.id.value)
      val file: Option[File] = envelope.files.find(_.fileId.value == fileId.value)

      file match {
        // format: OFF
        case Some(File(fileId, Error(reason), _))  => getError(fieldValue, reason)
        case Some(File(fileId, Infected, _))       => getError(fieldValue, "Virus detected")
        case Some(File(fileId, _, _))              => Valid(())
        case None if fieldValue.mandatory          => getError(fieldValue, "Please upload the file")
        case None                                  => Valid(())
        // format: ON
      }
    }

  private def validateText(fieldValue: FieldValue, text: Text)(data: Map[FieldId, Seq[String]]): Future[ValidatedType] = Future.successful {
    val textData = data.get(fieldValue.id).toList.flatten
    (fieldValue.mandatory, textData.filterNot(_.isEmpty()), text.constraint) match {
      case (true, Nil, _) => getError(fieldValue, "Please enter required data")
      case (_, _, AnyText) => Valid(())
      case (_, value :: Nil, ShortText) => shortTextValidation(fieldValue, value)
      case (_, value :: Nil, BasicText) => textValidation(fieldValue, value)
      case (_, value :: Nil, TextWithRestrictions(min, max)) => textValidator(fieldValue, value, min, max)
      case (_, value :: Nil, Sterling) => validateNumber(fieldValue, value, ValidationValues.sterlingLength, TextConstraint.defaultFactionalDigits, true)
      case (_, value :: Nil, UkBankAccountNumber) => checkLength(fieldValue, value, ValidationValues.bankAccountLength)
      case (_, value :: Nil, UTR) => checkId(fieldValue, value)
      case (_, value :: Nil, NINO) => checkId(fieldValue, value)
      case (_, value :: Nil, UkVrn) => checkVrn(fieldValue, value)
      case (_, value :: Nil, NonUkCountryCode) => checkNonUkCountryCode(fieldValue, value)
      case (_, value :: Nil, CountryCode) => checkCountryCode(fieldValue, value)
      case (_, value :: Nil, TelephoneNumber) => textValidator(fieldValue, value, ValidationValues.phoneDigits._1, ValidationValues.phoneDigits._2)
      case (_, value :: Nil, Email) => Monoid.combine(email(fieldValue, value), textValidator(fieldValue, value, 0, ValidationValues.emailLimit))
      case (_, value :: Nil, Number(maxWhole, maxFractional, _)) => validateNumber(fieldValue, value, maxWhole, maxFractional, false)
      case (_, value :: Nil, PositiveNumber(maxWhole, maxFractional, _)) => validateNumber(fieldValue, value, maxWhole, maxFractional, true)
      case (false, Nil, _) => Valid(())
      case (_, value :: rest, _) => Valid(()) // we don't support multiple values yet
    }
  }

  private def checkVrn(fieldValue: FieldValue, value: String) = {
    val Standard = "GB[0-9]{9}".r
    val Branch = "GB[0-9]{12}".r
    val Government = "GBGD[0-4][0-9]{2}".r
    val Health = "GBHA[5-9][0-9]{2}".r
    val str = value.replace(" ", "")
    str match {
      case Standard() => Valid(())
      case Branch() => Valid(())
      case Government() => Valid(())
      case Health() => Valid(())
      case _ => getError(fieldValue, "Not a valid VRN")
    }
  }

  private def checkNonUkCountryCode(fieldValue: FieldValue, value: String) = {
    val countryCode = "[A-Z]{2}".r
    value match {
      case countryCode() if value != "UK" => Valid(())
      case _ => getError(fieldValue, "Not a valid non UK country code")
    }
  }

  private def checkCountryCode(fieldValue: FieldValue, value: String) = {
    val countryCode = "[A-Z]{2}".r
    value match {
      case countryCode() => Valid(())
      case _ => getError(fieldValue, "Not a valid country code")
    }
  }

  private def checkId(fieldValue: FieldValue, value: String) = {
    val UTR = "[0-9]{10}".r
    value match {
      case UTR() => Valid(())
      case x if Nino.isValid(x) => Valid(())
      case _ => getError(fieldValue, "Not a valid Id")
    }
  }

  def shortTextValidation(fieldValue: FieldValue, value: String) = {
    val ShortTextValidation = "[0-9a-zA-Z\\s'\\-]{0,1000}".r
    value match {
      case ShortTextValidation() => Valid(())
      case _ => getError(fieldValue, "the text is too long for the validation")
    }
  }

  def textValidation(fieldValue: FieldValue, value: String) = {
    val TextValidation = """[A-Za-z0-9\(\)\,\'\-\.\r\s\£\\n\+\;\:\*\?\=\/\&\!\@\#\$\€\`\~\"\<\>\_\§\±\[\]\{\}]{0,100000}""".r
    value match {
      case TextValidation() => Valid(())
      case _ => getError(fieldValue, "The text is over 100000 so is not valid")
    }
  }

  private def textValidator(fieldValue: FieldValue, value: String, min: Int, max: Int) =
    value.length match {
      case tooLong if tooLong > max => getError(fieldValue, s"Entered too many characters should be at most $max long")
      case tooShort if tooShort < min => getError(fieldValue, s"Entered too few characters should be at least $min")
      case _ => Valid(())
    }

  private def email(fieldValue: FieldValue, value: String) =
    if (EmailAddress.isValid(value)) Valid(())
    else getError(fieldValue, "This email address is not valid")

  private def checkLength(fieldValue: FieldValue, value: String, desiredLength: Int) = {
    val WholeShape = s"[0-9]{$desiredLength}".r
    val x = "y"
    val FractionalShape = "([+-]?)(\\d*)[.](\\d+)".r
    value match {
      case FractionalShape(_, _, _) => getError(fieldValue, s"must be a whole number")
      case WholeShape() => Valid(())
      case _ => getError(fieldValue, s"must be a whole number of ${desiredLength} length")
    }
  }

  private def validateSortCode(fieldValue: FieldValue, sC: UkSortCode, mandatory: Boolean)(data: Map[FieldId, Seq[String]]) = Future.successful {
    Monoid[ValidatedType].combineAll(
      UkSortCode.fields(fieldValue.id)
        .map { fieldId =>
          val sortCode: Seq[String] = data.get(fieldId).toList.flatten
          (sortCode.filterNot(_.isEmpty), mandatory) match {
            case (Nil, true) => getError(fieldValue, "must be a whole number of 2 length")
            case (Nil, false) => Valid(())
            case (value :: Nil, _) => checkLength(fieldValue, value, 2)
            case (value :: Nil, _) => Valid(()) //Does not support multiple values
          }
        }
    )
  }

  private def validateNumber(fieldValue: FieldValue, value: String, maxWhole: Int, maxFractional: Int, mustBePositive: Boolean): ValidatedType = {
    val WholeShape = "([+-]?)(\\d+)[.]?".r
    val FractionalShape = "([+-]?)(\\d*)[.](\\d+)".r
    (value, maxFractional, mustBePositive) match {
      case (WholeShape(_, whole), _, _) if whole.size > maxWhole => getError(fieldValue, s"must be at most ${maxWhole} digits")
      case (WholeShape("-", _), _, true) => getError(fieldValue, "must be a positive number")
      case (WholeShape(_, _), _, _) => Valid(())
      case (FractionalShape(_, whole, fractional), 0, _) if whole.size > maxWhole && fractional.size > 0 => getError(fieldValue, s"number must be at most ${maxWhole} whole digits and no decimal fraction")
      case (FractionalShape(_, whole, fractional), _, _) if whole.size > maxWhole && fractional.size > maxFractional => getError(fieldValue, s"number must be at most ${maxWhole} whole digits and decimal fraction must be at most ${maxFractional} digits")
      case (FractionalShape(_, whole, _), _, _) if whole.size > maxWhole => getError(fieldValue, s"number must be at most ${maxWhole} whole digits")
      case (FractionalShape(_, _, fractional), 0, _) if fractional.size > 0 => getError(fieldValue, "must be a whole number")
      case (FractionalShape(_, _, fractional), _, _) if fractional.size > maxFractional => getError(fieldValue, s"decimal fraction must be at most ${maxFractional} digits")
      case (FractionalShape("-", _, _), _, true) => getError(fieldValue, "must be a positive number")
      case (FractionalShape(_, _, _), _, _) => Valid(())
      case (_, 0, true) => getError(fieldValue, "must be a positive whole number")
      case (_, _, true) => getError(fieldValue, "must be a positive number")
      case (_, 0, false) => getError(fieldValue, "must be a whole number")
      case _ => getError(fieldValue, "must be a number")
    }
  }

  private def validateRequired(fieldValue: FieldValue, fieldId: FieldId)(xs: Seq[String]): ValidatedType = {
    xs.filterNot(_.isEmpty()) match {
      case Nil => Invalid(Map(fieldId -> errors(fieldValue, "must be entered")))
      case value :: Nil => Valid(())
      case value :: rest => Valid(()) // we don't support multiple values yet
    }
  }

  private def validateForbidden(fieldValue: FieldValue, fieldId: FieldId)(xs: Seq[String]): ValidatedType = {
    xs.filterNot(_.isEmpty()) match {
      case Nil => Valid(())
      case value :: Nil => Invalid(Map(fieldId -> errors(fieldValue, "must not be entered")))
      case value :: rest => Invalid(Map(fieldId -> errors(fieldValue, "must not be entered"))) // we don't support multiple values yet
    }
  }

  private def addressLineValidation(fieldValue: FieldValue, fieldId: FieldId)(xs: Seq[String]): ValidatedType = {
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), fieldId.value) match {
      case (Nil, _) => Valid(())
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 => Invalid(Map(fieldId -> errors(fieldValue, s"this field is too long must be at most ${ValidationValues.addressLine4}")))
      case (value :: Nil, _) if value.length > ValidationValues.addressLine => Invalid(Map(fieldId -> errors(fieldValue, s"this field is too long must be at most ${ValidationValues.addressLine}")))
      case _ => Valid(())
    }
  }

  private def validateChoice(fieldValue: FieldValue)(data: Map[FieldId, Seq[String]]): Future[ValidatedType] = Future.successful {
    val choiceValue = data.get(fieldValue.id).toList.flatten.headOption

    (fieldValue.mandatory, choiceValue) match {
      case (true, None | Some("")) => getError(fieldValue, "Please enter required data")
      case _ => Valid(())
    }
  }

  def validateRF(fieldValue: FieldValue, value: String) = validateRequired(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateFF(fieldValue: FieldValue, value: String) = validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateAddress(fieldValue: FieldValue, address: Address)(data: Map[FieldId, Seq[String]]): Future[ValidatedType] = Future.successful {
    val addressValueOf: String => Seq[String] = suffix => data.get(fieldValue.id.withSuffix(suffix)).toList.flatten

    def validateRequiredFied(value: String) = validateRequired(fieldValue, fieldValue.id.withSuffix(value)) _

    def validateForbiddenField(value: String) = validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

    def lengthValidation(value: String) = addressLineValidation(fieldValue, fieldValue.id.withSuffix(value)) _

    val validatedResult: List[ValidatedType] = addressValueOf("uk") match {
      case "true" :: Nil =>
        List(
          Monoid[ValidatedType].combine(
            validateRequiredFied("street1")(addressValueOf("street1")),
            lengthValidation("street1")(addressValueOf("street1"))
          ),
          lengthValidation("street2")(addressValueOf("street2")),
          lengthValidation("street3")(addressValueOf("street3")),
          lengthValidation("street4")(addressValueOf("street4")),
          validateRequiredFied("postcode")(addressValueOf("postcode")),
          validateForbiddenField("country")(addressValueOf("country"))
        )
      case _ =>
        List(
          Monoid[ValidatedType].combine(validateRequiredFied("street1")(addressValueOf("street1")), lengthValidation("street1")(addressValueOf("street1"))),
          lengthValidation("street2")(addressValueOf("street2")),
          lengthValidation("street3")(addressValueOf("street3")),
          lengthValidation("street4")(addressValueOf("street4")),
          validateForbiddenField("postcode")(addressValueOf("postcode")),
          validateRequiredFied("country")(addressValueOf("country"))
        )
    }

    Monoid[ValidatedType].combineAll(validatedResult)
  }

  def validateToday(fieldValue: FieldValue, localDate: LocalDate,
    offset: OffsetDate, dateError: GformError)(func: (LocalDate, OffsetDate) => Boolean): ValidatedType = {
    func(localDate, offset) match {
      case true => Valid(())
      case false => Invalid(dateError)
    }
  }

  def validateConcreteDate(fieldValue: FieldValue, localDate: LocalDate,
    concreteDate: LocalDate, offset: OffsetDate, dateError: GformError)(func: (LocalDate, LocalDate, OffsetDate) => Boolean): ValidatedType = {
    func(localDate, concreteDate, offset) match {
      case true => Valid(())
      case false => Invalid(dateError)
    }
  }

  def isAfterToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean = date.isAfter(now.apply().plusDays(offset.value))

  def isAfterConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean = date.isAfter(concreteDay.plusDays(offset.value))

  def isBeforeToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean = date.isBefore(now.apply().plusDays(offset.value))

  def isBeforeConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean = date.isBefore(concreteDay.plusDays(offset.value))

  def validateConcreteDate(concreteDate: ConcreteDate, dateError: GformError): ValidatedLocalDate = {
    Try(LocalDate.of(concreteDate.year, concreteDate.month, concreteDate.day)) match {
      case Success(date) => Valid(date)
      case Failure(ex) => Invalid(dateError)
    }
  }

  def validateInputDate(fieldValue: FieldValue, fieldId: FieldId, errorMsg: Option[String], data: Map[FieldId, Seq[String]]): ValidatedLocalDate = {
    val fieldIdList = Date.fields(fieldId).map(fId => data.get(fId))

    fieldIdList match {
      case Some(day +: Nil) :: Some(month +: Nil) :: Some(year +: Nil) :: Nil =>

        validateLocalDate(fieldValue, errorMsg, day, month, year) match {
          case Valid(concreteDate) => validateConcreteDate(concreteDate, Map(fieldId -> errors(fieldValue, "enter a valid date")))
          case Invalid(nonEmptyList) => Invalid(nonEmptyList)
        }

      case _ =>
        getError(fieldValue, "Date is missing")
    }
  }

  def validateLocalDate(fieldValue: FieldValue, errorMessage: Option[String], day: String, month: String, year: String): ValidatedConcreteDate = {

    val d = isNumeric(day).andThen(y => isWithinBounds(y, 31)).leftMap(er => Map(fieldValue.id.withSuffix("day") -> Set(errorMessage.getOrElse(er))))
    val m = isNumeric(month).andThen(y => isWithinBounds(y, 12)).leftMap(er => Map(fieldValue.id.withSuffix("month") -> Set(errorMessage.getOrElse(er))))
    val y = isNumeric(year).andThen(y => hasValidNumberOfDigits(y, 4)).leftMap(er => Map(fieldValue.id.withSuffix("year") -> Set(errorMessage.getOrElse(er))))

    parallelWithApplicative(d, m, y)(ConcreteDate.apply)
  }

  def isNumeric(str: String): ValidatedNumeric = {
    Try(str.toInt) match {
      case Success(x) => Valid(x)
      case Failure(_) => Invalid("must be non-numeric")
    }
  }

  def isWithinBounds(number: Int, dayOrMonth: Int): ValidatedNumeric = {
    number match {
      case x if number <= dayOrMonth => Valid(number)
      case y if number > dayOrMonth => Invalid(s"entered is greater than $dayOrMonth")
    }
  }

  def hasValidNumberOfDigits(number: Int, digits: Int): ValidatedNumeric = {
    number.toString.length match {
      case x if x == digits => Valid(number)
      case y if y != digits => Invalid(s"is not a $digits digit number")
    }
  }

  def parallelWithApplicative[E: Semigroup](v1: Validated[E, Int], v2: Validated[E, Int], v3: Validated[E, Int])(f: (Int, Int, Int) => ConcreteDate): Validated[E, ConcreteDate] = (v3 |@| v2 |@| v1).map(f)

  def alwaysOk(fieldValue: FieldValue)(xs: Seq[String]): FormFieldValidationResult = {
    xs match {
      case Nil => FieldOk(fieldValue, "")
      case value :: rest => FieldOk(fieldValue, value) // we don't support multiple values yet
    }
  }

  private def errors(fieldValue: FieldValue, defaultErr: String): Set[String] = Set(fieldValue.errorMessage.getOrElse(defaultErr))

  private def getError(fieldValue: FieldValue, defaultMessage: String) = Map(fieldValue.id -> errors(fieldValue, defaultMessage)).invalid
}

object ValidationValues {

  val phoneDigits = (4, 30)
  val sortCodeLength = 2
  val bankAccountLength = 8
  val sterlingLength = 11
  val addressLine = 35
  val addressLine4 = 27
  val emailLimit = 241
}
