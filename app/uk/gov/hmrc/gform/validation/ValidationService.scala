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

import java.time.LocalDate

import cats.data.Validated.{ Invalid, Valid }
import cats.data._
import cats.implicits._
import cats.{ Monoid, Semigroup }
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse, InternationalAddress, UkAddress }
import uk.gov.hmrc.gform.sharedmodel.form.{ Validated => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Today, _ }
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, NotFound, ServiceResponse }
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.{ ValidatedType, _ }
import uk.gov.hmrc.gform.views.html._
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

//TODO: this validation must be performed on gform-backend site. Or else we will not able provide API for 3rd party services

class ValidationService(
  fileUploadService: FileUploadService,
  gformConnector: GformConnector,
  booleanExpr: BooleanExprEval[Future]
)(
  implicit ec: ExecutionContext
) {

  private def validateFieldValue(
    fieldValue: FormComponent,
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    new ComponentsValidator(data, fileUploadService, envelopeId, retrievals, booleanExpr, thirdPartyData, formTemplate)
      .validate(fieldValue, fieldValues)

  def validateComponents(
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    fieldValues
      .traverse(fv => validateFieldValue(fv, fieldValues, data, envelopeId, retrievals, thirdPartyData, formTemplate))
      .map(Monoid[ValidatedType[Unit]].combineAll)

  private def validateUsingValidators(section: Section, data: FormDataRecalculated)(
    implicit hc: HeaderCarrier): Future[ValidatedType[ValidationResult]] = {
    val sv: Option[Validator] = section.validators
    section.validators
      .map(validateUsingSectionValidators(_, data))
      .getOrElse(ValidationResult.empty.valid.pure[Future])
  }

  def validateForm(
    sectionFields: List[FormComponent],
    section: Section,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate)(data: FormDataRecalculated)(
    implicit hc: HeaderCarrier): Future[ValidatedType[ValidationResult]] = {
    val eT = for {
      _ <- EitherT(
            validateComponents(sectionFields, data, envelopeId, retrievals, thirdPartyData, formTemplate).map(
              _.toEither))
      valRes <- EitherT(validateUsingValidators(section, data).map(_.toEither))
    } yield valRes
    eT.value.map(Validated.fromEither)
  }

  def evaluateValidation(
    v: ValidatedType[ValidationResult],
    fields: List[FormComponent],
    data: FormDataRecalculated,
    envelope: Envelope): List[(FormComponent, FormFieldValidationResult)] =
    // We need to keep the formComponent order as they appear on the form for page-level-error rendering, do not convert to map
    ValidationUtil
      .evaluateValidationResult(fields, v, data, envelope)
      .map(ffvr => ffvr.fieldValue -> ffvr)

  private def validateUsingSectionValidators(v: Validator, data: FormDataRecalculated)(
    implicit hc: HeaderCarrier): Future[ValidatedType[ValidationResult]] = {
    def dataGetter(fieldId: FormComponentId): String =
      data.data.get(fieldId).toList.flatten.headOption.getOrElse("")

    def compare(postCode: String)(drr: DesRegistrationResponse): Boolean = {
      val maybePostalCode = drr.address match {
        case UkAddress(_, _, _, _, postalCode)               => postalCode
        case InternationalAddress(_, _, _, _, _, postalCode) => postalCode
      }
      maybePostalCode.fold(true)(_.replace(" ", "").equalsIgnoreCase(postCode.replace(" ", "")))
    }

    v match {
      case HmrcRosmRegistrationCheckValidator(errorMessage, regime, utr, postcode) =>
        def findByKey(key: String): String = data.data.get(FormComponentId(key)).toList.flatten.headOption.getOrElse("")

        val utrValue = findByKey(utr.value)
        val postcodeValue = findByKey(postcode.value)

        val errors = Map(utr.toFieldId -> Set(errorMessage), postcode.toFieldId -> Set(errorMessage))

        val desRegistrationRequest = DesRegistrationRequest(regime, false, false)

        gformConnector
          .validatePostCodeUtr(utrValue, desRegistrationRequest)
          .flatMap {
            case NotFound               => Future.successful(errors.invalid)
            case CannotRetrieveResponse => Future.failed(new Exception("Call to des registration has failed"))
            case ServiceResponse(drr) =>
              Future.successful(
                if (compare(postcodeValue)(drr)) ValidationResult(Some(drr)).valid
                else errors.invalid
              )
          }
      case BankAccoutnModulusCheck(errorMessage, accountNumber, sortCode) =>
        val sortCodeCombined = UkSortCode.fields(sortCode.toFieldId).toList.map(dataGetter).mkString("-")
        val errors = Map(accountNumber.toFieldId -> Set(errorMessage), sortCode.toFieldId -> Set(errorMessage))
        gformConnector
          .validateBankModulus(dataGetter(accountNumber.toFieldId), sortCodeCombined)
          .map(b => if (b) ValidationResult.empty.valid else errors.invalid)
    }
  }
}

class ComponentsValidator(
  data: FormDataRecalculated,
  fileUploadService: FileUploadService,
  envelopeId: EnvelopeId,
  retrievals: MaterialisedRetrievals,
  booleanExpr: BooleanExprEval[Future],
  thirdPartyData: ThirdPartyData,
  formTemplate: FormTemplate)(
  implicit ec: ExecutionContext
) {

  def validate(fieldValue: FormComponent, fieldValues: List[FormComponent])(
    implicit hc: HeaderCarrier): Future[ValidatedType[Unit]] = {

    def validIf(validationResult: ValidatedType[Unit]): Future[ValidatedType[Unit]] =
      (validationResult.isValid, fieldValue.validIf) match {
        case (true, Some(vi)) =>
          booleanExpr
            .isTrue(vi.expr, data.data, retrievals, data.invisible, thirdPartyData, envelopeId, formTemplate)
            .map {
              case false => getError(fieldValue, "must be entered")
              case true  => validationResult
            }
        case _ => validationResult.pure[Future]
      }

    fieldValue.`type` match {
      case sortCode @ UkSortCode(_) => validIf(validateSortCode(fieldValue, sortCode, fieldValue.mandatory)(data))
      case date @ Date(_, _, _) =>
        validIf(validateDate(fieldValue, date, getCompanionFieldComponent(date, fieldValues)))
      case text @ Text(constraint, _, _) => validIf(validateText(fieldValue, constraint, retrievals)(data))
      case TextArea(constraint, _, _)    => validIf(validateText(fieldValue, constraint, retrievals)(data))
      case address @ Address(_)          => validIf(validateAddress(fieldValue, address)(data))
      case c @ Choice(_, _, _, _, _)     => validIf(validateChoice(fieldValue)(data))
      case Group(_, _, _, _, _, _)       => validF //a group is read-only
      case FileUpload()                  => validateFileUpload(data, fieldValue)
      case InformationMessage(_, _)      => validF
      case HmrcTaxPeriod(_, _, _)        => validIf(validateChoice(fieldValue)(data))
    }
  }

  def validF(implicit ec: ExecutionContext) =
    ().valid.pure[Future]

  private def validateDate(
    fieldValue: FormComponent,
    date: Date,
    otherFieldValue: Option[FormComponent]): ValidatedType[Unit] =
    Monoid[ValidatedType[Unit]].combineAll(
      List(validateDateRequiredField(fieldValue), validateDateImpl(fieldValue, date, otherFieldValue)(data)))

  private lazy val dataGetter: FormComponent => String => Seq[String] = fv =>
    suffix => data.data.get(fv.id.withSuffix(suffix)).toList.flatten
  private def validateDateRequiredField(fieldValue: FormComponent): ValidatedType[Unit] = {
    val dateValueOf = dataGetter(fieldValue)

    val validatedResult = fieldValue.mandatory match {
      case true =>
        List(
          validateRF(fieldValue, "day")(dateValueOf("day")),
          validateRF(fieldValue, "month")(dateValueOf("month")),
          validateRF(fieldValue, "year")(dateValueOf("year"))
        )
      case false => List(().valid)
    }
    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  private def messagePrefix(
    fieldValue: FormComponent,
    workedOnId: FormComponentId,
    otherFormComponent: Option[FormComponent]) =
    otherFormComponent match {
      case Some(x) if x.id === workedOnId => localisation(x.shortName.getOrElse(x.label))
      case Some(x)                        => localisation(fieldValue.shortName.getOrElse(fieldValue.label))
      case None                           => localisation(fieldValue.shortName.getOrElse(fieldValue.label))
    }

  private def validateDateImpl(fieldValue: FormComponent, date: Date, otherFieldValue: Option[FormComponent])(
    data: FormDataRecalculated): ValidatedType[Unit] =
    date.constraintType match {

      case AnyDate =>
        validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data, otherFieldValue).andThen(lDate =>
          ().valid)

      case DateConstraints(dateConstraintList) =>
        val result = dateConstraintList.map {

          case DateConstraint(beforeOrAfterOrPrecisely, dateConstrInfo, offsetDate) =>
            (beforeOrAfterOrPrecisely, dateConstrInfo, offsetDate) match {

              case (beforeAfterPrecisely: BeforeAfterPrecisely, concreteDate: ConcreteDate, offset) =>
                validateConcreteDateWithMessages(fieldValue, beforeAfterPrecisely, concreteDate, offset, data)

              case (beforeAfterPrecisely: BeforeAfterPrecisely, Today, offset) =>
                validateTodayWithMessages(fieldValue, beforeAfterPrecisely, offset, data)

              case (beforeAfterPrecisely @ _, dateField: DateField, offset) =>
                validateDateFieldWithMessages(
                  fieldValue,
                  beforeAfterPrecisely,
                  dateField,
                  offset,
                  data,
                  otherFieldValue)
            }

        }
        Monoid[ValidatedType[Unit]].combineAll(result)
    }
  def validateConcreteDateWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    concreteDate: ConcreteDate,
    offset: OffsetDate,
    data: FormDataRecalculated): Validated[GformError, Unit] =
    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data).andThen(
      inputDate =>
        validateConcreteDate(
          fieldValue,
          inputDate,
          concreteDate,
          offset,
          Map(fieldValue.id -> errors(fieldValue, incorrectDateMessage(beforeAfterPrecisely, concreteDate, offset))))(
          concreteDateFunctionMatch(beforeAfterPrecisely)))

  def validateTodayWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate,
    data: FormDataRecalculated): Validated[GformError, Unit] =
    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
      .andThen(
        inputDate =>
          validateToday(
            fieldValue,
            inputDate,
            offset,
            Map(fieldValue.id -> errors(fieldValue, s"must be ${beforeAfterPrecisely.mkString} today")))(
            todayFunctionMatch(beforeAfterPrecisely)))

  def validateDateFieldWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    dateField: DateField,
    offset: OffsetDate,
    data: FormDataRecalculated,
    otherFieldValue: Option[FormComponent]): Validated[GformError, Unit] = {

    lazy val validateOtherDate = validateInputDate(fieldValue, dateField.value, None, data, otherFieldValue)

    lazy val validatedThisDate = validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)

    validateOtherDate.andThen { otherLocalDate =>
      validatedThisDate.andThen { thisLocalDate =>
        validateConcreteDate(
          fieldValue,
          thisLocalDate,
          localDateToConcreteDate(otherLocalDate),
          offset,
          Map(
            fieldValue.id ->
              errors(
                fieldValue,
                incorrectDateMessage(beforeAfterPrecisely, localDateToConcreteDate(otherLocalDate), offset)))
        )(concreteDateFunctionMatch(beforeAfterPrecisely))
      }
    }
  }

  def validateTodayWithMessages(
    fieldValue: FormComponent,
    beforeAfterPrecisely: BeforeAfterPrecisely,
    offset: OffsetDate): Validated[GformError, Unit] =
    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
      .andThen(
        inputDate =>
          validateToday(fieldValue, inputDate, offset, Map(fieldValue.id -> errors(fieldValue, "must be today")))(
            todayFunctionMatch(beforeAfterPrecisely)))

  //TODO: this will be called many times per one form. Maybe there is a way to optimise it?
  private def validateFileUpload(data: FormDataRecalculated, fieldValue: FormComponent)(
    implicit hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    fileUploadService
      .getEnvelope(envelopeId)
      .map { envelope =>
        val fileId = FileId(fieldValue.id.value)
        val file: Option[File] = envelope.files.find(_.fileId.value == fileId.value)

        file match {
          case Some(File(fileId, Error(Some(reason)), _)) => getError(fieldValue, reason)
          case Some(File(fileId, Error(None), _)) =>
            getError(fieldValue, "has an unknown error from file upload")
          case Some(File(fileId, Infected, _)) =>
            getError(fieldValue, "has a virus detected")
          case Some(File(fileId, _, _))     => ().valid
          case None if fieldValue.mandatory => getError(fieldValue, "must be uploaded")
          case None                         => ().valid
        }
      }

  private def validateText(fieldValue: FormComponent, constraint: TextConstraint, retrievals: MaterialisedRetrievals)(
    data: FormDataRecalculated): ValidatedType[Unit] = {
    val textData = data.data.get(fieldValue.id) match {
      case Some(s) => s.filterNot(_.isEmpty()).toList
      case None    => Nil
    }
    (fieldValue.mandatory, textData, constraint) match {
      case (true, Nil, _)                                    => getError(fieldValue, "must be entered")
      case (_, _, AnyText)                                   => ().valid
      case (_, value :: Nil, ShortText)                      => shortTextValidation(fieldValue, value)
      case (_, value :: Nil, BasicText)                      => textValidation(fieldValue, value)
      case (_, value :: Nil, TextWithRestrictions(min, max)) => textValidator(fieldValue, value, min, max)
      case (_, value :: Nil, Sterling(_)) =>
        validateNumber(fieldValue, value, ValidationValues.sterlingLength, TextConstraint.defaultFactionalDigits, false)
      case (_, value :: Nil, UkBankAccountNumber) =>
        checkLength(fieldValue, value, ValidationValues.bankAccountLength)
      case (_, value :: Nil, UTR)                       => checkId(fieldValue, value)
      case (_, value :: Nil, NINO)                      => checkId(fieldValue, value)
      case (_, value :: Nil, UkVrn)                     => checkVrn(fieldValue, value)
      case (_, value :: Nil, CompanyRegistrationNumber) => checkCompanyRegistrationNumber(fieldValue, value)
      case (_, value :: Nil, EORI)                      => checkEORI(fieldValue, value)
      case (_, value :: Nil, NonUkCountryCode)          => checkNonUkCountryCode(fieldValue, value)
      case (_, value :: Nil, CountryCode)               => checkCountryCode(fieldValue, value)
      case (_, value :: Nil, TelephoneNumber) =>
        textValidator(fieldValue, value, ValidationValues.phoneDigits._1, ValidationValues.phoneDigits._2)
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

  private def textValidator(fieldValue: FormComponent, value: String, min: Int, max: Int) =
    value.length match {
      case tooLong if tooLong > max =>
        getError(fieldValue, s"has more than $max characters")
      case tooShort if tooShort < min =>
        getError(fieldValue, s"has less than $min characters")
      case _ => ().valid
    }

  private def email(fieldValue: FormComponent, value: String) =
    if (EmailAddress.isValid(value)) ().valid
    else getError(fieldValue, "is not valid")

  private def checkLength(fieldValue: FormComponent, value: String, desiredLength: Int) = {
    val WholeShape = s"[0-9]{$desiredLength}".r
    val x = "y"
    val FractionalShape = "([+-]?)(\\d*)[.](\\d+)".r
    value match {
      case FractionalShape(_, _, _) =>
        getError(fieldValue, "must be a whole number")
      case WholeShape() => ().valid
      case _ =>
        getError(fieldValue, s"must be $desiredLength numbers")
    }
  }

  private def validateSortCode(fieldValue: FormComponent, sC: UkSortCode, mandatory: Boolean)(
    data: FormDataRecalculated) =
    Monoid[ValidatedType[Unit]].combineAll(
      UkSortCode
        .fields(fieldValue.id)
        .toList
        .map { fieldId =>
          val sortCode: Seq[String] = data.data.get(fieldId).toList.flatten
          (sortCode.filterNot(_.isEmpty), mandatory) match {
            case (Nil, true) =>
              getError(fieldValue, "values must be two digit numbers")
            case (Nil, false)      => ().valid
            case (value :: Nil, _) => checkLength(fieldValue, value, 2)
          }
        }
    )

  private def filterCommas(number: String) = number.filterNot(c => c == ',')

  private def validateNumber(
    fieldValue: FormComponent,
    value: String,
    maxWhole: Int,
    maxFractional: Int,
    mustBePositive: Boolean): ValidatedType[Unit] = {
    val WholeShape = "([+-]?)(\\d+(,\\d{3})*?)[.]?".r
    val FractionalShape = "([+-]?)(\\d*(,\\d{3})*?)[.](\\d+)".r
    (TextConstraint.filterNumberValue(value), maxFractional, mustBePositive) match {
      case (WholeShape(_, whole, _), _, _) if filterCommas(whole).size > maxWhole =>
        getError(fieldValue, s"must be at most $maxWhole digits")
      case (WholeShape("-", _, _), _, true) =>
        getError(fieldValue, "must be a positive number")
      case (WholeShape(_, _, _), _, _) => ().valid
      case (FractionalShape(_, whole, _, fractional), 0, _)
          if filterCommas(whole).size > maxWhole && fractional.size > 0 =>
        getError(fieldValue, s"must be at most $maxWhole whole digits and no decimal fraction")
      case (FractionalShape(_, whole, _, fractional), _, _)
          if filterCommas(whole).size > maxWhole && fractional.size > maxFractional =>
        getError(
          fieldValue,
          s"must be at most $maxWhole whole digits and decimal fraction must be at most $maxFractional digits")
      case (FractionalShape(_, whole, _, _), _, _) if filterCommas(whole).size > maxWhole =>
        getError(fieldValue, s"must be at most $maxWhole whole digits")
      case (FractionalShape(_, _, _, fractional), 0, _) if fractional.size > 0 =>
        getError(fieldValue, "must be a whole number")
      case (FractionalShape(_, _, _, fractional), _, _) if fractional.size > maxFractional =>
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

  private def validateRequired(fieldValue: FormComponent, fieldId: FormComponentId, errorPrefix: Option[String] = None)(
    xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil =>
        Map(fieldId -> errors(fieldValue, s"${errorPrefix.getOrElse("")} must be entered")).invalid
      case value :: Nil  => ().valid
      case value :: rest => ().valid // we don't support multiple values yet
    }

  private def validateForbidden(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil => ().valid
      case value :: Nil =>
        Map(fieldId -> errors(fieldValue, "must not be entered")).invalid
      case value :: rest =>
        Map(fieldId -> errors(fieldValue, "must not be entered")).invalid // we don't support multiple values yet
    }

  private def addressLineValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] = {
    val Fourth = "[4]$".r.unanchored
    (xs.filterNot(_.isEmpty()), fieldId.value) match {
      case (Nil, _) => ().valid
      case (value :: Nil, Fourth()) if value.length > ValidationValues.addressLine4 =>
        Map(fieldId -> errors(fieldValue, s"line 4 is longer than ${ValidationValues.addressLine4} characters")).invalid
      case (value :: Nil, _) if value.length > ValidationValues.addressLine =>
        Map(
          fieldId -> errors(
            fieldValue,
            s"line ${fieldId.value.takeRight(1)} is longer than ${ValidationValues.addressLine} characters")).invalid
      case _ => ().valid
    }
  }

  private def postcodeValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > ValidationValues.postcodeLimit =>
        Map(fieldId -> errors(fieldValue, s"postcode is longer than ${ValidationValues.postcodeLimit} characters")).invalid
      case _ => ().valid
    }

  private def validateChoice(fieldValue: FormComponent)(data: FormDataRecalculated): ValidatedType[Unit] = {
    val choiceValue = data.data.get(fieldValue.id).toList.flatten.headOption

    (fieldValue.mandatory, choiceValue) match {
      case (true, None | Some("")) =>
        getError(fieldValue, "must be selected")
      case _ => ().valid
    }
  }

  def validateRF(fieldValue: FormComponent, value: String) =
    validateRequired(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateFF(fieldValue: FormComponent, value: String) =
    validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateAddress(fieldValue: FormComponent, address: Address)(data: FormDataRecalculated): ValidatedType[Unit] = {
    val addressValueOf: String => Seq[String] = suffix => data.data.get(fieldValue.id.withSuffix(suffix)).toList.flatten

    def validateRequiredField(value: String, errorPrefix: String) =
      validateRequired(fieldValue, fieldValue.id.withSuffix(value), Some(errorPrefix)) _

    def validateForbiddenField(value: String) = validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

    def lengthValidation(value: String) = addressLineValidation(fieldValue, fieldValue.id.withSuffix(value)) _

    def postcodeLengthValidation(value: String) = postcodeValidation(fieldValue, fieldValue.id.withSuffix(value)) _

    val validatedResult: List[ValidatedType[Unit]] = addressValueOf("uk") match {
      case "true" :: Nil =>
        List(
          Monoid[ValidatedType[Unit]].combine(
            validateRequiredField("street1", localisation("line 1"))(addressValueOf("street1")),
            lengthValidation("street1")(addressValueOf("street1"))
          ),
          lengthValidation("street2")(addressValueOf("street2")),
          lengthValidation("street3")(addressValueOf("street3")),
          lengthValidation("street4")(addressValueOf("street4")),
          validateRequiredField("postcode", localisation("postcode"))(addressValueOf("postcode")),
          validateForbiddenField("country")(addressValueOf("country")),
          postcodeLengthValidation("postcode")(addressValueOf("postcode"))
        )
      case _ =>
        List(
          Monoid[ValidatedType[Unit]].combine(
            validateRequiredField("street1", localisation("line 1"))(addressValueOf("street1")),
            lengthValidation("street1")(addressValueOf("street1"))),
          lengthValidation("street2")(addressValueOf("street2")),
          lengthValidation("street3")(addressValueOf("street3")),
          lengthValidation("street4")(addressValueOf("street4")),
          validateForbiddenField("postcode")(addressValueOf("postcode")),
          validateRequiredField("country", localisation("Country"))(addressValueOf("country"))
        )
    }

    Monoid[ValidatedType[Unit]].combineAll(validatedResult)
  }

  def validateToday(fieldValue: FormComponent, localDate: LocalDate, offset: OffsetDate, dateError: GformError)(
    func: (LocalDate, OffsetDate) => Boolean): ValidatedType[Unit] =
    func(localDate, offset) match {
      case true  => ().valid
      case false => dateError.invalid
    }

  def validateConcreteDate(
    fieldValue: FormComponent,
    localDate: LocalDate,
    concreteDate: ConcreteDate,
    offset: OffsetDate = OffsetDate(0),
    dateError: GformError)(func: (LocalDate, ConcreteDate, OffsetDate) => Boolean): ValidatedType[Unit] =
    func(localDate, concreteDate, offset) match {
      case true  => ().valid
      case false => dateError.invalid
    }

  def concreteDateFunctionMatch(beforeAfterPrecisely: BeforeAfterPrecisely)(
    date: LocalDate,
    concreteDate: ConcreteDate,
    offset: OffsetDate): Boolean =
    beforeAfterPrecisely match {
      case Before if concreteDate.isExact => isBeforeExactConcreteDate(date, concreteDate, offset)

      case After if concreteDate.isExact => isAfterExactConcreteDate(date, concreteDate, offset)

      case Precisely => preciselyFunctionMatch(date, concreteDate, offset)

    }

  import cats.instances.int._
  import cats.syntax.eq._
  def preciselyFunctionMatch(date: LocalDate, concreteDate: ConcreteDate, offset: OffsetDate): Boolean = {
    val parametersLength = concreteDate.getNumericParameters.length
    if (concreteDate.isExact) {
      isPreciselyExactConcreteDate(date, concreteDate, offset)
    } else {
      concreteDate match {
        case concreteDate: ConcreteDate if concreteDate.day === FirstDay || concreteDate.day === LastDay =>
          isFirstOrLastDay(date, concreteDate)
        case concreteDate: ConcreteDate if concreteDate.year === Next || concreteDate.year === Previous =>
          isNextOrPreviousYear(date, concreteDate)
        case concreteDate: ConcreteDate if parametersLength === 1 || parametersLength === 2 =>
          isSameAbstractDate(date, concreteDate)
        case _ => true
      }
    }
  }

  def todayFunctionMatch(beforeAfterPrecisely: BeforeAfterPrecisely)(date: LocalDate, offset: OffsetDate): Boolean =
    beforeAfterPrecisely match {
      case Before    => isBeforeToday(date, offset)
      case After     => isAfterToday(date, offset)
      case Precisely => isPreciselyToday(date, offset)
    }

  def isAfterToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isAfter(now.apply().plusDays(offset.value.toLong))

  def isAfterExactConcreteDate(date: LocalDate, concreteDay: ConcreteDate, offset: OffsetDate): Boolean =
    date.isAfter(exactConcreteDateToLocalDate(concreteDay).plusDays(offset.value.toLong))

  def isBeforeToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isBefore(now.apply().plusDays(offset.value.toLong))

  def isBeforeExactConcreteDate(date: LocalDate, concreteDay: ConcreteDate, offset: OffsetDate): Boolean =
    date.isBefore(exactConcreteDateToLocalDate(concreteDay).plusDays(offset.value.toLong))

  def isPreciselyToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isEqual(now.apply().plusDays(offset.value.toLong))

  def isPreciselyExactConcreteDate(date: LocalDate, concreteDay: ConcreteDate, offset: OffsetDate): Boolean =
    date.isEqual(exactConcreteDateToLocalDate(concreteDay).plusDays(offset.value.toLong))

  def isSameAbstractDate(date: LocalDate, concreteDay: ConcreteDate): Boolean =
    concreteDay.getNumericParameters
      .map {
        case ExactYear(year)   => date.getYear === year
        case ExactMonth(month) => date.getMonthValue === month
        case ExactDay(day)     => date.getDayOfMonth === day
      }
      .forall(identity)

  def isFirstOrLastDay(date: LocalDate, concreteDay: ConcreteDate): Boolean = concreteDay.day match {
    case FirstDay => date.getDayOfMonth === 1
    case LastDay =>
      LocalDate.of(date.getYear, date.getMonthValue, date.getDayOfMonth).lengthOfMonth() === date.getDayOfMonth
  }

  def isNextOrPreviousYear(date: LocalDate, concreteDay: ConcreteDate): Boolean = {
    val areMonthAndDayEqual = isSameAbstractDate(date: LocalDate, concreteDay: ConcreteDate)
    concreteDay.year match {
      case Next     => date.getYear === getNextYear && areMonthAndDayEqual
      case Previous => date.getYear === getPreviousYear && areMonthAndDayEqual
    }
  }

  def tryConcreteDateAsLocalDate(concreteDate: ConcreteDate): Try[LocalDate] =
    concreteDate match {
      case ConcreteDate(year: ExactParameter, ExactMonth(month), day: ExactParameter) =>
        Try(LocalDate.of(getYear(year), month, getDay(getYear(year), month, day)))
    }

  def validateInputDate(
    formComponent: FormComponent,
    formComponentId: FormComponentId,
    errorMsg: Option[String],
    data: FormDataRecalculated,
    otherFormComponent: Option[FormComponent] = None): ValidatedLocalDate = {
    val fieldIdList = Date.fields(formComponentId).map(fId => data.data.get(fId)).toList

    fieldIdList match {
      case Some(day +: Nil) :: Some(month +: Nil) :: Some(year +: Nil) :: Nil =>
        validateLocalDate(formComponent, formComponentId, otherFormComponent, errorMsg, day, month, year) match {
          case Valid(ConcreteDate(ExactYear(concYear), ExactMonth(concMonth), ExactDay(concDay))) =>
            Try(LocalDate.of(concYear, concMonth, concDay)) match {
              case Success(date) => Valid(date)
              case Failure(ex)   => Map(formComponentId -> errors(formComponent, "must be a valid date")).invalid
            }
          case Invalid(nonEmptyList) =>
            Invalid(nonEmptyList)
        }

      case _ =>
        getError(formComponent, "is missing")
    }
  }

  def validateLocalDate(
    formComponent: FormComponent,
    formComponentId: FormComponentId,
    otherFormComponent: Option[FormComponent],
    errorMessage: Option[String],
    day: String,
    month: String,
    year: String): ValidatedConcreteDate = {

    val dayLabel = messagePrefix(formComponent, formComponentId, otherFormComponent) + " " + localisation("day")
    val monthLabel = messagePrefix(formComponent, formComponentId, otherFormComponent) + " " + localisation("month")
    val yearLabel = messagePrefix(formComponent, formComponentId, otherFormComponent) + " " + localisation("year")

    val d = isNumeric(day, dayLabel)
      .andThen(y => isWithinBounds(y, 31, dayLabel))
      .leftMap(er => Map(formComponentId.withSuffix("day") -> Set(errorMessage.getOrElse(er))))
    val m = isNumeric(month, monthLabel)
      .andThen(y => isWithinBounds(y, 12, monthLabel))
      .leftMap(er => Map(formComponentId.withSuffix("month") -> Set(errorMessage.getOrElse(er))))
    val y = isNumeric(year, yearLabel)
      .andThen(y => hasValidNumberOfDigits(y, 4, yearLabel))
      .leftMap(er => Map(formComponentId.withSuffix("year") -> Set(errorMessage.getOrElse(er))))

    parallelWithApplicative(d, m, y)(ConcreteDate.apply)
  }

  def isNumeric(str: String, label: String): ValidatedNumeric =
    Try(str.toInt) match {
      case Success(x) => Valid(x)
      case Failure(_) => Invalid(s"$label must be numeric")
    }

  def isWithinBounds(number: Int, dayOrMonth: Int, label: String): ValidatedNumeric =
    number match {
      case x if number <= dayOrMonth => Valid(number)
      case y if number > dayOrMonth  => Invalid(s"$label must not be greater than $dayOrMonth")
    }

  def hasValidNumberOfDigits(number: Int, digits: Int, label: String): ValidatedNumeric =
    number.toString.length match {
      case x if x == digits => Valid(number)
      case y if y != digits => Invalid(s"$label must be a $digits digit number")
    }

  def parallelWithApplicative[E: Semigroup](v1: Validated[E, Int], v2: Validated[E, Int], v3: Validated[E, Int])(
    f: (Int, Int, Int) => ConcreteDate): Validated[E, ConcreteDate] = (v3, v2, v1).mapN(f)

  def alwaysOk(fieldValue: FormComponent)(xs: Seq[String]): FormFieldValidationResult =
    xs match {
      case Nil           => FieldOk(fieldValue, "")
      case value :: rest => FieldOk(fieldValue, value) // we don't support multiple values yet
    }

  private def errors(fieldValue: FormComponent, defaultErr: String): Set[String] =
    Set(
      localisation(
        fieldValue.errorMessage.getOrElse(
          messagePrefix(fieldValue, fieldValue.id, None) + " " + localisation(defaultErr))))

  private def getError(fieldValue: FormComponent, defaultMessage: String) =
    Map(fieldValue.id -> errors(fieldValue, localisation(defaultMessage))).invalid
}

object ValidationValues {

  val phoneDigits = (4, 30)
  val sortCodeLength = 2
  val bankAccountLength = 8
  val sterlingLength = 11
  val addressLine = 35
  val addressLine4 = 27
  val emailLimit = 241
  val postcodeLimit = 8
}
