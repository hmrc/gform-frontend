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
import java.time.format.DateTimeFormatter

import cats.{ Monoid, Semigroup }
import cats.data.Validated.{ Invalid, Valid }
import cats.data._
import cats.implicits._
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.emailaddress.EmailAddress
import uk.gov.hmrc.gform.fileupload._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import ValidationUtil.{ ValidatedType, _ }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormDataRecalculated }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ BooleanExprEval, UkSortCode, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.typeclasses.Now
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.views.html._

//TODO: this validation must be performed on gform-backend site. Or else we will not able provide API for 3rd party services

class ValidationService(
  fileUploadService: FileUploadService,
  gformConnector: GformConnector,
  booleanExpr: BooleanExprEval[Future]
) {

  private def validateFieldValue(
    fieldValue: FormComponent,
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): Future[ValidatedType] =
    new ComponentsValidator(data, fileUploadService, envelopeId, retrievals, booleanExpr, formTemplate)
      .validate(fieldValue)

  def validateComponents(
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(implicit hc: HeaderCarrier): Future[ValidatedType] =
    fieldValues
      .traverse(fv => validateFieldValue(fv, data, envelopeId, retrievals, formTemplate))
      .map(Monoid[ValidatedType].combineAll)

  private def validateUsingValidators(section: Section, data: FormDataRecalculated)(
    implicit hc: HeaderCarrier): Future[ValidatedType] =
    section.validators
      .map(validateUsingSectionValidators(_, data))
      .getOrElse(().valid.pure[Future])

  def validateForm(
    sectionFields: List[FormComponent],
    section: Section,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    formTemplate: FormTemplate)(data: FormDataRecalculated)(implicit hc: HeaderCarrier): Future[ValidatedType] = {
    val eT = for {
      _ <- EitherT(validateComponents(sectionFields, data, envelopeId, retrievals, formTemplate).map(_.toEither))
      _ <- EitherT(validateUsingValidators(section, data).map(_.toEither))
    } yield ()
    eT.value.map(Validated.fromEither)
  }

  def evaluateValidation(
    v: ValidatedType,
    fields: List[FormComponent],
    data: FormDataRecalculated,
    envelope: Envelope): List[(FormComponent, FormFieldValidationResult)] =
    // We need to keep the formComponent order as they appear on the form for page-level-error rendering, do not convert to map
    ValidationUtil
      .evaluateValidationResult(fields, v, data, envelope)
      .map(v => v.fieldValue -> v)

  private def validateUsingSectionValidators(v: Validator, data: FormDataRecalculated)(
    implicit hc: HeaderCarrier): Future[ValidatedType] = {
    def dataGetter(fieldId: FormComponentId): String =
      data.data.get(fieldId).toList.flatten.headOption.getOrElse("")

    def getValidated(is: Boolean, errors: Map[FormComponentId, Set[String]]) =
      if (is) ().valid else errors.invalid

    v match {
      case HMRCUTRPostcodeCheckValidator(errorMessage, utr, postcode) =>
        gformConnector
          .validatePostCodeUtr(
            data.data.get(FormComponentId(utr.value)).toList.flatten.headOption.getOrElse(""),
            data.data.get(FormComponentId(postcode.value)).toList.flatten.headOption.getOrElse("")
          )
          .map(getValidated(_, Map(utr.toFieldId -> Set(errorMessage), postcode.toFieldId -> Set(errorMessage))))
      case BankAccoutnModulusCheck(errorMessage, accountNumber, sortCode) =>
        val sortCodeCombined = UkSortCode.fields(sortCode.toFieldId).map(dataGetter).mkString("-")
        gformConnector
          .validateBankModulus(dataGetter(accountNumber.toFieldId), sortCodeCombined)
          .map(
            getValidated(_, Map(accountNumber.toFieldId -> Set(errorMessage), sortCode.toFieldId -> Set(errorMessage))))
    }
  }
}

class ComponentsValidator(
  data: FormDataRecalculated,
  fileUploadService: FileUploadService,
  envelopeId: EnvelopeId,
  retrievals: MaterialisedRetrievals,
  booleanExpr: BooleanExprEval[Future],
  formTemplate: FormTemplate) {

  def validate(fieldValue: FormComponent)(implicit hc: HeaderCarrier): Future[ValidatedType] = {

    def validIf(validationResult: ValidatedType): Future[ValidatedType] =
      (validationResult.isValid, fieldValue.validIf) match {
        case (true, Some(vi)) =>
          booleanExpr.isTrue(vi.expr, data.data, retrievals, data.invisible, formTemplate).map {
            case false => getError(fieldValue, "must be entered")
            case true  => validationResult
          }
        case _ => validationResult.pure[Future]
      }

    fieldValue.`type` match {
      case sortCode @ UkSortCode(_)      => validIf(validateSortCode(fieldValue, sortCode, fieldValue.mandatory)(data))
      case date @ Date(_, _, _)          => validIf(validateDate(fieldValue, date))
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

  private def validateDate(fieldValue: FormComponent, date: Date): ValidatedType = {
    val reqFieldValidResult = validateDateRequiredField(fieldValue)
    val otherRulesValidResult = validateDateImpl(fieldValue, date)(data)
    Monoid[ValidatedType].combineAll(List(reqFieldValidResult, otherRulesValidResult))
  }

  private lazy val dataGetter: FormComponent => String => Seq[String] = fv =>
    suffix => data.data.get(fv.id.withSuffix(suffix)).toList.flatten
  private def validateDateRequiredField(fieldValue: FormComponent): ValidatedType = {
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
    Monoid[ValidatedType].combineAll(validatedResult)
  }

  private def messagePrefix(fieldValue: FormComponent) =
    localisation(fieldValue.shortName.getOrElse(fieldValue.label))

  private def validateDateImpl(fieldValue: FormComponent, date: Date)(data: FormDataRecalculated): ValidatedType = {
    val govDateFormat = DateTimeFormatter.ofPattern("dd MMMM yyyy")
    val dateWithOffset = (localDate: LocalDate, offset: OffsetDate) =>
      localDate.plusDays(offset.value.toLong).format(govDateFormat)
    date.constraintType match {
      case AnyDate =>
        validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data).andThen(lDate => ().valid)
      case DateConstraints(dateConstraintList) =>
        val result = dateConstraintList.map {
          case DateConstraint(beforeOrAfterOrPrecisely, dateConstrInfo, offsetDate) =>
            (beforeOrAfterOrPrecisely, dateConstrInfo, offsetDate) match {

              case (Before, Today, offset) =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(
                    inputDate =>
                      validateToday(
                        fieldValue,
                        inputDate,
                        offset,
                        Map(fieldValue.id -> errors(fieldValue, "should be before Today")))(isBeforeToday))

              case (Before, concreteDate: ConcreteDate, offset) =>
                validateConcreteDate(concreteDate, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { concreteDate =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateConcreteDate(
                            fieldValue,
                            inputDate,
                            concreteDate,
                            offset,
                            Map(fieldValue.id ->
                              errors(fieldValue, s"should be before ${dateWithOffset(concreteDate, offset)}"))
                          )(isBeforeConcreteDate))
                  }

              case (Before, firstDay: FirstDay, offset) =>
                validateFirstOrLastDay(firstDay, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { firstDay =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateFirstOrLastDay(
                            fieldValue,
                            inputDate,
                            firstDay,
                            offset,
                            Map(fieldValue.id ->
                              errors(
                                fieldValue,
                                s"must be before the first day of the month, for example: ${dateWithOffset(firstDay, offset)}"))
                          )(isBeforeFirstDay))
                  }

              case (Before, lastDay: LastDay, offset) =>
                validateFirstOrLastDay(lastDay, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { lastDay =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateFirstOrLastDay(
                            fieldValue,
                            inputDate,
                            lastDay,
                            offset,
                            Map(fieldValue.id ->
                              errors(
                                fieldValue,
                                s"must be before the last day of the month, for example: ${dateWithOffset(lastDay, offset)}"))
                          )(isBeforeLastDay))
                  }

              case (beforeOrAfterOrPrecisely @ _, DateField(fieldId), offset) => {
                lazy val validateOtherDate = validateInputDate(fieldValue, fieldId, None, data)

                lazy val validatedThisDate = validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)

                val beforeOrAfterOrPreciselyString = beforeOrAfterOrPrecisely match {
                  case After     => localisation("after")
                  case Before    => localisation("before")
                  case Precisely => localisation("precisely")
                }

                val beforeOrAfterOrPreciselyFunction = beforeOrAfterOrPrecisely match {
                  case After     => isAfterConcreteDate _
                  case Before    => isBeforeConcreteDate _
                  case Precisely => isPreciselyConcreteDate _
                }

                validateOtherDate.andThen { otherLocalDate =>
                  validatedThisDate.andThen { thisLocalDate =>
                    validateConcreteDate(
                      fieldValue,
                      thisLocalDate,
                      otherLocalDate,
                      offset,
                      Map(
                        fieldValue.id ->
                          errors(
                            fieldValue,
                            s"should be $beforeOrAfterOrPreciselyString ${dateWithOffset(otherLocalDate, offset)}"))
                    )(beforeOrAfterOrPreciselyFunction)
                  }
                }
              }

              case (After, Today, offset) =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(
                    inputDate =>
                      validateToday(
                        fieldValue,
                        inputDate,
                        offset,
                        Map(fieldValue.id -> errors(fieldValue, "should be after today")))(isAfterToday))

              case (After, concreteDate: ConcreteDate, offset) =>
                validateConcreteDate(concreteDate, Map(fieldValue.id -> errors(fieldValue, "must be a valid date")))
                  .andThen { concreteDate =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateConcreteDate(
                            fieldValue,
                            inputDate,
                            concreteDate,
                            offset,
                            Map(
                              fieldValue.id -> errors(
                                fieldValue,
                                s"should be after ${dateWithOffset(concreteDate, offset)}"))
                          )(isAfterConcreteDate))
                  }

              case (After, firstDay: FirstDay, offset) =>
                validateFirstOrLastDay(firstDay, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { firstDay =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateFirstOrLastDay(
                            fieldValue,
                            inputDate,
                            firstDay,
                            offset,
                            Map(fieldValue.id ->
                              errors(
                                fieldValue,
                                s"must be after the first day of the month, for example: ${dateWithOffset(firstDay, offset)}"))
                          )(isAfterFirstDay))
                  }

              case (After, lastDay: LastDay, offset) =>
                validateFirstOrLastDay(lastDay, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { lastDay =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateFirstOrLastDay(
                            fieldValue,
                            inputDate,
                            lastDay,
                            offset,
                            Map(fieldValue.id ->
                              errors(
                                fieldValue,
                                s"must be after the last day of the month, for example: ${dateWithOffset(lastDay, offset)}"))
                          )(isAfterLastDay))
                  }

              case (Precisely, firstDay: FirstDay, offset) if firstDay == FirstDay("YYYY", "MM") =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(
                    inputDate =>
                      validateFirstOrLastDay(
                        fieldValue,
                        inputDate,
                        firstDay,
                        offset,
                        Map(
                          fieldValue.id ->
                            errors(
                              fieldValue,
                              s"must be the first day of the month, for example: " +
                                s"${dateWithOffset(LocalDate.of(inputDate.getYear, inputDate.getMonthValue, FirstDay(inputDate.getYear.toString, inputDate.getMonthValue.toString).day), offset)}"
                            ))
                      )(isFirstDay))

              case (Precisely, lastDay: LastDay, offset) if lastDay == LastDay("YYYY", "MM") =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(
                    inputDate =>
                      validateFirstOrLastDay(
                        fieldValue,
                        inputDate,
                        lastDay,
                        offset,
                        Map(
                          fieldValue.id ->
                            errors(
                              fieldValue,
                              s"must be the last day of the month, for example: " +
                                s"${dateWithOffset(LocalDate.of(inputDate.getYear, inputDate.getMonthValue, LastDay(inputDate.getYear.toString, inputDate.getMonthValue.toString).day.get), offset)}"
                            ))
                      )(isLastDay))

              case (Precisely, concreteDate: ConcreteDate, offset) =>
                validateConcreteDate(concreteDate, Map(fieldValue.id -> errors(fieldValue, "must be a valid date")))
                  .andThen { concreteDate =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateConcreteDate(
                            fieldValue,
                            inputDate,
                            concreteDate,
                            offset,
                            Map(
                              fieldValue.id -> errors(
                                fieldValue,
                                s"must be precisely ${dateWithOffset(concreteDate, offset)}"))
                          )(isPreciselyConcreteDate))
                  }

              case (Precisely, Today, offset) =>
                validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                  .andThen(
                    inputDate =>
                      validateToday(
                        fieldValue,
                        inputDate,
                        offset,
                        Map(fieldValue.id -> errors(fieldValue, "must be precisely today")))(isPreciselyToday))

              case (Precisely, concreteDate: ConcreteDate, offset) =>
                validateConcreteDate(concreteDate, Map(fieldValue.id -> errors(fieldValue, "must be a valid date")))
                  .andThen { concreteDate =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateConcreteDate(
                            fieldValue,
                            inputDate,
                            concreteDate,
                            offset,
                            Map(
                              fieldValue.id -> errors(
                                fieldValue,
                                s"must be precisely ${dateWithOffset(concreteDate, offset)}"))
                          )(isPreciselyConcreteDate))
                  }

              case (Precisely, firstDay: FirstDay, offset) =>
                validateFirstOrLastDay(firstDay, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { firstDay =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateFirstOrLastDay(
                            fieldValue,
                            inputDate,
                            firstDay,
                            offset,
                            Map(fieldValue.id ->
                              errors(
                                fieldValue,
                                s"must be the first day of the month, for example: ${dateWithOffset(firstDay, offset)}"))
                          )(isPreciselyFirstDay))
                  }

              case (Precisely, lastDay: LastDay, offset) => {
                println("findmedude" + lastDay)
                validateFirstOrLastDay(lastDay, Map(fieldValue.id -> errors(fieldValue, "is not a valid date")))
                  .andThen { lastDay =>
                    validateInputDate(fieldValue, fieldValue.id, fieldValue.errorMessage, data)
                      .andThen(
                        inputDate =>
                          validateFirstOrLastDay(
                            fieldValue,
                            inputDate,
                            lastDay,
                            offset,
                            Map(fieldValue.id ->
                              errors(
                                fieldValue,
                                s"must be the last day of the month, for example: ${dateWithOffset(lastDay, offset)}"))
                          )(isPreciselyLastDay))
                  }
              }

            }

        }
        Monoid[ValidatedType].combineAll(result)
    }
  }

  //TODO: this will be called many times per one form. Maybe there is a way to optimise it?
  private def validateFileUpload(data: FormDataRecalculated, fieldValue: FormComponent)(
    implicit hc: HeaderCarrier): Future[ValidatedType] =
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
    data: FormDataRecalculated): ValidatedType = {
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
    val ValidCRN = "^[A-Z]{2}[0-9]{12}|[A-Z]{2}[0-9]{15}$".r
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
    Monoid[ValidatedType].combineAll(
      UkSortCode
        .fields(fieldValue.id)
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
    mustBePositive: Boolean): ValidatedType = {
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
    xs: Seq[String]): ValidatedType =
    xs.filterNot(_.isEmpty()) match {
      case Nil =>
        Map(fieldId -> errors(fieldValue, s"${errorPrefix.getOrElse("")} must be entered")).invalid
      case value :: Nil  => ().valid
      case value :: rest => ().valid // we don't support multiple values yet
    }

  private def validateForbidden(fieldValue: FormComponent, fieldId: FormComponentId)(xs: Seq[String]): ValidatedType =
    xs.filterNot(_.isEmpty()) match {
      case Nil => ().valid
      case value :: Nil =>
        Map(fieldId -> errors(fieldValue, "must not be entered")).invalid
      case value :: rest =>
        Map(fieldId -> errors(fieldValue, "must not be entered")).invalid // we don't support multiple values yet
    }

  private def addressLineValidation(fieldValue: FormComponent, fieldId: FormComponentId)(
    xs: Seq[String]): ValidatedType = {
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

  private def postcodeValidation(fieldValue: FormComponent, fieldId: FormComponentId)(xs: Seq[String]): ValidatedType =
    xs.filterNot(_.isEmpty) match {
      case value :: Nil if value.length > ValidationValues.postcodeLimit =>
        Map(fieldId -> errors(fieldValue, s"postcode is longer than ${ValidationValues.postcodeLimit} characters")).invalid
      case _ => ().valid
    }

  private def validateChoice(fieldValue: FormComponent)(data: FormDataRecalculated): ValidatedType = {
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

  def validateAddress(fieldValue: FormComponent, address: Address)(data: FormDataRecalculated): ValidatedType = {
    val addressValueOf: String => Seq[String] = suffix => data.data.get(fieldValue.id.withSuffix(suffix)).toList.flatten

    def validateRequiredField(value: String, errorPrefix: String) =
      validateRequired(fieldValue, fieldValue.id.withSuffix(value), Some(errorPrefix)) _

    def validateForbiddenField(value: String) = validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

    def lengthValidation(value: String) = addressLineValidation(fieldValue, fieldValue.id.withSuffix(value)) _

    def postcodeLengthValidation(value: String) = postcodeValidation(fieldValue, fieldValue.id.withSuffix(value)) _

    val validatedResult: List[ValidatedType] = addressValueOf("uk") match {
      case "true" :: Nil =>
        List(
          Monoid[ValidatedType].combine(
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
          Monoid[ValidatedType].combine(
            validateRequiredField("street1", localisation("line 1"))(addressValueOf("street1")),
            lengthValidation("street1")(addressValueOf("street1"))),
          lengthValidation("street2")(addressValueOf("street2")),
          lengthValidation("street3")(addressValueOf("street3")),
          lengthValidation("street4")(addressValueOf("street4")),
          validateForbiddenField("postcode")(addressValueOf("postcode")),
          validateRequiredField("country", localisation("Country"))(addressValueOf("country"))
        )
    }

    Monoid[ValidatedType].combineAll(validatedResult)
  }

  def validateToday(fieldValue: FormComponent, localDate: LocalDate, offset: OffsetDate, dateError: GformError)(
    func: (LocalDate, OffsetDate) => Boolean): ValidatedType =
    func(localDate, offset) match {
      case true  => ().valid
      case false => dateError.invalid
    }

  def validateConcreteDate(
    fieldValue: FormComponent,
    localDate: LocalDate,
    concreteDate: LocalDate,
    offset: OffsetDate,
    dateError: GformError)(func: (LocalDate, LocalDate, OffsetDate) => Boolean): ValidatedType =
    func(localDate, concreteDate, offset) match {
      case true  => ().valid
      case false => dateError.invalid
    }

  def validateFirstOrLastDay[T](
    fieldValue: FormComponent,
    localDate: LocalDate,
    firstOrLastDay: T,
    offset: OffsetDate,
    dateError: GformError)(func: (LocalDate, T, OffsetDate) => Boolean): ValidatedType =
    func(localDate, firstOrLastDay, offset) match {
      case true  => ().valid
      case false => dateError.invalid
    }

  def isAfterToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isAfter(now.apply().plusDays(offset.value.toLong))

  def isAfterConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean =
    date.isAfter(concreteDay.plusDays(offset.value.toLong))

  def isAfterFirstDay(date: LocalDate, firstDay: LocalDate, offset: OffsetDate): Boolean =
    date.isAfter(
      LocalDate.of(firstDay.getYear, firstDay.getMonthValue, firstDay.getDayOfMonth).plusDays(offset.value.toLong))

  def isAfterLastDay(date: LocalDate, lastDay: LocalDate, offset: OffsetDate): Boolean =
    date.isAfter(
      LocalDate.of(lastDay.getYear, lastDay.getMonthValue, lastDay.getDayOfMonth).plusDays(offset.value.toLong))

  def isBeforeToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isBefore(now.apply().plusDays(offset.value.toLong))

  def isBeforeConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean =
    date.isBefore(concreteDay.plusDays(offset.value.toLong))

  def isBeforeFirstDay(date: LocalDate, firstDay: LocalDate, offset: OffsetDate): Boolean =
    date.isBefore(
      LocalDate.of(firstDay.getYear, firstDay.getMonthValue, firstDay.getDayOfMonth).plusDays(offset.value.toLong))

  def isBeforeLastDay(date: LocalDate, lastDay: LocalDate, offset: OffsetDate): Boolean =
    date.isBefore(
      LocalDate.of(lastDay.getYear, lastDay.getMonthValue, lastDay.getDayOfMonth).plusDays(offset.value.toLong))

  def isPreciselyToday(date: LocalDate, offset: OffsetDate)(implicit now: Now[LocalDate]): Boolean =
    date.isEqual(now.apply().plusDays(offset.value.toLong))

  def isPreciselyConcreteDate(date: LocalDate, concreteDay: LocalDate, offset: OffsetDate): Boolean =
    date.isEqual(concreteDay.plusDays(offset.value.toLong))

  def isPreciselyFirstDay(date: LocalDate, firstDay: LocalDate, offset: OffsetDate): Boolean =
    date.isEqual(
      LocalDate.of(firstDay.getYear, firstDay.getMonthValue, firstDay.getDayOfMonth).plusDays(offset.value.toLong))

  def isPreciselyLastDay(date: LocalDate, lastDay: LocalDate, offset: OffsetDate): Boolean =
    date.isEqual(
      LocalDate.of(lastDay.getYear, lastDay.getMonthValue, lastDay.getDayOfMonth).plusDays(offset.value.toLong))

  def isLastDay(date: LocalDate, lastDay: LastDay, offset: OffsetDate): Boolean =
    date.getDayOfMonth == LastDay(date.getYear.toString, date.getMonthValue.toString).day.get

  def isFirstDay(date: LocalDate, firstDay: FirstDay, offset: OffsetDate): Boolean =
    date.getDayOfMonth == 1

  def validateConcreteDate(concreteDate: ConcreteDate, dateError: GformError): ValidatedLocalDate =
    Try(LocalDate.of(concreteDate.year, concreteDate.month, concreteDate.day)) match {
      case Success(date) => Valid(date)
      case Failure(ex)   => dateError.invalid
    }

  def validateFirstOrLastDay[T](firstOrLastDay: T, dateError: GformError): ValidatedLocalDate = {

    val day = firstOrLastDay match {
      case FirstDay(year, month) => Try(LocalDate.of(year.toInt, month.toInt, FirstDay(year, month).day))
      case LastDay(year, month) => Try(LocalDate.of(year.toInt, month.toInt, LastDay(year, month).day.get))
    }

    day match {
      case Success(date: LocalDate) => Valid(date)
      case Failure(ex)              => dateError.invalid
    }
  }

  def validateInputDate(
    fieldValue: FormComponent,
    fieldId: FormComponentId,
    errorMsg: Option[String],
    data: FormDataRecalculated): ValidatedLocalDate = {
    val fieldIdList = Date.fields(fieldId).map(fId => data.data.get(fId))

    fieldIdList match {
      case Some(day +: Nil) :: Some(month +: Nil) :: Some(year +: Nil) :: Nil =>
        validateLocalDate(fieldValue, errorMsg, day, month, year) match {
          case Valid(concreteDate) =>
            validateConcreteDate(concreteDate, Map(fieldId -> errors(fieldValue, "must be a valid date")))
          case Invalid(nonEmptyList) => Invalid(nonEmptyList)
        }

      case _ =>
        getError(fieldValue, "is missing")
    }
  }

  def validateLocalDate(
    fieldValue: FormComponent,
    errorMessage: Option[String],
    day: String,
    month: String,
    year: String): ValidatedConcreteDate = {

    val dayLabel = messagePrefix(fieldValue) + " " + localisation("day")
    val monthLabel = messagePrefix(fieldValue) + " " + localisation("month")
    val yearLabel = messagePrefix(fieldValue) + " " + localisation("year")

    val d = isNumeric(day, dayLabel)
      .andThen(y => isWithinBounds(y, 31, dayLabel))
      .leftMap(er => Map(fieldValue.id.withSuffix("day") -> Set(errorMessage.getOrElse(er))))
    val m = isNumeric(month, monthLabel)
      .andThen(y => isWithinBounds(y, 12, monthLabel))
      .leftMap(er => Map(fieldValue.id.withSuffix("month") -> Set(errorMessage.getOrElse(er))))
    val y = isNumeric(year, yearLabel)
      .andThen(y => hasValidNumberOfDigits(y, 4, yearLabel))
      .leftMap(er => Map(fieldValue.id.withSuffix("year") -> Set(errorMessage.getOrElse(er))))

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
    Set(localisation(fieldValue.errorMessage.getOrElse(messagePrefix(fieldValue) + " " + localisation(defaultErr))))

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
