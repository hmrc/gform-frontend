/*
 * Copyright 2020 HM Revenue & Customs
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

import cats.data._
import cats.implicits._
import cats.Monoid
import play.api.i18n.{ I18nSupport, Messages }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.fileupload._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.ExpandUtils.submittedFCs
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.sharedmodel.{ AccessCode, EmailVerifierService }
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse, InternationalAddress, UkAddress }
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.form.{ Validated => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, LangADT, NotFound, ServiceResponse }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ ExecutionContext, Future }

//TODO: this validation must be performed on gform-backend site. Or else we will not able provide API for 3rd party services

class ValidationService(
  fileUploadService: FileUploadAlgebra[Future],
  gformConnector: GformConnector,
  booleanExpr: BooleanExprEval[Future],
  lookupRegistry: LookupRegistry,
  recalculation: Recalculation[Future, Throwable],
  i18nSupport: I18nSupport
)(implicit ec: ExecutionContext) {

  private def validateFieldValue(
    fieldValue: FormComponent,
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher,
    maybeAccessCode: Option[AccessCode]
  )(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidatedType[Unit]] =
    new ComponentsValidator(
      data,
      envelopeId,
      envelope,
      retrievals,
      booleanExpr,
      thirdPartyData,
      formTemplate,
      lookupRegistry,
      maybeAccessCode
    ).validate(fieldValue, fieldValues, getEmailCodeFieldMatcher)

  def validateComponents(
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher,
    maybeAccessCode: Option[AccessCode]
  )(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Future[ValidatedType[Unit]] =
    fieldValues
      .traverse(
        fv =>
          validateFieldValue(
            fv,
            fieldValues,
            data,
            envelopeId,
            envelope,
            retrievals,
            thirdPartyData,
            formTemplate,
            getEmailCodeFieldMatcher,
            maybeAccessCode))
      .map(Monoid[ValidatedType[Unit]].combineAll)

  def validateComponentsWithCache(
    cache: AuthCacheWithForm,
    declarationData: FormDataRecalculated,
    envelope: Envelope,
    maybeAccessCode: Option[AccessCode])(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Future[ValidatedType[Unit]] = {
    val fieldValues: List[FormComponent] = cache.formTemplate.destinations match {
      case destinationList: DestinationList => Fields.flattenGroups(destinationList.declarationSection.fields)
      case _                                => Nil
    }
    fieldValues
      .traverse(
        fv =>
          validateFieldValue(
            fv,
            fieldValues,
            declarationData,
            cache.form.envelopeId,
            envelope,
            cache.retrievals,
            cache.form.thirdPartyData,
            cache.formTemplate,
            GetEmailCodeFieldMatcher.noop,
            maybeAccessCode
        ))
      .map(Monoid[ValidatedType[Unit]].combineAll)
  }

  private def sendVerificationEmails(
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    thirdPartyData: ThirdPartyData
  )(
    implicit
    hc: HeaderCarrier,
    messages: Messages
  ): Future[ValidatedType[Map[EmailFieldId, EmailAndCode]]] = {

    val emailFields: List[(EmailFieldId, EmailVerifierService)] = fieldValues.collect {
      case IsEmailVerifier(fcId, emailVerifiedBy) => (fcId, emailVerifiedBy.emailVerifierService)
    }

    def emailExist(formComponentId: EmailFieldId, email: String): Boolean =
      thirdPartyData.emailVerification.get(formComponentId).fold(false)(_.email === email)

    val emailAddressedToBeVerified: List[Option[(EmailFieldId, EmailAndCode, EmailVerifierService)]] =
      emailFields.map {
        case (ef, emailVerifierService) =>
          val maybeEmail = data.data.one(ef)

          maybeEmail.collect {
            case email if !emailExist(ef, email) =>
              (ef, EmailAndCode.emailVerificationCode(email), emailVerifierService)
          }
      }
    emailAddressedToBeVerified.flatten
      .traverse {
        case (emailFieldId, eac @ EmailAndCode(email, code), emailVerifierService) =>
          gformConnector
            .sendEmail(NotifierEmailAddress(email), ConfirmationCodeWithEmailService(code, emailVerifierService))
            .map(_ => (emailFieldId, eac))
      }
      .map(_.toMap.valid)
  }

  private def validateUsingValidators(section: Section, data: FormDataRecalculated)(
    implicit hc: HeaderCarrier,
    messages: Messages,
    sse: SmartStringEvaluator): Future[ValidatedType[ValidationResult]] =
    section.validators
      .map(validateUsingSectionValidators(_, data))
      .getOrElse(ValidationResult.empty.valid.pure[Future])

  def validateFormComponents(
    sectionFields: List[FormComponent],
    section: Section,
    envelopeId: EnvelopeId,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    data: FormDataRecalculated,
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher,
    maybeAccessCode: Option[AccessCode]
  )(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator): Future[ValidatedType[ValidationResult]] = {
    def lift[T](fv: Future[ValidatedType[T]]) = EitherT(fv.map(_.toEither))

    val eT = for {
      _ <- lift(
            validateComponents(
              sectionFields,
              data,
              envelopeId,
              envelope,
              retrievals,
              thirdPartyData,
              formTemplate,
              getEmailCodeFieldMatcher,
              maybeAccessCode))
      valRes                <- lift(validateUsingValidators(section, data))
      emailsForVerification <- lift(sendVerificationEmails(sectionFields, data, thirdPartyData))
    } yield {
      valRes.copy(emailVerification = emailsForVerification)
    }

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
    implicit hc: HeaderCarrier,
    messages: Messages,
    sse: SmartStringEvaluator): Future[ValidatedType[ValidationResult]] = {
    def dataGetter(fieldId: FormComponentId): String = data.data.oneOrElse(fieldId, "")

    def compare(postCode: String)(drr: DesRegistrationResponse): Boolean = {
      val maybePostalCode = drr.address match {
        case UkAddress(_, _, _, _, postalCode)               => postalCode
        case InternationalAddress(_, _, _, _, _, postalCode) => postalCode
      }
      maybePostalCode.fold(true)(_.replace(" ", "").equalsIgnoreCase(postCode.replace(" ", "")))
    }

    v match {
      case HmrcRosmRegistrationCheckValidator(errorMessage, regime, utr, postcode) =>
        def findByKey(key: String): String = data.data.oneOrElse(FormComponentId(key), "")

        val utrValue = findByKey(utr.value)
        val postcodeValue = findByKey(postcode.value)

        val errors = Map(utr.toFieldId -> Set(errorMessage.value), postcode.toFieldId -> Set(errorMessage.value))

        val desRegistrationRequest = DesRegistrationRequest(regime, false, false)

        gformConnector
          .validatePostCodeUtr(utrValue, desRegistrationRequest)
          .flatMap {
            case NotFound               => Future.successful(errors.invalid)
            case CannotRetrieveResponse => Future.failed(new Exception("Call to des registration has failed"))
            case ServiceResponse(drr) =>
              Future.successful(
                if (compare(postcodeValue)(drr)) ValidationResult(Some(drr), Map.empty).valid
                else errors.invalid
              )
          }
      case BankAccoutnModulusCheck(errorMessage, accountNumber, sortCode) =>
        val sortCodeCombined = UkSortCode.fields(sortCode.toFieldId).toList.map(dataGetter).mkString("-")
        val errors =
          Map(accountNumber.toFieldId -> Set(errorMessage.value), sortCode.toFieldId -> Set(errorMessage.value))
        gformConnector
          .validateBankModulus(dataGetter(accountNumber.toFieldId), sortCodeCombined)
          .map(b => if (b) ValidationResult.empty.valid else errors.invalid)
    }
  }

  def validateForm(
    cache: AuthCacheWithForm,
    envelope: Envelope,
    retrievals: MaterialisedRetrievals,
    maybeAccessCode: Option[AccessCode])(
    implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator)
    : Future[(ValidatedType[ValidationResult], Map[FormComponent, FormFieldValidationResult])] = {

    val dataRaw = cache.variadicFormData

    def filterSection(sections: List[Section], data: FormDataRecalculated): List[Section] =
      sections.filter(data.isVisible)

    for {
      data <- recalculation.recalculateFormData(
               dataRaw,
               cache.formTemplate,
               retrievals,
               cache.form.thirdPartyData,
               maybeAccessCode,
               cache.form.envelopeId
             )
      allSections = RepeatingComponentService.getAllSections(cache.formTemplate, data)
      sections = filterSection(allSections, data)
      allFields = sections.flatMap(_.expandSectionRc(data.data).allFCs)
      allRequiredFields = allFields.filter(_.mandatory)
      allSubmittedFields = submittedFCs(data, allFields)
      allFieldsToValidate = (allRequiredFields.toSet ++ allSubmittedFields.toSet).toList
      v1 <- sections
             .traverse(
               section =>
                 validateFormComponents(
                   allFieldsToValidate,
                   section,
                   cache.form.envelopeId,
                   envelope,
                   retrievals,
                   cache.form.thirdPartyData,
                   cache.formTemplate,
                   data,
                   GetEmailCodeFieldMatcher(allSections),
                   maybeAccessCode
               ))
             .map(Monoid[ValidatedType[ValidationResult]].combineAll)
      v = Monoid.combine(v1, ValidationUtil.validateFileUploadHasScannedFiles(allFieldsToValidate, envelope))
      errors = evaluateValidation(v, allFieldsToValidate, data, envelope).toMap
    } yield (v, errors)

  }
}

object ValidationValues {

  val sortCodeLength = 2
  val bankAccountLength = 8
  val sterlingLength = 11
  val addressLine = 35
  val addressLine4 = 27
  val emailLimit = 241
  val postcodeLimit = 8
}
