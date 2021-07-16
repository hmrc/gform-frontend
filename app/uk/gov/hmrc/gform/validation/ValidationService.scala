/*
 * Copyright 2021 HM Revenue & Customs
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
import org.typelevel.ci.CIString
import play.api.i18n.{ I18nSupport, Messages }
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.fileupload._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.{ PageModel, Repeater, Singleton, Visibility }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse, InternationalAddress, UkAddress }
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.form.{ Validated => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.sharedmodel.{ CannotRetrieveResponse, LangADT, NotFound, ServiceResponse }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.typeclasses.Rnd

import scala.concurrent.{ ExecutionContext, Future }

class ValidationService(
  fileUploadService: FileUploadAlgebra[Future],
  booleanExprEval: BooleanExprEval[Future],
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  recalculation: Recalculation[Future, Throwable],
  i18nSupport: I18nSupport
)(implicit ec: ExecutionContext) {

  def validatePageModel[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidatedType[ValidatorsResult]] = {
    def lift[T](fv: Future[ValidatedType[T]]) = EitherT(fv.map(_.toEither))

    // format: off
    val eT = for {
      _                     <- lift(validatePageModelComponents(pageModel, formModelVisibilityOptics, cache, envelope, getEmailCodeFieldMatcher))
      valRes                <- lift(validateUsingValidators(pageModel, formModelVisibilityOptics))
      emailsForVerification <- lift(sendVerificationEmails(pageModel, formModelVisibilityOptics, cache.thirdPartyData))
    } yield {
      valRes.copy(emailVerification = emailsForVerification)
    }
    // format: on

    eT.value.map(Validated.fromEither)
  }

  def validateFormModel[D <: DataOrigin](
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidationResult] = {

    val formModel = formModelVisibilityOptics.formModel
    val allFields = formModelVisibilityOptics.allFormComponents

    val emailCodeMatcher = GetEmailCodeFieldMatcher(formModel)

    for {
      v1 <- formModel.pages
              .traverse(pageModel =>
                validatePageModel(
                  pageModel,
                  cache,
                  envelope,
                  formModelVisibilityOptics,
                  emailCodeMatcher
                )
              )
              .map(Monoid[ValidatedType[ValidatorsResult]].combineAll)
    } yield {
      val v = Monoid.combine(v1, ValidationUtil.validateFileUploadHasScannedFiles(allFields, envelope))
      ValidationUtil.evaluateValidationResult(allFields, v, formModelVisibilityOptics, envelope)
    }
  }

  def validatePageModelComponents[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator): Future[ValidatedType[Unit]] =
    pageModel.allFormComponents
      .filterNot(_.onlyShowOnSummary)
      .traverse(fv => validateFormComponent(fv, formModelVisibilityOptics, cache, envelope, getEmailCodeFieldMatcher))
      .map(res => Monoid[ValidatedType[Unit]].combineAll(res))

  def validateDeclarationSection[D <: DataOrigin](
    cache: CacheData,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    envelope: EnvelopeWithMapping
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidationResult] =
    formModelVisibilityOptics.formModel.allFormComponents
      .filterNot(_.onlyShowOnSummary)
      .traverse(fv =>
        validateFormComponent(
          fv,
          formModelVisibilityOptics,
          cache,
          envelope,
          GetEmailCodeFieldMatcher.noop
        )
      )
      .map { res =>
        val validatedType: ValidatedType[ValidatorsResult] =
          Monoid[ValidatedType[Unit]].combineAll(res).map(_ => ValidatorsResult.empty)
        val allFields = formModelVisibilityOptics.allFormComponents
        ValidationUtil.evaluateValidationResult(allFields, validatedType, formModelVisibilityOptics, envelope)
      }

  private def validateFormComponent[D <: DataOrigin](
    formComponent: FormComponent,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher
  )(implicit
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidatedType[Unit]] =
    new ComponentsValidator[D, Future](
      formModelVisibilityOptics,
      formComponent,
      cache,
      envelope,
      lookupRegistry,
      booleanExprEval
    ).validate(getEmailCodeFieldMatcher)

  private def sendVerificationEmails[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    thirdPartyData: ThirdPartyData
  )(implicit
    hc: HeaderCarrier,
    l: LangADT
  ): Future[ValidatedType[Map[EmailFieldId, EmailAndCode]]] = {

    val emailFields: List[(EmailFieldId, EmailVerifierService)] = pageModel.allFormComponents.collect {
      case IsEmailVerifier(fcId, emailVerifiedBy) => (fcId, emailVerifiedBy.emailVerifierService)
    }

    def emailExist(formComponentId: EmailFieldId, email: String): Boolean =
      thirdPartyData.emailVerification.get(formComponentId).fold(false)(_.email === CIString(email))

    val emailAddressedToBeVerified: List[Option[(EmailFieldId, EmailAndCode, EmailVerifierService)]] =
      emailFields.map { case (ef, emailVerifierService) =>
        val maybeEmail = formModelVisibilityOptics.data.one(ef.modelComponentId)

        maybeEmail.collect {
          case email if !emailExist(ef, email) =>
            (ef, EmailAndCode.emailVerificationCode(email)(Rnd.RandomInt), emailVerifierService)
        }
      }
    emailAddressedToBeVerified.flatten
      .traverse { case (emailFieldId, eac @ EmailAndCode(email, code), emailVerifierService) =>
        gformConnector
          .sendEmail(
            ConfirmationCodeWithEmailService(NotifierEmailAddress(email.toString), code, emailVerifierService, l)
          )
          .map(_ => (emailFieldId, eac))
      }
      .map(_.toMap.valid)
  }

  private def validateUsingValidators[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    messages: Messages,
    hc: HeaderCarrier,
    sse: SmartStringEvaluator
  ): Future[ValidatedType[ValidatorsResult]] = {
    val valid = ValidatorsResult.empty.valid.pure[Future]
    pageModel match {
      case s: Singleton[_] =>
        s.page.validators.map(validateUsingSectionValidators(_, formModelVisibilityOptics)).getOrElse(valid)
      case r: Repeater[_] => valid
    }
  }

  private def validateUsingSectionValidators[D <: DataOrigin](
    v: Validator,
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    sse: SmartStringEvaluator
  ): Future[ValidatedType[ValidatorsResult]] = {

    def compare(postCode: String)(drr: DesRegistrationResponse): Boolean = {
      val maybePostalCode = drr.address match {
        case UkAddress(_, _, _, _, postalCode)               => postalCode
        case InternationalAddress(_, _, _, _, _, postalCode) => postalCode
      }
      maybePostalCode.fold(true)(_.replace(" ", "").equalsIgnoreCase(postCode.replace(" ", "")))
    }

    v match {
      case HmrcRosmRegistrationCheckValidator(errorMessage, regime, utr, postcode) =>
        val utrValue = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(utr).stringRepresentation
        val postcodeValue = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(postcode).stringRepresentation

        val errors =
          Map(
            utr.formComponentId.modelComponentId      -> Set(errorMessage.value),
            postcode.formComponentId.modelComponentId -> Set(errorMessage.value)
          )

        val desRegistrationRequest = DesRegistrationRequest(regime, false, false)

        gformConnector
          .validatePostCodeUtr(utrValue, desRegistrationRequest)
          .flatMap {
            case NotFound               => Future.successful(errors.invalid)
            case CannotRetrieveResponse => Future.failed(new Exception("Call to des registration has failed"))
            case ServiceResponse(drr) =>
              Future.successful(
                if (compare(postcodeValue)(drr)) ValidatorsResult(Some(drr), Map.empty).valid
                else errors.invalid
              )
          }
      case BankAccountModulusCheck(errorMessage, accountNumber, sortCode) =>
        val accountNumberValue = formModelVisibilityOptics.evalAndApplyTypeInfoFirst(accountNumber).stringRepresentation

        val sortCodeCombined = UkSortCode
          .fields(sortCode.formComponentId.modelComponentId.indexedComponentId)
          .map(formModelVisibilityOptics.data.one)
          .toList
          .flatten
          .mkString("-")

        val errors = Map(
          accountNumber.formComponentId.modelComponentId -> Set(errorMessage.value),
          sortCode.formComponentId.modelComponentId      -> Set(errorMessage.value)
        )

        gformConnector
          .validateBankModulus(accountNumberValue, sortCodeCombined)
          .map(b => if (b) ValidatorsResult.empty.valid else errors.invalid)
    }
  }
}

object ValidationValues {

  val sortCodeLength = 2
  val bankAccountLength = 8
  val sterlingLength = 11
  val addressLine = 35
  val addressLine4 = 27
  val overseasCity = 27
  val emailLimit = 241
  val postcodeLimit = 8
  val countryLimit = 50
}
