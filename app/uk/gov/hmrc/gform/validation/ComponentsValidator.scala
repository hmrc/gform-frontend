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

import cats.Monad
import cats.implicits._
import play.api.i18n.Messages

import scala.language.higherKinds
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.fileupload.{ Envelope, Error, File, Infected }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.Visibility
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.{ FormModel }
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, VerificationCodeFieldId, verificationCodeFieldId }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

class EmailCodeFieldMatcher(
  val fcId: VerificationCodeFieldId,
  val fcIds: Map[VerificationCodeFieldId, EmailFieldId]
) {
  object EmailCodeField {
    def unapply(formComponent: ComponentType): Option[EmailFieldId] = fcIds.get(fcId)
  }
}

class GetEmailCodeFieldMatcher(fcIds: Map[VerificationCodeFieldId, EmailFieldId]) {
  def apply(fc: FormComponent): EmailCodeFieldMatcher =
    new EmailCodeFieldMatcher(verificationCodeFieldId(fc.id), fcIds)
}

object GetEmailCodeFieldMatcher {
  def apply(formModel: FormModel[Visibility]) = {
    val fcIds: Map[VerificationCodeFieldId, EmailFieldId] = formModel.allFormComponents.collect {
      case IsEmailVerifier(emailFcId, emailVerifiedBy) =>
        (verificationCodeFieldId(emailVerifiedBy.formComponentId), emailFcId)
    }.toMap
    new GetEmailCodeFieldMatcher(fcIds)
  }
  val noop = new GetEmailCodeFieldMatcher(Map.empty)
}

class ComponentsValidator[D <: DataOrigin, F[_]: Monad](
  formModelVisibilityOptics: FormModelVisibilityOptics[D],
  formComponent: FormComponent,
  cache: CacheData,
  envelope: Envelope,
  lookupRegistry: LookupRegistry,
  booleanExprEval: BooleanExprEval[F]
)(
  implicit
  messages: Messages,
  l: LangADT,
  sse: SmartStringEvaluator
) {

  private val envelopeId: EnvelopeId = cache.envelopeId
  private val thirdPartyData: ThirdPartyData = cache.thirdPartyData
  private val formTemplate: FormTemplate = cache.formTemplate

  private val dateValidation = new DateValidation[D](formModelVisibilityOptics)

  private[validation] def validIf(
    validationResult: ValidatedType[Unit]
  )(
    implicit
    hc: HeaderCarrier
  ): F[ValidatedType[Unit]] =
    if (validationResult.isValid) {
      for {
        firstCustomer <- findFirstCustomValidationError
        res           <- produceCustomValidationErrorOrDefaultValidationResult(firstCustomer, validationResult)
      } yield res
    } else validationResult.pure[F]

  private def produceCustomValidationErrorOrDefaultValidationResult(
    customValidationError: Option[SmartString],
    validationResult: ValidatedType[Unit]
  )(
    implicit
    hc: HeaderCarrier
  ): F[ValidatedType[Unit]] =
    customValidationError
      .map(produceValidationError(_).pure[F])
      .getOrElse(defaultFormComponentValidIf(validationResult))

  private def findFirstCustomValidationError(
    implicit
    hc: HeaderCarrier
  ): F[Option[SmartString]] =
    evaluateCustomValidators(formComponent).map(_.find(listItem => !listItem._1).map(_._2))

  private def evaluateCustomValidators(
    formComponent: FormComponent
  )(
    implicit
    hc: HeaderCarrier
  ): F[List[(Boolean, SmartString)]] =
    formComponent.validators.traverse { formComponentValidator =>
      val fb: F[Boolean] = booleanExprEval.eval(formModelVisibilityOptics)(formComponentValidator.validIf.booleanExpr)
      fb.map { b =>
        (b, formComponentValidator.errorMessage)
      }
    }

  private def produceValidationError(
    message: SmartString
  ): ValidatedType[Unit] =
    Map(formComponent.modelComponentId -> Set(message.value)).invalid

  private def defaultFormComponentValidIf(
    validationResult: ValidatedType[Unit]
  )(
    implicit
    hc: HeaderCarrier
  ): F[ValidatedType[Unit]] =
    formComponent.validIf.fold(validationResult.pure[F]) { vi =>
      booleanExprEval.eval(formModelVisibilityOptics)(vi.booleanExpr).map { b =>
        if (b)
          validationResult
        else
          validationFailure(formComponent, "generic.error.required", None)
      }
    }

  def validate(
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher
  )(
    implicit
    hc: HeaderCarrier,
    messages: Messages
  ): F[ValidatedType[Unit]] = {

    val emailCodeFieldMatcher: EmailCodeFieldMatcher = getEmailCodeFieldMatcher(formComponent)

    formComponent.`type` match {
      case UkSortCode(_) =>
        validIf(
          SortCodeValidation
            .validateSortCode(formComponent)(formModelVisibilityOptics))
      case date @ Date(_, _, _) =>
        validIf(
          dateValidation.validateDate(
            formComponent,
            date
          )
        )
      case Text(SubmissionRefFormat, _, _, _) if formTemplate.parentFormSubmissionRefs.contains(formComponent.id) =>
        validIf(
          ComponentValidator
            .validateParentSubmissionRef(formComponent, SubmissionRef(envelopeId))(formModelVisibilityOptics))
      case emailCodeFieldMatcher.EmailCodeField(emailField) =>
        validIf(
          ComponentValidator.validateEmailCode(formComponent, emailField, formModelVisibilityOptics, thirdPartyData))
      case Text(constraint, _, _, _) =>
        validIf(ComponentValidator.validateText(formComponent, constraint)(formModelVisibilityOptics, lookupRegistry))
      case TextArea(constraint, _, _) =>
        validIf(
          ComponentValidator
            .validateText(formComponent, constraint)(formModelVisibilityOptics, lookupRegistry))
      case address @ Address(_) =>
        validIf(new AddressValidation[D]().validateAddress(formComponent, address)(formModelVisibilityOptics))
      case c @ Choice(_, _, _, _, _) =>
        validIf(ComponentValidator.validateChoice(formComponent)(formModelVisibilityOptics))
      case _: RevealingChoice =>
        validIf(ComponentValidator.validateChoice(formComponent)(formModelVisibilityOptics))
      case Group(_, _, _, _, _)     => validationSuccess.pure[F]
      case FileUpload()             => validateFileUpload(envelope).pure[F]
      case InformationMessage(_, _) => validationSuccess.pure[F]
      case HmrcTaxPeriod(_, _, _) =>
        validIf(ComponentValidator.validateChoice(formComponent)(formModelVisibilityOptics))
      case t @ Time(_, _) =>
        validIf(ComponentValidator.validateTime(formComponent, t, formModelVisibilityOptics))
    }
  }

  private def validateFileUpload(envelope: Envelope)(implicit messages: Messages): ValidatedType[Unit] = {
    val fileId = FileId(formComponent.id.value)
    val file: Option[File] = envelope.files.find(_.fileId.value == fileId.value)

    file match {
      case Some(File(fileId, Error(Some(reason)), _)) =>
        validationFailure(formComponent, "generic.error.unknownUpload", None)
      case Some(File(fileId, Error(None), _)) =>
        validationFailure(formComponent, "generic.error.unknownUpload", None)
      case Some(File(fileId, Infected, _)) =>
        validationFailure(formComponent, "generic.error.virus", None)
      case Some(File(fileId, _, _)) => validationSuccess
      case None if formComponent.mandatory =>
        validationFailure(formComponent, "generic.error.upload", None)
      case None => validationSuccess
    }
  }
}

class ComponentsValidatorHelper(implicit messages: Messages, sse: SmartStringEvaluator) {

  def validateRequired2(
    formComponent: FormComponent,
    atomicFcId: ModelComponentId.Atomic,
    errorPrefix: Option[String] = None
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil =>
        Map[ModelComponentId, Set[String]](
          atomicFcId -> ComponentsValidatorHelper
            .errors(formComponent, "field.error.required", None, errorPrefix.getOrElse(""))).invalid
      case value :: Nil  => validationSuccess
      case value :: rest => validationSuccess // we don't support multiple values yet
    }

  def validateForbidden(
    formComponent: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): ValidatedType[Unit] = {
    val res = Map[ModelComponentId, Set[String]](
      atomicFcId -> ComponentsValidatorHelper
        .errors(formComponent, "generic.error.forbidden", None)).invalid
    xs.filterNot(_.isEmpty()) match {
      case Nil           => validationSuccess
      case value :: Nil  => res
      case value :: rest => res // we don't support multiple values yet
    }
  }
}

object ComponentsValidatorHelper {

  def fieldDescriptor(
    formComponent: FormComponent,
    partLabel: String
  )(
    implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): String =
    formComponent.shortName
      .map(ls => messages("helper.order", ls.value, partLabel))
      .getOrElse(messages("helper.order", formComponent.label.value, partLabel))

  def errors(
    formComponent: FormComponent,
    messageKey: String,
    vars: Option[List[String]],
    partLabel: String = ""
  )(
    implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): Set[String] = {
    val varsList: List[String] = vars.getOrElse(Nil)
    val withDescriptor: List[String] = fieldDescriptor(formComponent, partLabel).trim :: varsList
    Set(
      formComponent.errorMessage
        .map(ls => ls.value)
        .getOrElse(messages(messageKey, withDescriptor: _*))
    )
  }
}
