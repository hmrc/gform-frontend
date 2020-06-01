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

import cats.data.Validated
import cats.implicits._
import play.api.i18n.Messages

import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.fileupload.{ Envelope, Error, File, Infected }
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, VerificationCodeFieldId, verificationCodeFieldId }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString, SubmissionRef }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormDataRecalculated, ThirdPartyData }
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

class GetEmailCodeFieldMatcher(sections: List[Section]) {
  def apply(fc: FormComponent): EmailCodeFieldMatcher = {
    val fcIds: Map[VerificationCodeFieldId, EmailFieldId] = sections
      .flatMap(_.expandSectionFull.allFCs)
      .collect {
        case IsEmailVerifier(emailFcId, emailVerifiedBy) =>
          (verificationCodeFieldId(emailVerifiedBy.formComponentId), emailFcId)
      }
      .toMap
    new EmailCodeFieldMatcher(verificationCodeFieldId(fc.id), fcIds)
  }
}

object GetEmailCodeFieldMatcher {
  def apply(sections: List[Section]) = new GetEmailCodeFieldMatcher(sections)
  val noop = new GetEmailCodeFieldMatcher(Nil)
}

class ComponentsValidator(
  data: FormDataRecalculated,
  envelopeId: EnvelopeId,
  envelope: Envelope,
  retrievals: MaterialisedRetrievals,
  booleanExpr: BooleanExprEval[Future],
  thirdPartyData: ThirdPartyData,
  formTemplate: FormTemplate,
  lookupRegistry: LookupRegistry
)(
  implicit
  ec: ExecutionContext,
  messages: Messages,
  l: LangADT,
  sse: SmartStringEvaluator
) {

  val cvh = new ComponentsValidatorHelper()
  val dateValidation = new DateValidation()

  private[validation] def validIf(formComponent: FormComponent, validationResult: ValidatedType[Unit])(
    implicit
    hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    if (validationResult.isValid) {
      findFirstCustomValidationError(formComponent)
        .flatMap(produceCustomValidationErrorOrDefaultValidationResult(formComponent, _, validationResult))
    } else validationResult.pure[Future]

  private def produceCustomValidationErrorOrDefaultValidationResult(
    formComponent: FormComponent,
    customValidationError: Option[SmartString],
    validationResult: ValidatedType[Unit])(
    implicit
    hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    customValidationError
      .map(produceValidationError(formComponent, _).pure[Future])
      .getOrElse(defaultFormComponentValidIf(formComponent, validationResult))

  private def findFirstCustomValidationError(formComponent: FormComponent)(
    implicit
    hc: HeaderCarrier): Future[Option[SmartString]] =
    evaluateCustomValidators(formComponent)
      .map(_.find(listItem => !listItem._1).map(_._2))

  private def evaluateCustomValidators(formComponent: FormComponent)(
    implicit
    hc: HeaderCarrier): Future[List[(Boolean, SmartString)]] =
    formComponent.validators.traverse { v =>
      booleanExpr
        .isTrue(v.validIf.expr, data.data, retrievals, data.invisible, thirdPartyData, envelopeId, formTemplate)
        .map(b => (b, v.errorMessage))
    }

  private def produceValidationError(formComponent: FormComponent, message: SmartString)(
    implicit
    hc: HeaderCarrier): Validated[Map[FormComponentId, Set[String]], Nothing] =
    Validated.invalid(Map[FormComponentId, Set[String]](formComponent.id -> Set(message.value)))

  private def defaultFormComponentValidIf(formComponent: FormComponent, validationResult: ValidatedType[Unit])(
    implicit
    hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    formComponent.validIf match {
      case Some(vi) =>
        booleanExpr
          .isTrue(vi.expr, data.data, retrievals, data.invisible, thirdPartyData, envelopeId, formTemplate)
          .map {
            case false => validationFailure(formComponent, "generic.error.required", None)
            case true  => validationResult
          }
      case None => validationResult.pure[Future]
    }

  def validate(
    formComponent: FormComponent,
    fieldValues: List[FormComponent],
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher
  )(
    implicit
    hc: HeaderCarrier,
    messages: Messages
  ): Future[ValidatedType[Unit]] = {

    val emailCodeFieldMatcher: EmailCodeFieldMatcher = getEmailCodeFieldMatcher(formComponent)

    formComponent.`type` match {
      case sortCode @ UkSortCode(_) =>
        validIf(
          formComponent,
          SortCodeValidation
            .validateSortCode(formComponent, sortCode, formComponent.mandatory)(data))
      case date @ Date(_, _, _) =>
        validIf(
          formComponent,
          dateValidation.validateDate(formComponent, date, getCompanionFieldComponent(date, fieldValues), data))
      case Text(SubmissionRefFormat, _, _, _) if formTemplate.parentFormSubmissionRefs.contains(formComponent.id) =>
        validIf(
          formComponent,
          ComponentValidator
            .validateParentSubmissionRef(formComponent, SubmissionRef(envelopeId))(data))
      case emailCodeFieldMatcher.EmailCodeField(emailField) =>
        validIf(formComponent, ComponentValidator.validateEmailCode(formComponent, emailField, data, thirdPartyData))
      case Text(constraint, _, _, _) =>
        validIf(formComponent, ComponentValidator.validateText(formComponent, constraint)(data, lookupRegistry))
      case TextArea(constraint, _, _) =>
        validIf(
          formComponent,
          ComponentValidator
            .validateText(formComponent, constraint)(data, lookupRegistry))
      case address @ Address(_) =>
        validIf(formComponent, new AddressValidation().validateAddress(formComponent, address)(data))
      case c @ Choice(_, _, _, _, _) =>
        validIf(formComponent, ComponentValidator.validateChoice(formComponent)(data))
      case _: RevealingChoice =>
        validIf(formComponent, ComponentValidator.validateChoice(formComponent)(data))
      case Group(_, _, _, _, _, _)  => cvh.validF //a group is read-only
      case FileUpload()             => validateFileUpload(data, formComponent, envelope).pure[Future]
      case InformationMessage(_, _) => cvh.validF
      case HmrcTaxPeriod(_, _, _) =>
        validIf(formComponent, ComponentValidator.validateChoice(formComponent)(data))
    }
  }

  private def validateFileUpload(data: FormDataRecalculated, fieldValue: FormComponent, envelope: Envelope)(
    implicit hc: HeaderCarrier,
    messages: Messages): ValidatedType[Unit] = {
    val fileId = FileId(fieldValue.id.value)
    val file: Option[File] = envelope.files.find(_.fileId.value == fileId.value)

    file match {
      case Some(File(fileId, Error(Some(reason)), _)) =>
        validationFailure(fieldValue, "generic.error.unknownUpload", None)
      case Some(File(fileId, Error(None), _)) =>
        validationFailure(fieldValue, "generic.error.unknownUpload", None)
      case Some(File(fileId, Infected, _)) =>
        validationFailure(fieldValue, "generic.error.virus", None)
      case Some(File(fileId, _, _)) => ValidationServiceHelper.validationSuccess
      case None if fieldValue.mandatory =>
        validationFailure(fieldValue, "generic.error.upload", None)
      case None =>
        val dataEmpty: Boolean = data.data.get(fieldValue.id).forall(_.isEmpty)
        if (dataEmpty) validationSuccess else validationFailure(fieldValue, "generic.error.upload", None)
    }
  }
}

class ComponentsValidatorHelper(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator) {

  def validF(implicit ec: ExecutionContext): Future[ValidatedType[Unit]] =
    ValidationServiceHelper.validationSuccess.pure[Future]

  def validateRF(fieldValue: FormComponent, value: String): Seq[String] => ValidatedType[Unit] =
    validateRequired(fieldValue, fieldValue.id.withSuffix(value), None, value) _

  def validateFF(fieldValue: FormComponent, value: String): Seq[String] => ValidatedType[Unit] =
    validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateRequired(
    fieldValue: FormComponent,
    fieldId: FormComponentId,
    errorPrefix: Option[String] = None,
    value: String)(xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil =>
        Map(
          fieldId -> ComponentsValidatorHelper
            .errors(fieldValue, "field.error.required", None, errorPrefix.getOrElse(""))).invalid
      case value :: Nil  => validationSuccess
      case value :: rest => validationSuccess // we don't support multiple values yet
    }

  def validateForbidden(fieldValue: FormComponent, fieldId: FormComponentId)(xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil => validationSuccess
      case value :: Nil =>
        Map(fieldId -> ComponentsValidatorHelper.errors(fieldValue, "generic.error.forbidden", None)).invalid
      case value :: rest =>
        Map(fieldId -> ComponentsValidatorHelper.errors(fieldValue, "generic.error.forbidden", None)).invalid // we don't support multiple values yet
    }
}

object ComponentsValidatorHelper {

  def fieldDescriptor(
    fieldValue: FormComponent,
    workedOnId: FormComponentId,
    otherFormComponent: Option[FormComponent],
    partLabel: String)(implicit l: LangADT, sse: SmartStringEvaluator, messages: Messages): String =
    otherFormComponent match {
      case Some(x) if x.id === workedOnId =>
        x.shortName.map { _.value + " " + partLabel }.getOrElse(x.label.value + " " + partLabel)
      case Some(x) =>
        fieldValue.shortName
          .map { input =>
            messages("helper.order", input.value, partLabel)
          }
          .getOrElse(messages("helper.order", fieldValue.label.value, partLabel))
      case None =>
        fieldValue.shortName
          .map(ls => messages("helper.order", ls.value, partLabel))
          .getOrElse(messages("helper.order", fieldValue.label.value, partLabel))
    }

  def errors(fieldValue: FormComponent, messageKey: String, vars: Option[List[String]], partLabel: String = "")(
    implicit l: LangADT,
    sse: SmartStringEvaluator,
    messages: Messages): Set[String] = {
    val varsList: List[String] = vars.getOrElse(Nil)
    val withDescriptor: List[String] = fieldDescriptor(fieldValue, fieldValue.id, None, partLabel).trim :: varsList
    Set(
      fieldValue.errorMessage
        .map(ls => ls.value)
        .getOrElse(messages(messageKey, withDescriptor: _*)))
  }

  def getError(fieldValue: FormComponent, messageKey: String, vars: Option[List[String]])(
    implicit l: LangADT,
    sse: SmartStringEvaluator,
    messages: Messages): Validated[Map[FormComponentId, Set[String]], Nothing] =
    Map(fieldValue.id -> errors(fieldValue, messageKey, vars)).invalid
}
