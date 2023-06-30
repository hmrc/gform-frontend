/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.{ Monad, Monoid }
import cats.implicits._
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.Visibility
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.models.FormModel
import uk.gov.hmrc.gform.models.email.{ EmailFieldId, VerificationCodeFieldId, verificationCodeFieldId }
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import ComponentChecker._

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
  envelope: EnvelopeWithMapping,
  lookupRegistry: LookupRegistry,
  booleanExprEval: BooleanExprEval[F],
  interpreter: CheckInterpreter
)(implicit
  messages: Messages,
  l: LangADT,
  sse: SmartStringEvaluator
) { self =>

  implicit val checkInterpreter: CheckInterpreter = interpreter

  private[validation] def validIf(
    validationResult: ValidatedType[Unit]
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
  ): F[ValidatedType[Unit]] =
    customValidationError
      .map(produceValidationError(_).pure[F])
      .getOrElse(defaultFormComponentValidIf(validationResult))

  private def findFirstCustomValidationError: F[Option[SmartString]] =
    evaluateCustomValidators(formComponent).map(_.find(listItem => !listItem._1).map(_._2))

  private def evaluateCustomValidators(
    formComponent: FormComponent
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
    formComponent match {
      case IsMultiField(_) =>
        val evaluationResults = formModelVisibilityOptics.evaluationResults
        val maybeAddressDetail = evaluationResults.exprMap.map(_._1).collectFirst {
          case AddressLens(fcId, addressDetail) if fcId === formComponent.id => addressDetail
        }
        val modelComponentIds = maybeAddressDetail.fold(formComponent.multiValueId.atomsModelComponentIds) {
          addressDetail =>
            formComponent match {
              case IsOverseasAddress(_) =>
                List(formComponent.atomicFormComponentId(addressDetail.toOverseasAddressAtom))
              case IsAddress(_) =>
                List(formComponent.atomicFormComponentId(addressDetail.toAddressAtom))
              case IsPostcodeLookup(_) =>
                List(formComponent.atomicFormComponentId(PostcodeLookup.postcode))
              case other => throw new Exception(s"Invalid formComponent - $other for addressDetail")
            }
        }
        Monoid[ValidatedType[Unit]].combineAll(
          modelComponentIds.map(mcId => Map[ModelComponentId, Set[String]](mcId -> Set(message.value())).invalid)
        )
      case _ =>
        Map[ModelComponentId, Set[String]](formComponent.modelComponentId -> Set(message.value())).invalid
    }

  private def defaultFormComponentValidIf(
    validationResult: ValidatedType[Unit]
  ): F[ValidatedType[Unit]] =
    formComponent.validIf.fold(validationResult.pure[F]) { vi =>
      booleanExprEval.eval(formModelVisibilityOptics)(vi.booleanExpr).map { b =>
        if (b)
          validationResult
        else
          validationFailure(formComponent, TextChecker.genericErrorRequired, None)
      }
    }

  def validate(
    getEmailCodeFieldMatcherProvided: GetEmailCodeFieldMatcher
  )(implicit
    messages: Messages
  ): F[ValidatedType[Unit]] = {

    val checkerDependency = new CheckerDependency[D] {
      def formModelVisibilityOptics: FormModelVisibilityOptics[D] = self.formModelVisibilityOptics
      def formComponent: FormComponent = self.formComponent
      def cache: CacheData = self.cache
      def envelope: EnvelopeWithMapping = self.envelope
      def lookupRegistry: LookupRegistry = self.lookupRegistry
      def getEmailCodeFieldMatcher = getEmailCodeFieldMatcherProvided
    }

    val emailCodeFieldMatcher: EmailCodeFieldMatcher = getEmailCodeFieldMatcherProvided(formComponent)

    formComponent.`type` match {
      case date @ Date(_, _, _) =>
        validIf(
          new DateChecker[D]().runCheck(checkerDependency)
        )
      case CalendarDate =>
        validIf(
          new CalendarDateChecker[D]().runCheck(checkerDependency)
        )
      case PostcodeLookup(_, _, _) =>
        validIf(
          new PostcodeLookupChecker[D]().runCheck(checkerDependency)
        )
      case TaxPeriodDate =>
        validIf(
          new TaxPeriodDateChecker[D]().runCheck(checkerDependency)
        )
      case emailCodeFieldMatcher.EmailCodeField(emailField) =>
        validIf(
          new EmailFieldIdChecker[D]().runCheck(checkerDependency)
        )
      case Text(constraint, _, _, _, _, _) =>
        validIf(
          new TextChecker[D]().runCheck(checkerDependency)
        )

      case TextArea(constraint, _, _, _, _, _) =>
        validIf(
          new TextChecker[D]().runCheck(checkerDependency)
        )
      case address @ Address(_, _, _, _) =>
        validIf(
          new AddressChecker[D]().runCheck(checkerDependency)
        )
      case overseasAddress @ OverseasAddress(_, _, _, _, _) =>
        validIf(
          new OverseasAddressChecker[D]().runCheck(checkerDependency)
        )
      case Choice(_, _, _, _, _, _, _, _, _, _) =>
        validIf(new ChoiceChecker[D]().runCheck(checkerDependency))
      case _: RevealingChoice =>
        validIf(new ChoiceChecker[D]().runCheck(checkerDependency))
      case Group(_, _, _, _, _) => validationSuccess.pure[F]
      case FileUpload(_, _, _) =>
        validIf(
          new FileUploadChecker[D]().runCheck(checkerDependency)
        )
      case InformationMessage(_, _) => validationSuccess.pure[F]
      case HmrcTaxPeriod(_, _, _) =>
        validIf(new ChoiceChecker[D]().runCheck(checkerDependency))
      case t @ Time(_, _) =>
        validIf(new TimeChecker[D]().runCheck(checkerDependency))
      case MiniSummaryList(_) => validationSuccess.pure[F]
      case _: TableComp       => validationSuccess.pure[F]
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
  ): CheckProgram[Unit] = {

    val isEmpty = xs.filterNot(_.isEmpty()) match {
      case value :: Nil  => false
      case value :: rest => false // we don't support multiple values yet
      case _             => true
    }
    ifProgram(
      andCond = isEmpty,
      thenProgram = errorProgram(
        Map[ModelComponentId, Set[String]](
          atomicFcId -> ComponentsValidatorHelper
            .errors(formComponent, "field.error.required", None, errorPrefix.getOrElse(""))
        )
      ),
      elseProgram = successProgram(())
    )
  }

  def validateForbidden(
    formComponent: FormComponent,
    atomicFcId: ModelComponentId.Atomic
  )(
    xs: Seq[String]
  ): CheckProgram[Unit] = {
    val res = errorProgram[Unit](
      Map[ModelComponentId, Set[String]](
        atomicFcId -> ComponentsValidatorHelper
          .errors(formComponent, "generic.error.forbidden", None)
      )
    )
    val isEmpty = xs.filterNot(_.isEmpty()) match {
      case value :: Nil  => false
      case value :: rest => false // we don't support multiple values yet
      case _             => true
    }
    ifProgram(
      andCond = isEmpty,
      thenProgram = successProgram(()),
      elseProgram = res
    )
  }
}

object ComponentsValidatorHelper {

  def fieldDescriptor(
    formComponent: FormComponent,
    partLabel: String
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): String =
    formComponent.errorPlaceholder
      .map(ls => messages("helper.order", ls.value(), partLabel))
      .getOrElse(messages("helper.order", formComponent.label.value(), partLabel))

  def errors(
    formComponent: FormComponent,
    messageKey: String,
    vars: Option[List[String]],
    partLabel: String = ""
  )(implicit
    sse: SmartStringEvaluator,
    messages: Messages
  ): Set[String] = {
    val varsList: List[String] = vars.getOrElse(Nil)
    val withDescriptor: List[String] = fieldDescriptor(formComponent, partLabel).trim :: varsList
    Set(
      formComponent.errorMessage
        .map(ls => ls.value())
        .getOrElse(messages(messageKey, withDescriptor: _*))
    )
  }
}
