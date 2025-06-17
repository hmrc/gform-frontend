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

import cats.Monoid
import cats.data._
import cats.implicits._
import org.typelevel.ci.CIString
import play.api.i18n.Messages
import uk.gov.hmrc.gform.controllers.CacheData
import uk.gov.hmrc.gform.eval.BooleanExprEval
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.objectStore._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.PageModel
import uk.gov.hmrc.gform.models.Visibility
import uk.gov.hmrc.gform.models.email.EmailFieldId
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.models.optics.FormModelVisibilityOptics
import uk.gov.hmrc.gform.sharedmodel.EmailVerifierService
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.email.ConfirmationCodeWithEmailService
import uk.gov.hmrc.gform.sharedmodel.form.{ Validated => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.notifier.NotifierEmailAddress
import uk.gov.hmrc.gform.typeclasses.Rnd
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import ComponentChecker.CheckInterpreter
import GformError.linkedHashSetMonoid

class ValidationService(
  booleanExprEval: BooleanExprEval[Future],
  gformConnector: GformConnector,
  lookupRegistry: LookupRegistry,
  checkInterpreter: CheckInterpreter = ComponentChecker.NonShortCircuitInterpreter
)(implicit ec: ExecutionContext) {

  private def lift[T](fv: Future[ValidatedType[T]]) = EitherT(fv.map(_.toEither))
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
  ): Future[ValidatedType[ValidatorsResult]] =
    validatePageModelBase(pageModel, cache, envelope, formModelVisibilityOptics, getEmailCodeFieldMatcher, true)

  def validatePageModelWithoutCustomValidators[D <: DataOrigin](
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
  ): Future[ValidatedType[ValidatorsResult]] =
    validatePageModelBase(pageModel, cache, envelope, formModelVisibilityOptics, getEmailCodeFieldMatcher, false)

  private def validatePageModelBase[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher,
    validateCustomValidators: Boolean
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidatedType[ValidatorsResult]] = {
    val eT = for {
      _ <- lift(
             validatePageModelComponents(
               pageModel,
               formModelVisibilityOptics,
               cache,
               envelope,
               getEmailCodeFieldMatcher,
               validateCustomValidators
             )
           )
      formTemplateId = cache.formTemplate._id
      emailsForVerification <-
        lift(sendVerificationEmails(pageModel, formModelVisibilityOptics, cache.thirdPartyData, formTemplateId))
    } yield ValidatorsResult(emailVerification = emailsForVerification)

    eT.value.map(Validated.fromEither)
  }

  def validateFormModel[D <: DataOrigin](
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    maybeCoordinates: Option[Coordinates]
  )(implicit
    hc: HeaderCarrier,
    messages: Messages,
    l: LangADT,
    sse: SmartStringEvaluator
  ): Future[ValidationResult] = {

    val formModel = formModelVisibilityOptics.formModel

    //form component should only be included if both it's page and itself pass onDemandIncludeIf
    def onDemandIncludeIfFilter(formComponent: FormComponent): Boolean = {
      val page = formModel.pageLookup(formComponent.id)
      def includeComponent = formComponent.includeIf.forall(includeIf => formModel.onDemandIncludeIf.forall(f => f(includeIf)))
      def includePage = page.getIncludeIf.forall(includeIf => formModel.onDemandIncludeIf.forall(f => f(includeIf)))
      includeComponent && includePage
    }

    val allFields = maybeCoordinates
      .fold(formModelVisibilityOptics.allFormComponents)(
        formModelVisibilityOptics.allFormComponentsForCoordinates
      )
      .filter(
        onDemandIncludeIfFilter
      )

    val emailCodeMatcher = GetEmailCodeFieldMatcher(formModel)

    for {
      v <- formModel.pages
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
    } yield ValidationUtil.evaluateValidationResult(allFields, v, formModelVisibilityOptics, envelope)
  }

  def validateATLs[D <: DataOrigin](
    pageModels: List[PageModel[Visibility]],
    formModelVisibilityOptics: FormModelVisibilityOptics[D]
  ): ValidatedType[Unit] =
    pageModels
      .flatMap(_.allATLRepeatsWhiles)
      .groupBy { case (key, _) => key }
      .values
      .map(_.flatMap(_._2))
      .map { includeIfs =>
        if (includeIfs.isEmpty || !includeIfs.forall(formModelVisibilityOptics.evalIncludeIfExpr(_, None))) {
          ().valid
        } else {
          GformError.emptyGformError.invalid
        }
      }
      .toList
      .combineAll

  def validatePageModelComponents[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    cache: CacheData,
    envelope: EnvelopeWithMapping,
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher,
    validateValidators: Boolean
  )(implicit messages: Messages, l: LangADT, sse: SmartStringEvaluator): Future[ValidatedType[Unit]] =
    pageModel.allFormComponents
      .filterNot(_.onlyShowOnSummary)
      .traverse(fv =>
        validateFormComponent(
          fv,
          formModelVisibilityOptics,
          cache,
          envelope,
          getEmailCodeFieldMatcher,
          validateValidators
        )
      )
      .map(res => Monoid[ValidatedType[Unit]].combineAll(res))

  def validateAllSections[D <: DataOrigin](
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
          GetEmailCodeFieldMatcher.noop,
          true
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
    getEmailCodeFieldMatcher: GetEmailCodeFieldMatcher,
    validateValidators: Boolean
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
      booleanExprEval,
      checkInterpreter,
      validateValidators
    ).validate(getEmailCodeFieldMatcher)

  private def sendVerificationEmails[D <: DataOrigin](
    pageModel: PageModel[Visibility],
    formModelVisibilityOptics: FormModelVisibilityOptics[D],
    thirdPartyData: ThirdPartyData,
    formTemplateId: FormTemplateId
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
            ConfirmationCodeWithEmailService(
              NotifierEmailAddress(email.toString),
              code,
              emailVerifierService,
              l,
              formTemplateId
            )
          )
          .map(_ => (emailFieldId, eac))
      }
      .map(_.toMap.valid)
  }
}

object ValidationValues {

  val bankAccountLength = 8
  val sterlingLength = 11
  val addressLine = 35
  val addressLine4 = 27
  val overseasCity = 27
  val emailLimit = 241
  val postcodeLimit = 8
  val countryLimit = 50
}
