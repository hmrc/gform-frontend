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

import cats.data._
import cats.implicits._
import cats.Monoid
import play.api.i18n.{ I18nSupport, Messages }
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.controllers.AuthCacheWithForm
import uk.gov.hmrc.gform.controllers.helpers.FormDataHelpers
import uk.gov.hmrc.gform.fileupload._
import uk.gov.hmrc.gform.gformbackend.GformConnector
import uk.gov.hmrc.gform.graph.Recalculation
import uk.gov.hmrc.gform.keystore.RepeatingComponentService
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.models.ExpandUtils.submittedFCs
import uk.gov.hmrc.gform.models.helpers.Fields
import uk.gov.hmrc.gform.sharedmodel.des.{ DesRegistrationRequest, DesRegistrationResponse, InternationalAddress, UkAddress }
import uk.gov.hmrc.gform.sharedmodel.form.{ Validated => _, _ }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
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
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate)(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT): Future[ValidatedType[Unit]] =
    new ComponentsValidator(
      data,
      fileUploadService,
      envelopeId,
      retrievals,
      booleanExpr,
      thirdPartyData,
      formTemplate,
      lookupRegistry)
      .validate(fieldValue, fieldValues)

  def validateComponents(
    fieldValues: List[FormComponent],
    data: FormDataRecalculated,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate)(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT): Future[ValidatedType[Unit]] =
    fieldValues
      .traverse(fv => validateFieldValue(fv, fieldValues, data, envelopeId, retrievals, thirdPartyData, formTemplate))
      .map(Monoid[ValidatedType[Unit]].combineAll)

  def validateComponentsWithCache(cache: AuthCacheWithForm, declarationData: FormDataRecalculated)(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT): Future[ValidatedType[Unit]] = {
    val fieldValues: List[FormComponent] = Fields.flattenGroups(cache.formTemplate.declarationSection.fields)
    fieldValues
      .traverse(
        fv =>
          validateFieldValue(
            fv,
            fieldValues,
            declarationData,
            cache.form.envelopeId,
            cache.retrievals,
            cache.form.thirdPartyData,
            cache.formTemplate))
      .map(Monoid[ValidatedType[Unit]].combineAll)
  }

  private def validateUsingValidators(section: Section, data: FormDataRecalculated)(
    implicit hc: HeaderCarrier,
    messages: Messages): Future[ValidatedType[ValidationResult]] = {
    val sv: Option[Validator] = section.validators
    section.validators
      .map(validateUsingSectionValidators(_, data))
      .getOrElse(ValidationResult.empty.valid.pure[Future])
  }

  def validateFormComponents(
    sectionFields: List[FormComponent],
    section: Section,
    envelopeId: EnvelopeId,
    retrievals: MaterialisedRetrievals,
    thirdPartyData: ThirdPartyData,
    formTemplate: FormTemplate,
    data: FormDataRecalculated)(
    implicit hc: HeaderCarrier,
    messages: Messages,
    l: LangADT): Future[ValidatedType[ValidationResult]] = {
    def lift[T](fv: Future[ValidatedType[T]]) = EitherT(fv.map(_.toEither))

    val eT = for {
      _      <- lift(validateComponents(sectionFields, data, envelopeId, retrievals, thirdPartyData, formTemplate))
      valRes <- lift(validateUsingValidators(section, data))
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
    implicit hc: HeaderCarrier,
    messages: Messages): Future[ValidatedType[ValidationResult]] = {
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

  def validateForm(cache: AuthCacheWithForm, envelope: Envelope, retrievals: MaterialisedRetrievals)(
    implicit hc: HeaderCarrier,
    l: LangADT): Future[(ValidatedType[ValidationResult], Map[FormComponent, FormFieldValidationResult])] = {

    val dataRaw = FormDataHelpers.formDataMap(cache.form.formData)

    def filterSection(sections: List[Section], data: FormDataRecalculated): List[Section] =
      sections.filter(data.isVisible)

    import i18nSupport._

    for {
      data <- recalculation.recalculateFormData(
               dataRaw,
               cache.formTemplate,
               retrievals,
               cache.form.thirdPartyData,
               cache.form.envelopeId
             )
      allSections = RepeatingComponentService.getAllSections(cache.formTemplate, data)
      sections = filterSection(allSections, data)
      allFields = submittedFCs(
        data,
        sections
          .flatMap(_.expandSectionRc(data.data).allFCs)
      )
      v1 <- sections
             .traverse(
               section =>
                 validateFormComponents(
                   allFields,
                   section,
                   cache.form.envelopeId,
                   retrievals,
                   cache.form.thirdPartyData,
                   cache.formTemplate,
                   data))
             .map(Monoid[ValidatedType[ValidationResult]].combineAll)
      v = Monoid.combine(v1, ValidationUtil.validateFileUploadHasScannedFiles(allFields, envelope))
      errors = evaluateValidation(v, allFields, data, envelope).toMap
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
