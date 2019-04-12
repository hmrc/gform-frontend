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

import cats.implicits._
import uk.gov.hmrc.gform.sharedmodel.form.{ Validated => _ }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import cats.data.Validated
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.{ Error, File, FileUploadService, Infected }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FileId, FormDataRecalculated, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._
import uk.gov.hmrc.gform.views.html.localisation
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.gform.validation.ValidationServiceHelper.{ validationFailure, validationSuccess }
import scala.concurrent.{ ExecutionContext, Future }
import uk.gov.hmrc.gform.validation.ComponentsValidator._

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
              case false => validationFailure(fieldValue, "must be entered")
              case true  => validationResult
            }
        case _ => validationResult.pure[Future]
      }

    fieldValue.`type` match {
      case sortCode @ UkSortCode(_) =>
        validIf(
          SortCodeValidation
            .validateSortCode(fieldValue, sortCode, fieldValue.mandatory)(data))
      case date @ Date(_, _, _) =>
        validIf(DateValidation.validateDate(fieldValue, date, getCompanionFieldComponent(date, fieldValues), data))
      case text @ Text(constraint, _, _, _) =>
        validIf(
          ComponentValidator
            .validateText(fieldValue, constraint, retrievals)(data))
      case TextArea(constraint, _, _) =>
        validIf(
          ComponentValidator
            .validateText(fieldValue, constraint, retrievals)(data))
      case address @ Address(_) => validIf(AddressValidation.validateAddress(fieldValue, address)(data))
      case c @ Choice(_, _, _, _, _) =>
        validIf(ComponentValidator.validateChoice(fieldValue)(data))
      case Group(_, _, _, _, _, _)  => validF //a group is read-only
      case FileUpload()             => validateFileUpload(data, fieldValue)
      case InformationMessage(_, _) => validF
      case HmrcTaxPeriod(_, _, _) =>
        validIf(ComponentValidator.validateChoice(fieldValue)(data))
    }
  }

  //TODO: this will be called many times per one form. Maybe there is a way to optimise it?
  private def validateFileUpload(data: FormDataRecalculated, fieldValue: FormComponent)(
    implicit hc: HeaderCarrier): Future[ValidatedType[Unit]] =
    fileUploadService
      .getEnvelope(envelopeId)
      .map { envelope =>
        val fileId = FileId(fieldValue.id.value)
        val file: Option[File] = envelope.files.find(_.fileId.value == fileId.value)

        file match {
          case Some(File(fileId, Error(Some(reason)), _)) => validationFailure(fieldValue, reason)
          case Some(File(fileId, Error(None), _)) =>
            validationFailure(fieldValue, "has an unknown error from file upload")
          case Some(File(fileId, Infected, _)) =>
            validationFailure(fieldValue, "has a virus detected")
          case Some(File(fileId, _, _))     => validationSuccess
          case None if fieldValue.mandatory => validationFailure(fieldValue, "must be uploaded")
          case None                         => validationSuccess
        }
      }
}

object ComponentsValidator {

  def validatorHelper(
    fieldValueConstraint: Int,
    fieldValue: FormComponent,
    value: String,
    min: Int,
    max: Int): Validated[Map[FormComponentId, Set[String]], Unit] =
    fieldValueConstraint match {
      case tooLong if tooLong > max =>
        validationFailure(fieldValue, s"has more than $max characters")
      case tooShort if tooShort < min =>
        validationFailure(fieldValue, s"has less than $min characters")
      case _ => validationSuccess
    }

  def validF(implicit ec: ExecutionContext) =
    validationSuccess.pure[Future]

  def errors(fieldValue: FormComponent, defaultErr: String): Set[String] =
    Set(
      localisation(
        fieldValue.errorMessage.getOrElse(
          messagePrefix(fieldValue, fieldValue.id, None) + " " + localisation(defaultErr))))

  def getError(
    fieldValue: FormComponent,
    defaultMessage: String): Validated[Map[FormComponentId, Set[String]], Nothing] =
    Map(fieldValue.id -> errors(fieldValue, localisation(defaultMessage))).invalid

  def messagePrefix(fieldValue: FormComponent, workedOnId: FormComponentId, otherFormComponent: Option[FormComponent]) =
    otherFormComponent match {
      case Some(x) if x.id === workedOnId => localisation(x.shortName.getOrElse(x.label))
      case Some(x)                        => localisation(fieldValue.shortName.getOrElse(fieldValue.label))
      case None                           => localisation(fieldValue.shortName.getOrElse(fieldValue.label))
    }

  def validateRF(fieldValue: FormComponent, value: String) =
    validateRequired(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateFF(fieldValue: FormComponent, value: String) =
    validateForbidden(fieldValue, fieldValue.id.withSuffix(value)) _

  def validateRequired(fieldValue: FormComponent, fieldId: FormComponentId, errorPrefix: Option[String] = None)(
    xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil =>
        Map(fieldId -> errors(fieldValue, s"${errorPrefix.getOrElse("")} must be entered")).invalid
      case value :: Nil  => validationSuccess
      case value :: rest => validationSuccess // we don't support multiple values yet
    }

  def validateForbidden(fieldValue: FormComponent, fieldId: FormComponentId)(xs: Seq[String]): ValidatedType[Unit] =
    xs.filterNot(_.isEmpty()) match {
      case Nil => validationSuccess
      case value :: Nil =>
        Map(fieldId -> errors(fieldValue, "must not be entered")).invalid
      case value :: rest =>
        Map(fieldId -> errors(fieldValue, "must not be entered")).invalid // we don't support multiple values yet
    }
}
