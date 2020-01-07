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

import java.time.LocalDate

import cats.implicits._
import cats.Monoid
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import play.api.Logger
import uk.gov.hmrc.gform.eval.smartstring._
import uk.gov.hmrc.gform.fileupload.{ Envelope, Error, File, Other, Quarantined }
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, ValidationResult }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ValidationUtil {

  type GformError = Map[FormComponentId, Set[String]]
  type ValidatedLocalDate = Validated[GformError, LocalDate]
  type ValidatedNumeric = Validated[String, Int]
  type ValidatedConcreteDate = Validated[GformError, ConcreteDate]

  type ValidatedType[A] = Validated[GformError, A]

  val printErrors: Map[String, Set[String]] => Set[String] = (map: Map[String, Set[String]]) => {
    map.foldLeft(Set[String]())(_ ++ _._2)
  }

  def isFormValid(errors: Map[FormComponent, FormFieldValidationResult]): Boolean =
    errors.values.view.forall(_.isOk)

  def renderErrors(value: String, validationResult: FormFieldValidationResult): Map[String, Set[String]] =
    validationResult match {
      case FieldError(fv, _, errors)      => Map(value -> errors)
      case FieldGlobalError(_, _, errors) => Map(value -> errors)
      case ComponentField(_, compData) =>
        compData.flatMap(kv => renderErrors(kv._1, kv._2))

      case _ => Map.empty[String, Set[String]]
    }

  def evaluateWithSuffix(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(
    dGetter: FormComponentId => Seq[String]): List[(FormComponentId, FormFieldValidationResult)] =
    fieldValue.`type` match {
      case UkSortCode(_) =>
        UkSortCode.fields(fieldValue.id).toList.map { fieldId =>
          gformErrors.get(fieldId) match {
            case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
            case None         => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
          }
        }
      case Address(_) =>
        Address.fields(fieldValue.id).toList.map { fieldId =>
          gformErrors.get(fieldId) match {
            case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
            case None         => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
          }
        }

      case Date(_, _, _) =>
        Date.fields(fieldValue.id).toList.map { fieldId =>
          gformErrors.get(fieldId) match {
            case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
            case None         => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
          }
        }
      case _: Choice | _: RevealingChoice | _: FileUpload | _: Group | _: InformationMessage | _: Text | _: TextArea |
          _: HmrcTaxPeriod =>
        List.empty[(FormComponentId, FormFieldValidationResult)]
    }

  def evaluateWithoutSuffix(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(
    dGetter: FormComponentId => Seq[String]): (FormComponentId, FormFieldValidationResult) =
    gformErrors.get(fieldValue.id) match {
      //without suffix
      case Some(errors) =>
        (fieldValue.id, FieldGlobalError(fieldValue, dGetter(fieldValue.id).headOption.getOrElse(""), errors))
      case None => (fieldValue.id, FieldGlobalOk(fieldValue, dGetter(fieldValue.id).headOption.getOrElse("")))
    }

  def evaluateValidationResult(
    atomicFields: List[FormComponent],
    validationResult: ValidatedType[ValidationResult],
    data: FormDataRecalculated,
    envelope: Envelope): List[FormFieldValidationResult] = {

    // ToDo: This should be removed and replaced with data.data.one or data.data.many as appropriate
    val dataGetter: FormComponentId => Seq[String] = fId => data.data.get(fId).toList.flatMap(_.toSeq)

    val gFormErrors = validationResult match {
      case Invalid(errors) => errors
      case Valid(_)        => Map.empty[FormComponentId, Set[String]]
    }
    def matchComponentType(fieldValue: FormComponent): FormFieldValidationResult = fieldValue.`type` match {
      case _: UkSortCode =>
        val valSuffixResult: List[(FormComponentId, FormFieldValidationResult)] =
          evaluateWithSuffix(fieldValue, gFormErrors)(dataGetter)
        val valWithoutSuffixResult: (FormComponentId, FormFieldValidationResult) =
          evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

        val dataMap = (valWithoutSuffixResult :: valSuffixResult).map { kv =>
          kv._1.value -> kv._2
        }.toMap

        ComponentField(fieldValue, dataMap)

      case _: Address =>
        val valSuffixResult: List[(FormComponentId, FormFieldValidationResult)] =
          evaluateWithSuffix(fieldValue, gFormErrors)(dataGetter)
        val valWithoutSuffixResult: (FormComponentId, FormFieldValidationResult) =
          evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

        val dataMap = (valWithoutSuffixResult :: valSuffixResult).map { kv =>
          kv._1.value -> kv._2
        }.toMap

        ComponentField(fieldValue, dataMap)

      case _: Date =>
        val valSuffixResult: List[(FormComponentId, FormFieldValidationResult)] =
          evaluateWithSuffix(fieldValue, gFormErrors)(dataGetter)

        val valWithoutSuffixResult: (FormComponentId, FormFieldValidationResult) =
          evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

        val dataMap = (valWithoutSuffixResult :: valSuffixResult).map { kv =>
          kv._1.value -> kv._2
        }.toMap

        ComponentField(fieldValue, dataMap)

      case IsTextOrTextArea(constraint) =>
        val data = constraint match {
          case UkVrn | CompanyRegistrationNumber | EORI | UkEORI =>
            dataGetter(fieldValue.id).headOption.getOrElse("").replace(" ", "")
          case _ => dataGetter(fieldValue.id).headOption.getOrElse("")
        }
        gFormErrors
          .get(fieldValue.id)
          .fold[FormFieldValidationResult](
            FieldOk(fieldValue, data)
          )(errors => FieldError(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""), errors))

      case _: Group =>
        FieldOk(fieldValue, "") //nothing to validate for group (TODO - review)

      case _: Choice | _: RevealingChoice =>
        gFormErrors.get(fieldValue.id) match {
          case Some(errors) =>
            FieldError(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""), errors)
          case None =>
            val optionalData = data.data.many(fieldValue.id).map { selectedValue =>
              selectedValue.map { index =>
                fieldValue.id.value + index -> FieldOk(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""))
              }.toMap
            }
            ComponentField(fieldValue, optionalData.getOrElse(Map.empty))
        }

      case _: FileUpload =>
        val fileName =
          envelope.files.find(_.fileId.value == fieldValue.id.value).map(_.fileName).getOrElse("Upload document")
        gFormErrors.get(fieldValue.id) match {
          case Some(errors) => FieldError(fieldValue, fileName, errors)
          case None         => FieldOk(fieldValue, fileName)
        }

      case _: InformationMessage => FieldOk(fieldValue, "")

      case _: HmrcTaxPeriod =>
        gFormErrors.get(fieldValue.id) match {
          case Some(errors) =>
            FieldError(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""), errors)
          case None =>
            val optionalData = data.data.one(fieldValue.id).map { selectedValue =>
              selectedValue.map { _ =>
                fieldValue.id.value -> FieldOk(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""))
              }.toMap

            }
            ComponentField(fieldValue, optionalData.getOrElse(Map.empty))
        }
    }

    val resultErrors: List[FormFieldValidationResult] = atomicFields.map { fieldValue =>
      matchComponentType(fieldValue)
    }
    resultErrors
  }

  private final object IsTextOrTextArea {
    def unapply(expr: ComponentType): Option[TextConstraint] =
      expr match {
        case Text(constraint, _, _, _)  => Some(constraint)
        case TextArea(constraint, _, _) => Some(constraint)
        case _                          => None
      }
  }

  def validateFileUploadHasScannedFiles(fieldValues: List[FormComponent], e: Envelope)(
    implicit l: LangADT,
    sse: SmartStringEvaluator): Validated[GformError, ValidationResult] = {
    val fileUploads: Map[FormComponentId, FormComponent] = fieldValues.collect {
      case fv @ FormComponent(id, _: FileUpload, _, _, _, _, _, _, _, _, _, _, _, _) => id -> fv
    }.toMap

    //TODO: below code was borrowed from components validator. make it reusable in ValidationUtil
    def errors(fieldValue: FormComponent, defaultErr: String): Set[String] =
      Set(fieldValue.errorMessage.map(localisedString => localisedString.value).getOrElse(defaultErr))
    def getError(
      fieldValue: FormComponent,
      defaultMessage: String): Validated[Map[FormComponentId, Set[String]], Nothing] =
      Map(fieldValue.id -> errors(fieldValue, defaultMessage)).invalid

    val flakies: Seq[ValidatedType[ValidationResult]] = e.files
      .collect {
        case f @ File(_, Quarantined, _) =>
          //not processed (scanned by virus scanner) files are in quarantined state
          (f, "File has not been processed, please wait and try again")
        case f @ File(_, s: Other, _) =>
          val message = s"Internal server problem. Please contact support. (Unsupported state from FU: $s)"
          Logger.error(message)
          (f, message)
        case f @ File(_, s: Error, _) =>
          val message = s"Internal server problem. Please contact support. (Error state from FU: $s)"
          Logger.error(message)
          (f, message)
      }
      .map { fs =>
        val fieldValue = fieldValues
          .find(_.id == fs._1.fileId.toFieldId)
          .getOrElse(throw new UnexpectedStateException(
            s"Looks like there are more files in the envelope than we expected to have. Could not find 'FieldValue' to corresponding file: $fs"))
        getError(fieldValue, fs._2)
      }
    Monoid[ValidatedType[ValidationResult]].combineAll(flakies)
  }
}
