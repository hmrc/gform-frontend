/*
 * Copyright 2017 HM Revenue & Customs
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

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

object ValidationUtil {

  type GformError = Map[FieldId, Set[String]]
  type ValidatedLocalDate = Validated[GformError, LocalDate]
  type ValidatedNumeric = Validated[String, Int]
  type ValidatedConcreteDate = Validated[GformError, ConcreteDate]

  type ValidatedType = Validated[GformError, Unit]

  val printErrors: (Map[String, Set[String]]) => Set[String] = (map: Map[String, Set[String]]) => {
    map.foldLeft(Set[String]())(_ ++ _._2)
  }

  def renderErrors(value: String, validationResult: FormFieldValidationResult): Map[String, Set[String]] = {

    validationResult match {
      case FieldError(fv, _, errors) => Map(fv.id.value -> errors)
      case FieldGlobalError(_, _, errors) => Map(value -> errors)
      case ComponentField(_, compData) =>
        compData.flatMap(kv => renderErrors(kv._1, kv._2))

      case _ => Map.empty[String, Set[String]]
    }
  }

  def evaluateWithSuffix(fieldValue: FieldValue, gformErrors: Map[FieldId, Set[String]])(dGetter: (FieldId) => Seq[String]): List[(FieldId, FormFieldValidationResult)] = {
    fieldValue.`type` match {
      case UkSortCode(_) => UkSortCode.fields(fieldValue.id).map { fieldId =>
        gformErrors.get(fieldId) match {
          case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
          case None => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
        }
      }
      case Address(_) => Address.fields(fieldValue.id).map { fieldId =>

        gformErrors.get(fieldId) match {
          case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
          case None => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
        }
      }

      case Date(_, _, _) => Date.fields(fieldValue.id).map { fieldId =>
        gformErrors.get(fieldId) match {
          case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
          case None => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
        }
      }
      case Choice(_, _, _, _, _) | FileUpload() | Group(_, _, _, _, _, _) | InformationMessage(_, _) | Text(_, _) =>
        List[(FieldId, FormFieldValidationResult)]()
    }
  }

  def evaluateWithoutSuffix(fieldValue: FieldValue, gformErrors: Map[FieldId, Set[String]])(dGetter: (FieldId) => Seq[String]): (FieldId, FormFieldValidationResult) = {

    gformErrors.get(fieldValue.id) match {
      //without suffix
      case Some(errors) => (fieldValue.id, FieldGlobalError(fieldValue, dGetter(fieldValue.id).headOption.getOrElse(""), errors))
      case None => (fieldValue.id, FieldGlobalOk(fieldValue, dGetter(fieldValue.id).headOption.getOrElse("")))
    }
  }

  def evaluateValidationResult(atomicFields: List[FieldValue], validationResult: ValidatedType, data: Map[FieldId, Seq[String]],
    envelope: Envelope): Either[List[FormFieldValidationResult], List[FormFieldValidationResult]] = {

    val dataGetter: FieldId => Seq[String] = fId => data.get(fId).toList.flatten

    val gFormErrors = validationResult match {
      case Invalid(errors) => errors
      case Valid(()) => Map.empty[FieldId, Set[String]]
    }

    val resultErrors: List[FormFieldValidationResult] = atomicFields.map { fieldValue =>

      fieldValue.`type` match {
        case sortCode @ UkSortCode(_) =>
          val valSuffixResult: List[(FieldId, FormFieldValidationResult)] = evaluateWithSuffix(fieldValue, gFormErrors)(dataGetter)
          val valWithoutSuffixResult: (FieldId, FormFieldValidationResult) = evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

          val dataMap = (valWithoutSuffixResult :: valSuffixResult)
            .map { kv => kv._1.value -> kv._2 }.toMap

          ComponentField(fieldValue, dataMap)
        case address @ Address(_) =>

          val valSuffixResult: List[(FieldId, FormFieldValidationResult)] = evaluateWithSuffix(fieldValue, gFormErrors)(dataGetter)
          val valWithoutSuffixResult: (FieldId, FormFieldValidationResult) = evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

          val dataMap = (valWithoutSuffixResult :: valSuffixResult)
            .map { kv => kv._1.value -> kv._2 }.toMap

          ComponentField(fieldValue, dataMap)

        case date @ Date(_, _, _) =>

          val valSuffixResult: List[(FieldId, FormFieldValidationResult)] = evaluateWithSuffix(fieldValue, gFormErrors)(dataGetter)

          val valWithoutSuffixResult: (FieldId, FormFieldValidationResult) = evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

          val dataMap = (valWithoutSuffixResult :: valSuffixResult)
            .map { kv => kv._1.value -> kv._2 }.toMap

          ComponentField(fieldValue, dataMap)

        case Text(constraint, _) =>
          val data = constraint match {
            case UkVrn => dataGetter(fieldValue.id).headOption.getOrElse("").replace(" ", "")
            case _ => dataGetter(fieldValue.id).headOption.getOrElse("")
          }
          gFormErrors
            .get(fieldValue.id)
            .fold[FormFieldValidationResult](
              FieldOk(fieldValue, data)
            )(errors => FieldError(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""), errors))
        case Group(_, _, _, _, _, _) => {

          FieldOk(fieldValue, "") //nothing to validate for group (TODO - review)

        }

        case Choice(_, _, _, _, _) =>

          gFormErrors.get(fieldValue.id) match {
            case Some(errors) => FieldError(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""), errors) // ""
            case None =>

              val optionalData = data.get(fieldValue.id).map { selectedValue =>

                selectedValue.map { index =>

                  fieldValue.id.value + index -> FieldOk(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""))
                }.toMap

              }

              ComponentField(fieldValue, optionalData.getOrElse(Map.empty))
          }
        case FileUpload() => {
          val fileName = envelope.files.find(_.fileId.value == fieldValue.id.value).map(_.fileName).getOrElse("Upload document")
          gFormErrors.get(fieldValue.id) match {
            case Some(errors) => FieldError(fieldValue, fileName, errors)
            case None => FieldOk(fieldValue, fileName)
          }
        }
        case InformationMessage(_, infoText) => FieldOk(fieldValue, infoText)
      }

    }

    validationResult match {
      case Invalid(_) => Left(resultErrors)
      case Valid(()) => Right(resultErrors)
    }
  }

}
