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

package uk.gov.hmrc.gform.models

import java.time.LocalDate

import cats.data.Validated.{ Invalid, Valid }
import uk.gov.hmrc.gform.models.components.{ Address, FieldId, FieldValue, _ }
import cats.data.Validated
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._
import uk.gov.hmrc.gform.fileupload.{ Envelope, File }
import uk.gov.hmrc.gform.gformbackend.model.FormField

sealed trait FormFieldValidationResult {
  def isOk: Boolean = this match {
    case FieldOk(_, _) => true
    case FieldGlobalOk(_, _) => true
    case ComponentField(_, data) => data.values.forall(_.isOk)
    case _ => false
  }

  def getCurrentValue: Option[String] = this match {
    case FieldOk(_, "") => None
    case FieldOk(_, cv) => Some(cv)
    case FieldError(_, cv, _) => Some(cv)
    case _ => None
  }

  def getOptionalCurrentValue(key: String): Option[String] = this match {
    case ComponentField(_, data) => data.get(key).flatMap(_.getCurrentValue)
    case _ => None
  }

  def getCurrentValue(key: String): String = this match {
    case ComponentField(_, data) => data.get(key).flatMap(_.getCurrentValue).getOrElse("")
    case _ => ""
  }

  /**
   * If `this` field is not ok, we want to indicate error by using Left(())
   */
  def toFormField: Either[Unit, List[FormField]] = this match {
    case FieldOk(fieldValue, cv) => Right(List(FormField(fieldValue.id, cv)))
    case FieldGlobalError(_, _, _) => Right(List.empty[FormField])
    case FieldGlobalOk(_, _) => Right(List.empty[FormField])
    case ComponentField(fieldValue, data) =>
      fieldValue `type` match {
        case Address(_) => toAddressFormField(data)
        case Choice(_, _, _, _, _) => Right(List(FormField(fieldValue.id, data.keys.map(_.replace(fieldValue.id.value, "")).mkString(","))))
        case _ => data.map { case (suffix, value) => value.toFormField.map(_.map(_.withSuffix(suffix))) }.toList.sequenceU.map(_.flatten)
      }

    case _ => Left(())
  }

  def toFormFieldTolerant: List[FormField] = this match {
    case FieldOk(fieldValue, cv) => List(FormField(fieldValue.id, cv))
    case FieldError(fieldValue, _, _) => List(FormField(fieldValue.id, ""))
    case FieldGlobalError(_, _, _) => List.empty[FormField]
    case FieldGlobalOk(_, _) => List.empty[FormField]
    case ComponentField(fieldValue, data) =>
      List(FormField(fieldValue.id, ""))
      data.flatMap { case (suffix, value) => value.toFormFieldTolerant.map(_.withSuffix(suffix)) }.toList
  }

  private def toAddressFormField(data: Map[String, FormFieldValidationResult]) = {
    data.map {
      case (fieldName, formFieldValidationResult) => formFieldValidationResult.toFormField.map { formField =>
        formField.map { _.copy(id = FieldId(fieldName)) }
      }
    }.toList.sequenceU.map(_.flatten)
  }
}

case class FieldOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult

case class FieldGlobalOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult

case class FieldError(fieldValue: FieldValue, currentValue: String, errors: Set[String]) extends FormFieldValidationResult

case class FieldGlobalError(fieldValue: FieldValue, currentValue: String, errors: Set[String]) extends FormFieldValidationResult

case class ComponentField(fieldValue: FieldValue, data: Map[String, FormFieldValidationResult]) extends FormFieldValidationResult

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
      case FieldError(_, _, errors) => Map(value -> errors)
      case FieldGlobalError(_, _, errors) => Map(value -> errors)
      case ComponentField(_, compData) =>
        compData.flatMap(kv => renderErrors(kv._1, kv._2))

      case _ => Map.empty[String, Set[String]]
    }
  }

  def evaluateWithSuffix[t <: ComponentType](component: ComponentType, fieldValue: FieldValue, gformErrors: Map[FieldId, Set[String]])(dGetter: (FieldId) => Seq[String]): List[(FieldId, FormFieldValidationResult)] = {
    component match {
      case Address(_) => Address.allFieldIds(fieldValue.id).map { fieldId =>

        gformErrors.get(fieldId) match {
          //with suffix
          case Some(errors) => (fieldId, FieldError(fieldValue, dGetter(fieldId).headOption.getOrElse(""), errors))
          case None => (fieldId, FieldOk(fieldValue, dGetter(fieldId).headOption.getOrElse("")))
        }
      }

      case Date(_, _, _) => Date.allFieldIds(fieldValue.id).map { fieldId =>

        gformErrors.get(fieldId) match {
          //with suffix
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
      case Invalid(errors) =>
        errors

      case Valid(()) => Map.empty[FieldId, Set[String]]
    }

    val resultErrors: List[FormFieldValidationResult] = atomicFields.map { fieldValue =>

      fieldValue.`type` match {
        case address @ Address(_) =>

          val valSuffixResult: List[(FieldId, FormFieldValidationResult)] = evaluateWithSuffix(address, fieldValue, gFormErrors)(dataGetter)
          val valWithoutSuffixResult: (FieldId, FormFieldValidationResult) = evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

          val dataMap = (valWithoutSuffixResult :: valSuffixResult)
            .map { kv => kv._1.getSuffix(fieldValue.id) -> kv._2
            }.toMap

          ComponentField(fieldValue, dataMap)

        case date @ Date(_, _, _) =>

          val valSuffixResult: List[(FieldId, FormFieldValidationResult)] = evaluateWithSuffix(date, fieldValue, gFormErrors)(dataGetter)

          val valWithoutSuffixResult: (FieldId, FormFieldValidationResult) = evaluateWithoutSuffix(fieldValue, gFormErrors)(dataGetter)

          val dataMap = (valWithoutSuffixResult :: valSuffixResult)
            .map { kv => kv._1.getSuffix(fieldValue.id) -> kv._2
            }.toMap

          ComponentField(fieldValue, dataMap)

        case Text(_, _) =>

          val fieldId = fieldValue.id

          gFormErrors.get(fieldId) match {
            case Some(errors) => FieldError(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""), errors)
            case None => FieldOk(fieldValue, dataGetter(fieldValue.id).headOption.getOrElse(""))
          }

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
          val fileName = envelope.files.find(_.fileId.value == fieldValue.id.value).map(_.fileName).getOrElse("please upload file")
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
