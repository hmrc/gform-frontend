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

package uk.gov.hmrc.bforms.models

import java.time.LocalDate

import cats.data.Validated.{Invalid, Valid}
import cats.data.Validated
import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._
import uk.gov.hmrc.bforms.models.components.{Address, _}
import uk.gov.hmrc.bforms.models.form.FormField

sealed trait FormFieldValidationResult {
  def isOk: Boolean = this match {
    case FieldOk(_, _) => true
    case ComponentField(_, data) => data.values.forall(_.isOk)
    case _ => false
  }

  def getCurrentValue: Option[String] = this match {
    case FieldOk(_, "") => None
    case FieldOk(_, cv) => Some(cv)
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
    case ComponentField(fieldValue, data) =>
      fieldValue `type` match {
        case Choice(_, _, _, _) => Right(List(FormField(fieldValue.id, data.keys.map(_.replace(fieldValue.id.value, "")).mkString(","))))
        case _ => data.map { case (suffix, value) => value.toFormField.map(_.map(_.withSuffix(suffix))) }.toList.sequenceU.map(_.flatten)
      }

    case _ => Left(())
  }

  def toFormFieldTolerant: List[FormField] = this match {
    case FieldOk(fieldValue, cv) => List(FormField(fieldValue.id, cv))
    case FieldError(fieldValue, _, _) => List(FormField(fieldValue.id, ""))
    case ComponentField(fieldValue, data) => List(FormField(fieldValue.id, ""))
      data.flatMap { case (suffix, value) => value.toFormFieldTolerant.map(_.withSuffix(suffix)) }.toList
  }
}

case class FieldOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult

case class FieldError(fieldValue: FieldValue, currentValue: String, errors: Set[String]) extends FormFieldValidationResult

case class ComponentField(fieldValue: FieldValue, data: Map[String, FormFieldValidationResult]) extends FormFieldValidationResult

object ValidationUtil {

  type GFormError = Map[FieldId, Set[String]]
  type ValidatedLocalDate = Validated[GFormError, LocalDate]
  type ValidatedNumeric = Validated[GFormError, Int]
  type ValidatedConcreteDate = Validated[GFormError, ConcreteDate]

  type ValidatedType = Validated[GFormError, Unit]

  def f(allFields: List[FieldValue], validationResult: ValidatedType, data: Map[FieldId, Seq[String]]):
  Either[List[FormFieldValidationResult], List[FormFieldValidationResult]] = {

    val dataGetter: FieldValue => Seq[String] = fv => data.get(fv.id).toList.flatten

    val gFormErrors = validationResult match {
      case Invalid(errors) =>
        errors

      case Valid(()) => Map.empty[FieldId, Set[String]]
    }

    val resultErrors: List[FormFieldValidationResult] = allFields.map { fieldValue =>

      fieldValue.`type` match {
        case Address =>

          val formFieldValRes: List[(FieldId, FormFieldValidationResult)] = Address.fields(fieldValue.id).map { fieldId =>

            gFormErrors.get(fieldId) match {
              case Some(errors) => (fieldId, FieldError(fieldValue, dataGetter(fieldValue).headOption.getOrElse(""), errors))
              case None => (fieldId, FieldOk(fieldValue, dataGetter(fieldValue).headOption.getOrElse("")))
            }

          }
          val dataMap = formFieldValRes.map { kv =>

            kv._1.getSuffix(fieldValue.id) -> kv._2

          }.toMap

          ComponentField(fieldValue, dataMap)

        case Date(_, _, _) =>
          val formFieldValRes: List[(FieldId, FormFieldValidationResult)] = Date.fields(fieldValue.id).map { fieldId =>

            gFormErrors.get(fieldId) match {
              case Some(errors) => (fieldId, FieldError(fieldValue, dataGetter(fieldValue).headOption.getOrElse(""), errors))
              case None => (fieldId, FieldOk(fieldValue, dataGetter(fieldValue).headOption.getOrElse("")))
            }

          }
          val dataMap = formFieldValRes.map { kv =>

            kv._1.getSuffix(fieldValue.id) -> kv._2

          }.toMap

          ComponentField(fieldValue, dataMap)

        case Text(_) =>

          val fieldId = fieldValue.id

          gFormErrors.get(fieldId) match {
            case Some(errors) => FieldError(fieldValue, dataGetter(fieldValue).headOption.getOrElse(""), errors)
            case None => FieldOk(fieldValue, dataGetter(fieldValue).headOption.getOrElse(""))
          }

        case Choice(_, _, _, _) => FieldOk(fieldValue, "") // fix me

      }

    }

    validationResult match {
      case Invalid(_) => Left(resultErrors)
      case Valid(()) => Right(resultErrors)
    }
  }

}





