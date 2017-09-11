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

import uk.gov.hmrc.gform.models._
import cats.implicits._

import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ Choice, FieldId, FieldValue }

case class FieldOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult
case class FieldGlobalOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult
case class FieldError(fieldValue: FieldValue, currentValue: String, errors: Set[String]) extends FormFieldValidationResult
case class FieldGlobalError(fieldValue: FieldValue, currentValue: String, errors: Set[String]) extends FormFieldValidationResult
case class ComponentField(fieldValue: FieldValue, data: Map[String, FormFieldValidationResult]) extends FormFieldValidationResult

trait FormFieldValidationResult {

  lazy val fieldErrors: Set[String] = this match {
    case e: FieldError => e.errors
    case cf: ComponentField => cf.data.values.foldLeft[Set[String]](Set())(_ ++ _.fieldErrors)
    case _ => Set()
  }

  lazy val fieldErrorsByField: Map[FieldValue, Set[String]] = this match {
    case e: FieldError => Map(fieldValue -> e.errors)
    case cf: ComponentField =>
      cf.data.values.foldLeft[Map[FieldValue, Set[String]]](Map())(_ |+| _.fieldErrorsByField)
    case _ => Map()
  }

  lazy val globalErrors: Set[String] = this match {
    case e: FieldGlobalError => e.errors
    case cf: ComponentField => cf.data.values.foldLeft[Set[String]](Set())(_ ++ _.globalErrors)
    case _ => Set()
  }

  def fieldValue: FieldValue

  def isOk: Boolean = this match {
    case FieldOk(_, _) => true
    case FieldGlobalOk(_, _) => true
    case ComponentField(_, data) => data.values.forall(_.isOk)
    case _ => false
  }

  def isNotOk: Boolean = !isOk

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

  private def withId(f: FormField, id: String) = f.copy(FieldId(id))

  def toFormField: List[FormField] = this match {
    case FieldOk(fieldValue, cv) => List(FormField(fieldValue.id, cv))
    case FieldError(fieldValue, cv, _) => List(FormField(fieldValue.id, cv))
    case FieldGlobalError(fieldValue, cv, _) => List(FormField(fieldValue.id, cv))
    case FieldGlobalOk(fieldValue, cv) => List(FormField(fieldValue.id, cv))
    case ComponentField(fieldValue, data) =>
      fieldValue.`type` match {
        case c: Choice => List(FormField(fieldValue.id, data.keys.map(_.replace(fieldValue.id.value, "")).mkString(",")))
        case _ => data.flatMap { case (suffix, value) => value.toFormField.map(withId(_, suffix)) }.toList
      }
  }

}

