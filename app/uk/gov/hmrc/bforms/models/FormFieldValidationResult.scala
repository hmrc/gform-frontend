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

import cats.instances.either._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.either._

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

  def getOptionalCurrentValue(key: String): Option[String] = this match{
    case ComponentField(_, data) => data.get(key).flatMap(_.getCurrentValue)
    case _ => None
  }

  def getCurrentValue(key: String): String = this match {
    case ComponentField(_, data) => data.get(key).flatMap(_.getCurrentValue).getOrElse("")
    case _ => ""
  }

  def toFormField: Either[Unit, List[FormField]] = this match {
    case FieldOk(fieldValue, cv) => Right(List(FormField(fieldValue.id, cv)))
    case ComponentField(fieldValue, data) =>
      data.map { case (suffix, value) => value.toFormField.map(_.map(_.withSuffix(suffix))) }.toList.sequenceU.map(_.flatten)
    case _ => Left(())
  }

  def toFormFieldTolerant: FormField = this match {
    case FieldOk(fieldValue, cv) => FormField(fieldValue.id, cv)
    case RequiredField(fieldValue) => FormField(fieldValue.id, "")
    case WrongFormat(fieldValue) => FormField(fieldValue.id, "")
    case ComponentField(fieldValue, _) => FormField(fieldValue.id, "")
  }
}

sealed abstract class DateError
final case class DayViolation(id: FieldId, message: String) extends DateError
final case class BeforeDateError(id: FieldId, message: String) extends DateError
final case class AfterDateError(id: FieldId, message: String) extends DateError

case class FieldOk(fieldValue: FieldValue, currentValue: String) extends FormFieldValidationResult
case class RequiredField(fieldValue: FieldValue) extends FormFieldValidationResult
case class WrongFormat(fieldValue: FieldValue) extends FormFieldValidationResult
case class ComponentField(fieldValue: FieldValue, data: Map[String, FormFieldValidationResult]) extends FormFieldValidationResult
