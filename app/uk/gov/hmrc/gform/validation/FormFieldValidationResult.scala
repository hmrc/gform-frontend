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

import cats.implicits._
import uk.gov.hmrc.gform.models.Atom
import uk.gov.hmrc.gform.models.ids.ModelComponentId
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponent
import uk.gov.hmrc.gform.sharedmodel.formtemplate.FormComponentId
import scala.collection.mutable.LinkedHashSet

case class FieldOk(formComponent: FormComponent, currentValue: String) extends FormFieldValidationResult
case class FieldGlobalOk(formComponent: FormComponent, currentValue: String) extends FormFieldValidationResult
case class FieldError(formComponent: FormComponent, currentValue: String, errors: LinkedHashSet[String])
    extends FormFieldValidationResult
case class FieldGlobalError(formComponent: FormComponent, currentValue: String, errors: LinkedHashSet[String])
    extends FormFieldValidationResult
case class ComponentField(
  formComponent: FormComponent,
  data: Map[HtmlFieldId, FormFieldValidationResult]
) // Used by multivalue fields ie. date, address but also choice and revealingChoice
    extends FormFieldValidationResult

trait FormFieldValidationResult {

  def forgetErrors: FormFieldValidationResult = this match {
    case t: FieldOk          => t
    case t: FieldGlobalOk    => t
    case t: FieldError       => FieldOk(t.formComponent, t.currentValue)
    case t: FieldGlobalError => FieldGlobalOk(t.formComponent, t.currentValue)
    case t: ComponentField   => ComponentField(t.formComponent, t.data.view.mapValues(_.forgetErrors).toMap)
    case t                   => t
  }

  lazy val fieldErrors: LinkedHashSet[String] = this match {
    case e: FieldError                 => e.errors
    case globalError: FieldGlobalError => globalError.errors
    case cf: ComponentField            => cf.data.values.foldLeft[LinkedHashSet[String]](LinkedHashSet())(_ ++ _.fieldErrors)
    case _                             => LinkedHashSet()
  }

  lazy val fieldErrorsByFieldValue: List[LinkedHashSet[String]] = this match {
    case e: FieldError => List(e.errors)
    case cf: ComponentField =>
      cf.data.values.toList.flatMap(_.fieldErrorsByFieldValue)
    case _ => Nil
  }

  def fieldErrorsWithSuffix(atom: Atom): LinkedHashSet[String] = this match {
    case ComponentField(formComponent, data) =>
      val modelComponentId: ModelComponentId = formComponent.atomicFormComponentId(atom)
      data
        .get(HtmlFieldId.Pure(modelComponentId))
        .map(_.fieldErrors)
        .getOrElse(LinkedHashSet())
    case _ => LinkedHashSet()
  }

  lazy val globalErrors: LinkedHashSet[String] = this match {
    case e: FieldGlobalError => e.errors
    case cf: ComponentField  => cf.data.values.foldLeft[LinkedHashSet[String]](LinkedHashSet())(_ ++ _.globalErrors)
    case _                   => LinkedHashSet()
  }

  def formComponent: FormComponent

  def forHtmlFieldId(htmlFieldId: HtmlFieldId): FormFieldValidationResult = this match {
    case ComponentField(id, data) =>
      data.get(htmlFieldId).getOrElse(ComponentField(id, Map.empty[HtmlFieldId, FormFieldValidationResult]))
    case unsupported => throw new Exception("Wrong invocation. Expected 'ComponentField', but got: " + unsupported)
  }

  def isOk: Boolean = this match {
    case FieldOk(_, _)           => true
    case FieldGlobalOk(_, _)     => true
    case ComponentField(_, data) => data.values.forall(_.isOk)
    case _                       => false
  }

  def isNotOk: Boolean = !isOk

  def getCurrentValue: Option[String] = this match {
    case FieldOk(_, "")       => None
    case FieldOk(_, cv)       => Some(cv)
    case FieldError(_, cv, _) => Some(cv)
    case _                    => None
  }

  def getOptionalCurrentValue(key: HtmlFieldId): Option[String] =
    this match {
      case ComponentField(_, data) => data.get(key).flatMap(_.getCurrentValue)
      case _                       => None
    }

  def getCurrentValue(key: HtmlFieldId): String = getOptionalCurrentValue(key).getOrElse("")

  def getComponentFieldIndices(formComponentId: FormComponentId): List[String] =
    this match {
      case ComponentField(_, data) =>
        data
          .collect {
            case (HtmlFieldId.Indexed(fcId, index), _) if fcId === formComponentId => index
          }
          .toList
          .sorted
      case _ => Nil
    }

  // Construct List[FormField] to be stored in MongoDB
  def convertToFormField: List[FormField] = this match {
    case FieldOk(formComponent, cv)             => List(FormField(formComponent.modelComponentId, cv))
    case FieldError(formComponent, cv, _)       => List(FormField(formComponent.modelComponentId, cv))
    case FieldGlobalError(formComponent, cv, _) => List(FormField(formComponent.modelComponentId, cv))
    case FieldGlobalOk(formComponent, cv)       => List(FormField(formComponent.modelComponentId, cv))
    case ComponentField(formComponent, data) =>
      val indexed: List[HtmlFieldId.Indexed] = data.collect { case (htmlFieldId: HtmlFieldId.Indexed, _) =>
        htmlFieldId
      }.toList
      val indices = indexed.map(_.index)
      val xs1: List[FormField] =
        if (indices.isEmpty) Nil else List(FormField(formComponent.modelComponentId, indices.mkString(",")))

      val pures: Map[HtmlFieldId.Pure, FormFieldValidationResult] = data.collect {
        case (htmlFieldId: HtmlFieldId.Pure, ffvr) => (htmlFieldId, ffvr)
      }

      val xs2: List[FormField] =
        pures.flatMap { case (htmlFieldId, ffvr) =>
          ffvr.convertToFormField.map(withId(_, htmlFieldId.modelComponentId))
        }.toList

      xs1 ++ xs2
    case other => throw new Exception(s"Unsupported FormField - $other")
  }

  private def withId(f: FormField, id: ModelComponentId) = f.copy(id = id)
}
