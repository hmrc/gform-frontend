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

package uk.gov.hmrc.gform.models.helpers

import uk.gov.hmrc.gform.fileupload.Envelope
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.form.FormField
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponentId, _ }
import uk.gov.hmrc.gform.validation.ValidationUtil.ValidatedType
import uk.gov.hmrc.gform.validation._

object Fields {

  def evaluateWithSuffix(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(dGetter: (FormComponentId) => List[FormField]): List[(FormComponentId, FormFieldValidationResult)] = {
    val data: FormComponentId => String = id => dGetter(id).headOption.map(_.value).getOrElse("")
    fieldValue.`type` match {
      case UkSortCode(_) => UkSortCode.fields(fieldValue.id).map { fieldId =>
        gformErrors.get(fieldId) match {
          case Some(errors) => (fieldId, FieldError(fieldValue, data(fieldId), errors))
          case None => (fieldId, FieldOk(fieldValue, data(fieldId)))
        }
      }
      case Address(_) => Address.fields(fieldValue.id).map { fieldId =>

        gformErrors.get(fieldId) match {
          case Some(errors) => (fieldId, FieldError(fieldValue, data(fieldId), errors))
          case None => (fieldId, FieldOk(fieldValue, data(fieldId)))
        }
      }

      case Date(_, _, _) => Date.fields(fieldValue.id).map { fieldId =>
        gformErrors.get(fieldId) match {
          case Some(errors) => (fieldId, FieldError(fieldValue, data(fieldId), errors))
          case None => (fieldId, FieldOk(fieldValue, data(fieldId)))
        }
      }
      case FileUpload() | Group(_, _, _, _, _, _) | InformationMessage(_, _) | Text(_, _) =>
        List[(FormComponentId, FormFieldValidationResult)]()
    }
  }

  def evaluateWithoutSuffix(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(dGetter: (FormComponentId) => List[FormField]): (FormComponentId, FormFieldValidationResult) = {

    val data = dGetter(fieldValue.id).headOption.map(_.value).getOrElse("")
    gformErrors.get(fieldValue.id) match {
      //without suffix
      case Some(errors) => (fieldValue.id, FieldGlobalError(fieldValue, data, errors))
      case None => (fieldValue.id, FieldGlobalOk(fieldValue, data))
    }
  }

  def evaluateComponent(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(dGetter: (FormComponentId) => List[FormField]) =
    (evaluateWithoutSuffix(fieldValue, gformErrors)(dGetter) :: evaluateWithSuffix(fieldValue, gformErrors)(dGetter))
      .map(kv => kv._1.value -> kv._2).toMap

  def getValidationResult(formFieldMap: Map[FormComponentId, Seq[String]], fieldValues: List[FormComponent], envelope: Envelope, gformErrors: Map[FormComponentId, Set[String]])(fieldValue: FormComponent): Option[FormFieldValidationResult] = {
    val formFields: Map[FormComponentId, FormField] = toFormField(formFieldMap, fieldValues).filter(_.value.nonEmpty).map(hf => hf.id -> hf).toMap

    val dataGetter: FormComponentId => List[FormField] = fId => formFields.get(fId).toList

    def componentField(list: List[FormComponentId]) = {
      val data = evaluateComponent(fieldValue, gformErrors)(dataGetter)
      Some(ComponentField(fieldValue, data))
    }

    fieldValue.`type` match {
      case Address(_) => componentField(Address.fields(fieldValue.id))
      case Date(_, _, _) => componentField(Date.fields(fieldValue.id))
      case UkSortCode(_) => componentField(UkSortCode.fields(fieldValue.id))
      case Text(_, _) | Group(_, _, _, _, _, _) => formFields.get(fieldValue.id).map { formField =>
        gformErrors.get(fieldValue.id).fold[FormFieldValidationResult](FieldOk(fieldValue, formField.value))(errors => FieldError(fieldValue, formField.value, errors))
      }
      case Choice(_, _, _, _, _) => evalChoice(fieldValue, gformErrors)(dataGetter)
      case FileUpload() => formFields.get(fieldValue.id).map { formField =>
        val fileName = envelope.files.find(_.fileId.value == formField.id.value).map(_.fileName).getOrElse("")
        gformErrors.get(fieldValue.id).fold[FormFieldValidationResult](FieldOk(fieldValue, fileName))(errors => FieldError(fieldValue, fileName, errors))
      }
      case InformationMessage(_, _) => None
    }
  }

  def evalChoice(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(dGetter: (FormComponentId) => List[FormField]) = gformErrors.get(fieldValue.id) match {
    case None =>
      val data = dGetter(fieldValue.id).flatMap { formField =>
        formField.value.split(",").toList.map(selectedIndex => fieldValue.id.value + selectedIndex -> FieldOk(fieldValue, selectedIndex))
      }.toMap
      Some(ComponentField(fieldValue, data))
    case Some(errors) =>
      val data = dGetter(fieldValue.id).flatMap { formField =>
        formField.value.split(",").toList.map(selectedIndex => fieldValue.id.value + selectedIndex -> FieldError(fieldValue, selectedIndex, errors))
      }.toMap
      Some(ComponentField(fieldValue, data))
  }

  def toFormField(fieldData: Map[FormComponentId, Seq[String]], templateFields: List[FormComponent]): List[FormField] = {

    val getFieldData: FormComponentId => FormField = fieldId => {
      val value = fieldData.get(fieldId).toList.flatten.headOption.getOrElse("")
      FormField(fieldId, value)
    }

    def getFormFields(templateFields: List[FormComponent]): List[FormField] = templateFields.flatMap { fv =>
      fv.`type` match {
        case Group(_, _, _, _, _, _) =>
          require(true, "There shouldn't be Group fields here")
          Nil // For completion, there shouldn't be Groups here
        case Address(_) => Address.fields(fv.id).map(getFieldData)
        case Date(_, _, _) => Date.fields(fv.id).map(getFieldData)
        case UkSortCode(_) => UkSortCode.fields(fv.id).map(getFieldData)
        case Text(_, _) | Choice(_, _, _, _, _) => List(getFieldData(fv.id))
        case FileUpload() => List(getFieldData(fv.id))
        case InformationMessage(_, _) => List()
      }
    }

    getFormFields(templateFields)
  }
}
