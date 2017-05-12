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

import cats.data.Validated.Valid
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.components._
import uk.gov.hmrc.gform.models.form.FormField

object Fields {


  def okValues(formFieldMap: Map[FieldId, Seq[String]], fieldValues: List[FieldValue])
              (fieldValue: FieldValue) : Option[FormFieldValidationResult] = {
    val formFields = toFormField(formFieldMap, fieldValues).map(hf => hf.id -> hf).toMap
    fieldValue.`type` match {
      case Address | Date(_, _, _) =>
        val fieldOkData =
          formFields.filter {
            case (fieldId, formField) => fieldId.value.startsWith(fieldValue.id.value) // Get just fieldIds related to fieldValue
          }.map {
            case (fieldId, formField) => fieldId.value.replace(fieldValue.id + ".", "") -> FieldOk(fieldValue, formField.value)
          }
        Some(ComponentField(fieldValue, fieldOkData))
      case Text(_, _) => formFields.get(fieldValue.id).map { formField =>
        FieldOk(fieldValue, formField.value)
      }
      case Choice(_, _, _, _, _) =>
        val fieldId = fieldValue.id
        val fieldOks = formFields.get(fieldId).map { formField =>
          val selections = formField.value.split(",").toList
          selections.map(selectedIndex => fieldId.value + selectedIndex -> FieldOk(fieldValue, selectedIndex)).toMap
        }
        fieldOks.map(data => ComponentField(fieldValue, data))
    }
  }

  def toFormField(formFields: Map[FieldId, Seq[String]], fieldValue: List[FieldValue]) : List[FormField] = {

    val getFormFieldValue: FieldId => FormField = fieldId => {
      val value = formFields.get(fieldId).toList.flatten.headOption.getOrElse("")
      FormField(fieldId, value)
    }

    fieldValue.flatMap { fv =>
      fv.`type` match {
        case Address => Address.fields(fv.id).map(getFormFieldValue)
        case Date(_, _, _) => Date.fields(fv.id).map(getFormFieldValue)
        case Text(_, _) | Choice(_, _, _, _,_) => List(getFormFieldValue(fv.id))
      }
    }
  }
}
