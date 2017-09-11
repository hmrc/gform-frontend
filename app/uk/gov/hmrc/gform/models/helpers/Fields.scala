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
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation._

object Fields {

  def okValues(formFieldMap: Map[FieldId, Seq[String]], fieldValues: List[FieldValue], envelope: Envelope)(fieldValue: FieldValue): Option[FormFieldValidationResult] = {
    val formFields = toFormField(formFieldMap, fieldValues).map(hf => hf.id -> hf).toMap

    def componentField(list: List[FieldId]) = {
      val data = list.map { fieldId =>
        fieldId.value -> FieldOk(fieldValue, formFields.get(fieldId).map(_.value).getOrElse(""))
      }.toMap
      Some(ComponentField(fieldValue, data))
    }

    fieldValue.`type` match {
      case Address(_) => componentField(Address.fields(fieldValue.id))
      case Date(_, _, _) => componentField(Date.fields(fieldValue.id))
      case UkSortCode(_) => componentField(UkSortCode.fields(fieldValue.id))
      case Text(_, _) | Group(_, _, _, _, _, _) => formFields.get(fieldValue.id).map { formField =>
        FieldOk(fieldValue, formField.value)
      }
      case Choice(_, _, _, _, _) =>
        val fieldId = fieldValue.id
        val fieldOks = formFields.get(fieldId).map { formField =>
          val selections = formField.value.split(",").toList
          selections.map(selectedIndex => fieldId.value + selectedIndex -> FieldOk(fieldValue, selectedIndex)).toMap
        }
        fieldOks.map(data => ComponentField(fieldValue, data))

      case FileUpload() => formFields.get(fieldValue.id).map { formField =>
        val fileName = envelope.files.find(_.fileId.value == formField.id.value).map(_.fileName).getOrElse("")
        FieldOk(fieldValue, fileName)
      }
      case InformationMessage(_, _) => None
    }
  }

  def toFormField(fieldData: Map[FieldId, Seq[String]], templateFields: List[FieldValue]): List[FormField] = {

    val getFieldData: FieldId => FormField = fieldId => {
      val value = fieldData.get(fieldId).toList.flatten.headOption.getOrElse("")
      FormField(fieldId, value)
    }

    def getFormFields(templateFields: List[FieldValue]): List[FormField] = templateFields.flatMap { fv =>
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
