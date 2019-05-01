/*
 * Copyright 2019 HM Revenue & Customs
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
import uk.gov.hmrc.gform.lookup.LookupExtractors
import uk.gov.hmrc.gform.sharedmodel.form.{ FormDataRecalculated, FormField }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation._
import uk.gov.hmrc.gform.models.ExpandUtils._

object Fields {

  def evaluateWithSuffix(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(
    dGetter: FormComponentId => Option[FormField]): List[(FormComponentId, FormFieldValidationResult)] = {
    val data: FormComponentId => String = id => dGetter(id).headOption.map(_.value).getOrElse("")
    fieldValue.`type` match {
      case UkSortCode(_) =>
        UkSortCode.fields(fieldValue.id).toList.map { fieldId =>
          gformErrors.get(fieldId) match {
            case Some(errors) => (fieldId, FieldError(fieldValue, data(fieldId), errors))
            case None         => (fieldId, FieldOk(fieldValue, data(fieldId)))
          }
        }
      case Address(_) =>
        Address.fields(fieldValue.id).toList.map { fieldId =>
          gformErrors.get(fieldId) match {
            case Some(errors) => (fieldId, FieldError(fieldValue, data(fieldId), errors))
            case None         => (fieldId, FieldOk(fieldValue, data(fieldId)))
          }
        }

      case Date(_, _, _) =>
        Date.fields(fieldValue.id).toList.map { fieldId =>
          gformErrors.get(fieldId) match {
            case Some(errors) => (fieldId, FieldError(fieldValue, data(fieldId), errors))
            case None         => (fieldId, FieldOk(fieldValue, data(fieldId)))
          }
        }
      case FileUpload() | Group(_, _, _, _, _, _) | InformationMessage(_, _) | Text(_, _, _, _) | TextArea(_, _, _) |
          Choice(_, _, _, _, _) | RevealingChoice(_) | HmrcTaxPeriod(_, _, _) =>
        List[(FormComponentId, FormFieldValidationResult)]()
    }
  }

  def evaluateWithoutSuffix(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(
    dGetter: FormComponentId => Option[FormField]): (FormComponentId, FormFieldValidationResult) = {

    val data = dGetter(fieldValue.id).headOption.map(_.value).getOrElse("")
    gformErrors.get(fieldValue.id) match {
      //without suffix
      case Some(errors) => (fieldValue.id, FieldGlobalError(fieldValue, data, errors))
      case None         => (fieldValue.id, FieldGlobalOk(fieldValue, data))
    }
  }

  def evaluateComponent(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(
    dGetter: FormComponentId => Option[FormField]) =
    (evaluateWithoutSuffix(fieldValue, gformErrors)(dGetter) :: evaluateWithSuffix(fieldValue, gformErrors)(dGetter))
      .map(kv => kv._1.value -> kv._2)
      .toMap

  def getValidationResult(
    formFieldMap: FormDataRecalculated,
    fieldValues: List[FormComponent],
    envelope: Envelope,
    gformErrors: Map[FormComponentId, Set[String]])(fieldValue: FormComponent): Option[FormFieldValidationResult] = {
    val formFields: Map[FormComponentId, FormField] =
      toFormField(formFieldMap, fieldValues).map(hf => hf.id -> hf).toMap

    val dataGetter: FormComponentId => Option[FormField] = fId => formFields.get(fId)

    def componentField(list: List[FormComponentId]) = {
      val data = evaluateComponent(fieldValue, gformErrors)(dataGetter)
      Some(ComponentField(fieldValue, data))
    }

    fieldValue.`type` match {
      case Address(_)    => componentField(Address.fields(fieldValue.id).toList)
      case Date(_, _, _) => componentField(Date.fields(fieldValue.id).toList)
      case UkSortCode(_) => componentField(UkSortCode.fields(fieldValue.id).toList)
      case Text(_, _, _, _) | TextArea(_, _, _) | Group(_, _, _, _, _, _) =>
        formFields.get(fieldValue.id).map { formField =>
          gformErrors
            .get(fieldValue.id)
            .fold[FormFieldValidationResult](FieldOk(fieldValue, formField.value))(errors =>
              FieldError(fieldValue, formField.value, errors))
        }
      case Choice(_, _, _, _, _) => evalChoice(fieldValue, gformErrors)(dataGetter)
      case RevealingChoice(_)    => evalChoice(fieldValue, gformErrors)(dataGetter)
      case FileUpload() =>
        formFields.get(fieldValue.id).map { formField =>
          val fileName = envelope.files.find(_.fileId.value == formField.id.value).map(_.fileName).getOrElse("")
          gformErrors
            .get(fieldValue.id)
            .fold[FormFieldValidationResult](FieldOk(fieldValue, fileName))(errors =>
              FieldError(fieldValue, fileName, errors))
        }
      case InformationMessage(_, _) => None
      case HmrcTaxPeriod(_, _, _)   => evalChoice(fieldValue, gformErrors)(dataGetter)
    }
  }

  def evalChoice(fieldValue: FormComponent, gformErrors: Map[FormComponentId, Set[String]])(
    dGetter: FormComponentId => Option[FormField]) =
    dGetter(fieldValue.id).fold(Option.empty[FormFieldValidationResult]) { formField =>
      val data = formField.value
        .split(",")
        .toList
        .map { selectedIndex =>
          val validationResult = gformErrors
            .get(fieldValue.id)
            .fold[FormFieldValidationResult](
              FieldOk(fieldValue, selectedIndex)
            )(FieldError(fieldValue, selectedIndex, _))

          fieldValue.id.value + selectedIndex -> validationResult
        }
        .toMap
      Some(ComponentField(fieldValue, data))
    }

  def toFormField(fieldData: FormDataRecalculated, templateFields: List[FormComponent]): List[FormField] = {
    val getFieldData: FormComponentId => FormField = fieldId => {
      val value = fieldData.data.get(fieldId).toList.flatten.headOption.getOrElse("")
      FormField(fieldId, value)
    }

    def getFormFields(templateFields: List[FormComponent]): List[FormField] = templateFields.flatMap { fv =>
      fv.`type` match {
        case Group(_, _, _, _, _, _) =>
          require(true, "There shouldn't be Group fields here")
          Nil // For completion, there shouldn't be Groups here
        case Address(_)    => Address.fields(fv.id).toList.map(getFieldData)
        case Date(_, _, _) => Date.fields(fv.id).toList.map(getFieldData)
        case UkSortCode(_) => UkSortCode.fields(fv.id).toList.map(getFieldData)
        /* case RevealingChoice(options) =>
         *   List(getFieldData(fv.id)) ++ getFormFields(options.toList.flatMap(_.revealingFields)) */ // This causes double hidden field rendering for fields from RevealingChoice
        case Text(_, _, _, _) | TextArea(_, _, _) | Choice(_, _, _, _, _) | HmrcTaxPeriod(_, _, _) |
            RevealingChoice(_) =>
          List(getFieldData(fv.id))
        case FileUpload()             => List(getFieldData(fv.id))
        case InformationMessage(_, _) => List(getFieldData(fv.id))
      }
    }

    getFormFields(templateFields)
  }

  def getHiddenTemplateFields(
    section: Section,
    dynamicSections: List[Section],
    data: FormDataRecalculated,
    lookupExtractors: LookupExtractors): (List[FormComponent], FormDataRecalculated) = {
    val renderList: List[Section] = dynamicSections.filterNot(_ == section)
    val sectionAtomicFields: List[FormComponent] = renderList.flatMap(_.expandSection(data.data).allFCs)

    val submitted = submittedFCs(data, sectionAtomicFields)
    val alwaysEmptyHiddenGroup = getAlwaysEmptyHiddenGroup(data, section, lookupExtractors)
    val alwaysEmptyHidden = getAlwaysEmptyHidden(section, lookupExtractors)
    val hiddenFUs = hiddenFileUploads(section)

    val idsToRenderAsEmptyHidden = (alwaysEmptyHiddenGroup ++ alwaysEmptyHidden).map(_.id)

    val dataUpd = idsToRenderAsEmptyHidden.foldRight(data.data) {
      case (id, acc) => acc.updated(id, "" :: Nil)
    }

    (
      submitted ++ alwaysEmptyHiddenGroup ++ alwaysEmptyHidden ++ hiddenFUs,
      data.copy(recData = data.recData.copy(data = dataUpd)))

  }

  def flattenGroups(fields: List[FormComponent]): List[FormComponent] =
    fields.flatMap { fieldValue =>
      fieldValue.`type` match {
        case grp: Group => flattenGroups(grp.fields)
        case _          => List(fieldValue)
      }
    }
}
