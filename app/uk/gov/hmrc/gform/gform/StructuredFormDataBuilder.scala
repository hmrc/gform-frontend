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

package uk.gov.hmrc.gform.gform

import cats.data.NonEmptyList
import cats.instances.option._
import cats.syntax.foldable._
import cats.syntax.option._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

object StructuredFormDataBuilder {
  def apply(form: Form, template: FormTemplate)(implicit l: LangADT): ObjectStructure =
    StructuredFormValue.ObjectStructure(new StructuredFormDataBuilder(form, template).build())
}

class StructuredFormDataBuilder(form: Form, template: FormTemplate)(implicit l: LangADT) {
  private val formValuesByUnindexedId: Map[FormComponentId, NonEmptyList[String]] =
    form.formData.fields
      .groupBy(f => f.id.reduceToTemplateFieldId)
      .mapValues(values => NonEmptyList.fromListUnsafe(values.map(_.value).toList))

  private val multiChoiceFieldIds: Set[FormComponentId] = extractMultiChoiceFieldIds(template)

  def build(): List[Field] =
    buildSections ++ buildBaseSection(template.acknowledgementSection) ++ buildBaseSection(template.declarationSection)

  private def buildSections(): List[Field] =
    for {
      section     <- template.sections
      field       <- section.fields
      fieldAsJson <- buildField(field, section.isRepeating)
    } yield fieldAsJson

  def buildBaseSection(section: BaseSection): List[Field] =
    for {
      unstructuredField <- section.fields
      structuredField   <- buildField(unstructuredField, repeatable = false)
    } yield structuredField

  private def buildField(field: FormComponent, repeatable: Boolean): Seq[Field] =
    field.`type` match {
      case g: Group => buildGroupFields(g)
      case _        => buildNonGroupField(field, repeatable).toSeq
    }

  private def buildGroupFields(group: Group): Seq[Field] =
    group.fields.flatMap { buildNonGroupField(_, repeatable = true) }

  private def buildNonGroupField(nonGroupField: FormComponent, repeatable: Boolean): Option[Field] =
    nonGroupField.`type` match {
      case mf: MultiField     => buildMultiField(nonGroupField, mf, repeatable)
      case r: RevealingChoice => buildRevealingChoiceFields(nonGroupField.id, r)
      case _ =>
        formValuesByUnindexedId.get(nonGroupField.id).map {
          buildSimpleField(nonGroupField, _, repeatable, multiChoiceFieldIds.contains(nonGroupField.id))
        }
    }

  private def buildMultiField(baseField: FormComponent, mf: MultiField, repeatable: Boolean): Option[Field] =
    mf.fields(baseField.id)
      .traverse { f =>
        formValuesByUnindexedId.get(f).map(v => (f, v))
      }
      .map(sequence)
      .map { v =>
        if (repeatable) buildRepeatableMultiField(baseField.id, v, mf)
        else buildNonRepeatableMultiField(baseField.id, v.head, mf)
      }

  private def buildRepeatableMultiField(
    baseFieldId: FormComponentId,
    values: NonEmptyList[NonEmptyList[(FormComponentId, String)]],
    mf: MultiField): Field =
    Field(
      FieldName(baseFieldId.value),
      ArrayNode(values.toList.map { buildObjectStructureForMultiField(baseFieldId, _, mf) }),
      Map.empty
    )

  private def buildNonRepeatableMultiField(
    baseFieldId: FormComponentId,
    fields: NonEmptyList[(FormComponentId, String)],
    mf: MultiField): Field =
    Field(FieldName(baseFieldId.value), buildObjectStructureForMultiField(baseFieldId, fields, mf), Map.empty)

  private def buildObjectStructureForMultiField(
    baseFieldId: FormComponentId,
    fields: NonEmptyList[(FormComponentId, String)],
    mf: MultiField) =
    ObjectStructure(fields.map {
      case (k, v) =>
        val strippedId = k.stripBase(baseFieldId).value
        Field(FieldName(strippedId), TextNode(v), mf.alternateNamesFor(FormComponentId(strippedId)))
    }.toList)

  private def buildSimpleField(
    field: FormComponent,
    values: NonEmptyList[String],
    repeatable: Boolean,
    multiValue: Boolean): Field =
    if (repeatable) buildRepeatingSimpleField(field, values, multiValue)
    else buildNonRepeatingSimpleField(field, values.head, multiValue)

  private def buildNonRepeatingSimpleField(field: FormComponent, value: String, multiValue: Boolean): Field =
    Field(FieldName(field.id.value), buildNode(value, multiValue), Map.empty)

  private def buildRepeatingSimpleField(field: FormComponent, value: NonEmptyList[String], multiValue: Boolean): Field =
    Field(FieldName(field.id.value), ArrayNode(value.toList.map(buildNode(_, multiValue))), Map.empty)

  private def buildRevealingChoiceFields(id: FormComponentId, revealingChoice: RevealingChoice): Option[Field] =
    formValuesByUnindexedId.get(id).map(_.head).map { selectionStr =>
      val selection = selectionStr.toInt
      val maybeRevealingChoiceElement = revealingChoice.options.get(selection)
      Field(
        FieldName(id.value),
        ObjectStructure(
          maybeRevealingChoiceElement.fold(List.empty[Field]) { rcElement =>
            List(
              Field(FieldName("choice"), TextNode(rcElement.choice.value)),
              Field(FieldName("revealed"), ObjectStructure(revealedChoiceFields(rcElement)))
            )
          }
        )
      )
    }

  private def revealedChoiceFields(rcElement: RevealingChoiceElement): List[Field] =
    rcElement.revealingFields
      .flatMap { component =>
        buildNonGroupField(component, false)
      }

  private def buildNode(value: String, multiValue: Boolean): StructuredFormValue =
    if (multiValue) choicesToArray(value)
    else TextNode(value)

  private def extractMultiChoiceFieldIds(template: FormTemplate): Set[FormComponentId] = {
    def groupOrNot(field: FormComponent): List[FormComponent] = field.`type` match {
      case g: Group => g.fields
      case _        => List(field)
    }

    def multiChoiceFieldId(field: FormComponent): Option[FormComponentId] = field.`type` match {
      case c: Choice if c.`type` == Checkbox => field.id.some
      case _                                 => None
    }

    (for {
      section       <- template.sections
      field         <- section.fields
      nonGroupField <- groupOrNot(field)
      id            <- multiChoiceFieldId(nonGroupField)
    } yield id).toSet
  }

  private def choicesToArray(commaSeparatedString: String): ArrayNode =
    ArrayNode(commaSeparatedString.split(',').map(_.trim).filterNot(_.isEmpty).map(TextNode).toList)

  def sequence(
    map: NonEmptyList[(FormComponentId, NonEmptyList[String])]): NonEmptyList[NonEmptyList[(FormComponentId, String)]] =
    NonEmptyList.fromListUnsafe {
      (0 until map.head._2.size).toList.map { i =>
        map.map { case (k, v) => k -> v.toList(i) }
      }
    }
}
