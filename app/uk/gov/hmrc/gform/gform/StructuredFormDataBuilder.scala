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

import cats.{ Monad, MonadError }
import cats.data.NonEmptyList
import cats.instances.option._
import cats.instances.list._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.functor._
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.sharedmodel.form.Form
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform.StructuredFormValue.{ ArrayNode, ObjectStructure, TextNode }
import uk.gov.hmrc.gform.sharedmodel.structuredform.{ Field, FieldName, StructuredFormValue }

object StructuredFormDataBuilder {
  def apply[F[_]](form: Form, template: FormTemplate, lookupRegistry: LookupRegistry)(
    implicit l: LangADT,
    me: MonadError[F, Throwable]): F[ObjectStructure] =
    new StructuredFormDataBuilder[F](form, template, lookupRegistry)
      .build()
      .map(StructuredFormValue.ObjectStructure.apply)

}

class StructuredFormDataBuilder[F[_]](form: Form, template: FormTemplate, lookupRegistry: LookupRegistry)(
  implicit me: MonadError[F, Throwable]) {
  private val formValuesByUnindexedId: Map[FormComponentId, NonEmptyList[String]] =
    form.formData.fields
      .groupBy(f => f.id.reduceToTemplateFieldId)
      .mapValues(values => NonEmptyList.fromListUnsafe(values.map(_.value).toList))

  private val multiChoiceFieldIds: Set[FormComponentId] = extractMultiChoiceFieldIds(template)

  def build()(implicit l: LangADT): F[List[Field]] =
    (buildSections, buildBaseSection(template.acknowledgementSection), buildBaseSection(template.declarationSection))
      .mapN(_ ++ _ ++ _)

  private def buildSections()(implicit l: LangADT): F[List[Field]] = {
    val fields: List[F[List[Field]]] =
      for {
        section <- template.sections
        field   <- section.fields
      } yield buildField(field, section.isRepeating)

    fields.flatTraverse(identity)

  }

  def buildBaseSection(section: BaseSection)(implicit l: LangADT): F[List[Field]] =
    section.fields
      .flatTraverse { unstructuredField =>
        buildField(unstructuredField, repeatable = false)
      }

  private def buildField(field: FormComponent, repeatable: Boolean)(implicit l: LangADT): F[List[Field]] =
    field.`type` match {
      case g: Group => buildGroupFields(g)
      case _        => buildNonGroupField(field, repeatable)
    }

  private def buildGroupFields(group: Group)(implicit l: LangADT): F[List[Field]] =
    group.fields.flatTraverse { buildNonGroupField(_, repeatable = true) }

  private def buildNonGroupField(nonGroupField: FormComponent, repeatable: Boolean)(
    implicit l: LangADT): F[List[Field]] = {
    val maybeField: F[Option[Field]] = nonGroupField.`type` match {
      case mf: MultiField     => buildMultiField(nonGroupField, mf, repeatable).pure[F]
      case r: RevealingChoice => buildRevealingChoiceFields(nonGroupField.id, r)
      case _ =>
        formValuesByUnindexedId
          .get(nonGroupField.id)
          .traverse {
            buildSimpleField(nonGroupField, _, repeatable, multiChoiceFieldIds.contains(nonGroupField.id))
          }

    }
    maybeField.map(_.toList)
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
    multiValue: Boolean)(implicit l: LangADT): F[Field] =
    if (repeatable) buildRepeatingSimpleField(field, values, multiValue)
    else buildNonRepeatingSimpleField(field, values.head, multiValue)

  private def lookupIdFromLabel(label: LookupLabel, register: Register)(implicit l: LangADT): F[String] =
    lookupRegistry
      .get(register)
      .flatMap {
        case RadioLookup(options)      => options.lookupInfo(label)
        case AjaxLookup(options, _, _) => options.lookupInfo(label)
      } match {
      case Some(LookupInfo(LookupId(id), _)) => id.pure[F]
      case None =>
        me.raiseError(StructuredFormDataBuilderException(s"Cannot find '${label.label}' in register $register"))
    }

  private def valueForFieldType(field: FormComponent, value: String)(implicit l: LangADT): F[String] =
    field.`type` match {
      case Text(Lookup(register), _, _, _) => lookupIdFromLabel(LookupLabel(value), register)
      case _                               => value.pure[F]
    }

  private def buildNonRepeatingSimpleField(field: FormComponent, value: String, multiValue: Boolean)(
    implicit l: LangADT): F[Field] =
    for {
      fieldValue <- valueForFieldType(field, value)
    } yield Field(FieldName(field.id.value), buildNode(fieldValue, multiValue), Map.empty)

  private def buildRepeatingSimpleField(field: FormComponent, value: NonEmptyList[String], multiValue: Boolean)(
    implicit l: LangADT): F[Field] = {
    val elementsF: F[List[StructuredFormValue]] = value.toList.traverse { v =>
      valueForFieldType(field, v).map(buildNode(_, multiValue))
    }
    elementsF.map(elements => Field(FieldName(field.id.value), ArrayNode(elements), Map.empty))
  }

  private def buildRevealingChoiceFields(id: FormComponentId, revealingChoice: RevealingChoice)(
    implicit l: LangADT): F[Option[Field]] =
    formValuesByUnindexedId.get(id).map(_.head).flatTraverse { selectionStr =>
      val selection = selectionStr.toInt
      revealingChoice.options.get(selection).traverse { rcElement =>
        revealedChoiceFields(rcElement).map { os =>
          Field(
            FieldName(id.value),
            ObjectStructure(
              List(
                Field(FieldName("choice"), TextNode(selection.toString)),
                Field(FieldName("revealed"), ObjectStructure(os))
              )
            )
          )
        }
      }
    }

  private def revealedChoiceFields(rcElement: RevealingChoiceElement)(implicit l: LangADT): F[List[Field]] =
    rcElement.revealingFields.flatTraverse(buildNonGroupField(_, false))

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

case class StructuredFormDataBuilderException(message: String) extends Exception
