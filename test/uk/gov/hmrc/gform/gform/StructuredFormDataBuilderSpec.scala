/*
 * Copyright 2020 HM Revenue & Customs
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
import cats.instances.either._
import cats.syntax.either._
import org.scalatest.Assertion
import org.scalactic.source.Position
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LangADT }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform._

class StructuredFormDataBuilderSpec extends Spec {
  private implicit val l: LangADT = LangADT.En

  type EitherEffect[A] = Either[Throwable, A]

  "apply(Form, FormTemplate)" must "create the correct JSON for simple fields in non-repeating sections/groups" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createNonGroupField("field")
        )
      ),
      createForm(
        "field" -> "fieldValue"
      ),
      objectStructure(field("field", textNode("fieldValue"))).asRight
    )
  }

  it must "create the correct JSON for multivalue fields in non-repeating sections/groups" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createMultiChoice("field")
        )
      ),
      createForm(
        "field" -> "value1,value2"
      ),
      objectStructure(field("field", arrayNode(textNode("value1"), textNode("value2")))).asRight
    )
  }

  it must "create the correct JSON for MultiField fields in non-repeating sections/groups" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createDate("field")
        )
      ),
      createForm(
        "field"       -> "",
        "field-day"   -> "1",
        "field-month" -> "2",
        "field-year"  -> "3"
      ),
      objectStructure(
        field(
          "field",
          objectStructure(field("day", textNode("1")), field("month", textNode("2")), field("year", textNode("3"))))).asRight
    )
  }

  it must "create the correct JSON for simple fields in groups in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createGroup(createNonGroupField("field"))
        )
      ),
      createForm(
        "1_field" -> "fieldValue1",
        "2_field" -> "fieldValue2"
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            textNode("fieldValue1"),
            textNode("fieldValue2")
          ))).asRight
    )
  }

  it must "create the correct JSON for multivalue fields in groups in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createGroup(createMultiChoice("field"))
        )
      ),
      createForm(
        "1_field" -> "value1,value2",
        "2_field" -> "value1,value3, value4"
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            arrayNode(textNode("value1"), textNode("value2")),
            arrayNode(textNode("value1"), textNode("value3"), textNode("value4"))
          ))).asRight
    )
  }

  it must "create the correct JSON for MultiField fields in groups in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createGroup(createDate("field"))
        )
      ),
      createForm(
        "field"         -> "",
        "1_field-day"   -> "1",
        "1_field-month" -> "2",
        "1_field-year"  -> "3",
        "2_field-day"   -> "4",
        "2_field-month" -> "5",
        "3_field-year"  -> "6"
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            objectStructure(field("day", textNode("1")), field("month", textNode("2")), field("year", textNode("3"))),
            objectStructure(field("day", textNode("4")), field("month", textNode("5")), field("year", textNode("6")))
          )
        )
      ).asRight
    )
  }

  it must "create the correct JSON for simple fields in repeating sections" in {
    validate(
      createFormTemplate(
        createRepeatingSection(
          createNonGroupField("field")
        )
      ),
      createForm(
        "field"   -> "fieldValue1",
        "2_field" -> "fieldValue2"
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            textNode("fieldValue1"),
            textNode("fieldValue2")
          ))).asRight
    )
  }

  it must "create the correct JSON for multivalue fields in repeating sections" in {
    validate(
      createFormTemplate(
        createRepeatingSection(
          createMultiChoice("field")
        )
      ),
      createForm(
        "field"   -> "value1,value2",
        "2_field" -> "value1,value3, value4"
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            arrayNode(textNode("value1"), textNode("value2")),
            arrayNode(textNode("value1"), textNode("value3"), textNode("value4"))
          )
        )
      ).asRight
    )
  }

  it must "create the correct JSON for MultiField fields in repeating sections" in {
    validate(
      createFormTemplate(
        createRepeatingSection(
          createDate("field")
        )
      ),
      createForm(
        "field"         -> "",
        "field-day"     -> "1",
        "field-month"   -> "2",
        "field-year"    -> "3",
        "2_field-day"   -> "4",
        "2_field-month" -> "5",
        "3_field-year"  -> "6"
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            objectStructure(field("day", textNode("1")), field("month", textNode("2")), field("year", textNode("3"))),
            objectStructure(field("day", textNode("4")), field("month", textNode("5")), field("year", textNode("6")))
          )
        )
      ).asRight
    )
  }

  it must "create the correct JSON for single value RevealingChoice components in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createRevealingChoice(
            "revealYourSecrets",
            false,
            createRevealingChoiceElement(createNonGroupField("revealedField1"), createDate("revealedDate")),
            createRevealingChoiceElement(createNonGroupField("unrevealedField1"))
          )
        )
      ),
      createForm(
        "revealYourSecrets"  -> "0",
        "revealedField1"     -> "revealedField1Value",
        "revealedDate"       -> "",
        "revealedDate-day"   -> "1",
        "revealedDate-month" -> "2",
        "revealedDate-year"  -> "3",
        "unrevealedField1"   -> "unrevealedField1Value"
      ),
      objectStructure(
        Field(
          FieldName("revealYourSecrets"),
          objectStructure(
            Field(FieldName("choice"), textNode("0")),
            Field(
              FieldName("revealed"),
              objectStructure(
                Field(FieldName("revealedField1"), textNode("revealedField1Value")),
                field(
                  "revealedDate",
                  objectStructure(
                    field("day", textNode("1")),
                    field("month", textNode("2")),
                    field("year", textNode("3"))))
              )
            )
          )
        )
      ).asRight
    )
  }

  it must "create the correct JSON for multi value RevealingChoice components in non-repeating sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createRevealingChoice(
            "revealYourSecrets",
            true,
            createRevealingChoiceElement(createNonGroupField("revealedField1"), createDate("revealedDate")),
            createRevealingChoiceElement(createNonGroupField("unrevealedField1")),
            createRevealingChoiceElement(createNonGroupField("revealedField2"))
          )
        )
      ),
      createForm(
        "revealYourSecrets"  -> "0,2",
        "revealedField1"     -> "revealedField1Value",
        "revealedDate"       -> "",
        "revealedDate-day"   -> "1",
        "revealedDate-month" -> "2",
        "revealedDate-year"  -> "3",
        "unrevealedField1"   -> "unrevealedField1Value",
        "revealedField2"     -> "revealedField2Value"
      ),
      objectStructure(
        Field(
          FieldName("revealYourSecrets"),
          objectStructure(
            Field(FieldName("choices"), arrayNode(textNode("0"), textNode("2"))),
            Field(
              FieldName("revealed"),
              objectStructure(
                Field(FieldName("revealedField1"), textNode("revealedField1Value")),
                field(
                  "revealedDate",
                  objectStructure(
                    field("day", textNode("1")),
                    field("month", textNode("2")),
                    field("year", textNode("3")))),
                Field(FieldName("revealedField2"), textNode("revealedField2Value"))
              )
            )
          )
        )
      ).asRight
    )
  }

  it must "include the acknowledgment and declaration sections" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createNonGroupField("field")
        ),
        Some(AcknowledgementSection(null, None, None, List(createNonGroupField("ackField")))),
        Some(DeclarationSection(null, None, None, List(createNonGroupField("decField"))))
      ),
      createForm(
        "field"    -> "fieldValue",
        "ackField" -> "ackFieldValue",
        "decField" -> "decFieldValue"
      ),
      objectStructure(
        field("field", textNode("fieldValue")),
        field("ackField", textNode("ackFieldValue")),
        field("decField", textNode("decFieldValue"))
      ).asRight
    )
  }

  it must "provide the correct alternative field names for robotic XML" in {
    def roboticsXmlPurposeMap(fieldName: String) = createStructuredPurposeMap(RoboticsXml, FieldName(fieldName))

    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createAddress("field")
        )
      ),
      createForm(
        "field-street1"  -> "1",
        "field-street2"  -> "2",
        "field-street3"  -> "3",
        "field-street4"  -> "4",
        "field-uk"       -> "5",
        "field-postcode" -> "6",
        "field-country"  -> "7"
      ),
      objectStructure(
        field(
          "field",
          objectStructure(
            field("street1", textNode("1"), roboticsXmlPurposeMap("line1")),
            field("street2", textNode("2"), roboticsXmlPurposeMap("line2")),
            field("street3", textNode("3"), roboticsXmlPurposeMap("line3")),
            field("street4", textNode("4"), roboticsXmlPurposeMap("line4")),
            field("uk", textNode("5"), roboticsXmlPurposeMap("uk")),
            field("postcode", textNode("6"), roboticsXmlPurposeMap("postcode")),
            field("country", textNode("7"), roboticsXmlPurposeMap("country"))
          )
        )
      ).asRight
    )
  }

  it must "translate label frm lookup component to its id" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createLookupField("field")
        )
      ),
      createForm(
        "field" -> "fieldValue"
      ),
      objectStructure(
        field("field", textNode("field_id"))
      ).asRight
    )
  }

  it must "fail when label doesn't exist in lookup component" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createLookupField("field")
        )
      ),
      createForm(
        "field" -> "non existent label"
      ),
      StructuredFormDataBuilderException("Cannot find 'non existent label' in register Origin").asLeft
    )
  }

  private val lookupRegistry = new LookupRegistry(
    Map(
      Register.Origin -> RadioLookup(LocalisedLookupOptions(
        Map(LangADT.En -> LookupOptions(Map(LookupLabel("fieldValue") -> LookupInfo(LookupId("field_id"), 1))))))))

  private def validate[A](formTemplate: FormTemplate, formData: Form, expected: A)(
    implicit position: Position,
    l: LangADT): Assertion =
    StructuredFormDataBuilder[EitherEffect](formData, formTemplate, lookupRegistry) shouldBe expected

  def createForm(fields: (String, String)*): Form =
    Form(
      FormId("TheForm"),
      null,
      null,
      FormTemplateId(""),
      FormData(fields.map { case (k, v) => FormField(FormComponentId(k), v) }),
      null,
      VisitIndex(Set(1)),
      ThirdPartyData.empty,
      None
    )

  def createFormTemplate(
    section: Section,
    acknowledgementSection: Option[AcknowledgementSection] = None,
    declarationSection: Option[DeclarationSection] = None): FormTemplate =
    FormTemplate(
      FormTemplateId(""),
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      null,
      List(section),
      acknowledgementSection.getOrElse(AcknowledgementSection(toSmartString("Ack"), None, None, Nil)),
      declarationSection.getOrElse(DeclarationSection(toSmartString("Decl"), None, None, Nil)),
      Nil,
      null,
      AvailableLanguages.default,
      null
    )

  def createNonRepeatingSection(fields: FormComponent*): Section =
    Section.NonRepeatingPage(
      Page(
        null,
        null,
        null,
        null,
        null,
        null,
        fields.toList,
        null,
        null
      ))

  def createRepeatingSection(fields: FormComponent*): Section =
    Section.RepeatingPage(
      Page(null, null, null, null, null, null, fields.toList, null, null),
      TextExpression(Value)
    )

  def createNonGroupField(id: String): FormComponent =
    createFormComponent(id, Text(BasicText, Value))

  def createLookupField(id: String): FormComponent =
    createFormComponent(id, Text(Lookup(Register.Origin), Value))

  def createGroup(fields: FormComponent*): FormComponent =
    createFormComponent("a group", Group(fields.toList, null))

  def createFormComponent(id: String, componentType: ComponentType): FormComponent =
    FormComponent(
      FormComponentId(id),
      componentType,
      toSmartString(""),
      None,
      None,
      None,
      true,
      true,
      true,
      true,
      true,
      None
    )

  def createMultiChoice(id: String): FormComponent =
    createFormComponent(
      id,
      Choice(
        Checkbox,
        NonEmptyList.of(toSmartString("One"), toSmartString("Two"), toSmartString("Three")),
        Vertical,
        Nil,
        None))

  def createRadio(id: String): FormComponent =
    createFormComponent(
      id,
      Choice(
        Radio,
        NonEmptyList.of(toSmartString("One"), toSmartString("Two"), toSmartString("Three")),
        Vertical,
        Nil,
        None))

  def createDate(id: String): FormComponent =
    createFormComponent(id, Date(AnyDate, Offset(0), None))

  def createRevealingChoice(
    id: String,
    multiValue: Boolean,
    element1: RevealingChoiceElement,
    elements: RevealingChoiceElement*): FormComponent =
    createFormComponent(id, RevealingChoice(NonEmptyList.of(element1, elements: _*), multiValue))

  def createRevealingChoiceElement(fields: FormComponent*): RevealingChoiceElement =
    RevealingChoiceElement(toSmartString("foo"), fields.toList, false)

  def createAddress(id: String): FormComponent = createFormComponent(id, Address(false))

  def createStructuredPurposeMap(
    purpose: StructuredFormDataFieldNamePurpose,
    fieldNames: FieldName*): Map[StructuredFormDataFieldNamePurpose, FieldName] = fieldNames.map(purpose -> _).toMap

  private def objectStructure(fields: Field*): StructuredFormValue =
    StructuredFormValue.ObjectStructure(fields.map { case Field(n, v, a) => Field(n, v, a) } toList)

  private def textNode(value: String): StructuredFormValue =
    StructuredFormValue.TextNode(value)

  private def arrayNode(values: StructuredFormValue*): StructuredFormValue =
    StructuredFormValue.ArrayNode(values.toList)

  private def field(
    name: String,
    value: StructuredFormValue,
    alternateFieldNames: Map[StructuredFormDataFieldNamePurpose, FieldName] = Map.empty) =
    Field(FieldName(name), value, alternateFieldNames)
}
