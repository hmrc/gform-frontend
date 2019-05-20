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
import org.scalatest.Assertion
import org.scalactic.source.Position
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.{ AvailableLanguages, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform._

class StructuredFormDataBuilderSpec extends Spec {
  implicit val l = LangADT.En
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
      objectStructure(field("field", textNode("fieldValue")))
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
      objectStructure(field("field", arrayNode(textNode("value1"), textNode("value2"))))
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
          objectStructure(field("day", textNode("1")), field("month", textNode("2")), field("year", textNode("3")))))
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
          )))
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
          )))
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
      )
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
          )))
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
      )
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
      )
    )
  }

  it must "create the correct JSON for RevealingChoice components" in {
    validate(
      createFormTemplate(
        createNonRepeatingSection(
          createRevealingChoice("field", createNonGroupField("revealedField1"), createNonGroupField("revealedField2"))
        )
      ),
      createForm(
        "field"          -> "0",
        "revealedField1" -> "revealedField1Value",
        "revealedField2" -> "revealedField2Value"
      ),
      objectStructure(
        Field(
          FieldName("field"),
          objectStructure(
            Field(FieldName("choice"), textNode("Foo")),
            Field(
              FieldName("revealed"),
              objectStructure(
                Field(FieldName("revealedField1"), textNode("revealedField1Value")),
                Field(FieldName("revealedField2"), textNode("revealedField2Value"))
              )
            )
          )
        )
      )
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
      )
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
        ))
    )
  }

  private def validate(formTemplate: FormTemplate, formData: Form, expected: StructuredFormValue)(
    implicit position: Position,
    l: LangADT): Assertion =
    StructuredFormDataBuilder(formData, formTemplate) shouldBe expected

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
      null,
      null,
      null,
      null,
      null,
      List(section),
      acknowledgementSection.getOrElse(AcknowledgementSection(toLocalisedString("Ack"), None, None, Nil)),
      declarationSection.getOrElse(DeclarationSection(toLocalisedString("Decl"), None, None, Nil)),
      null,
      AvailableLanguages.default
    )

  def createNonRepeatingSection(fields: FormComponent*): Section =
    Section(
      null,
      null,
      null,
      null,
      null,
      None,
      None,
      null,
      fields.toList,
      null,
      null
    )

  def createRepeatingSection(fields: FormComponent*): Section =
    Section(
      null,
      null,
      null,
      null,
      null,
      Some(TextExpression(Value)),
      Some(TextExpression(Value)),
      null,
      fields.toList,
      null,
      null
    )

  def createNonGroupField(id: String): FormComponent =
    createFormComponent(id, Text(AnyText, Value))

  def createGroup(fields: FormComponent*): FormComponent =
    createFormComponent("a group", Group(fields.toList, null))

  def createFormComponent(id: String, componentType: ComponentType): FormComponent =
    FormComponent(
      FormComponentId(id),
      componentType,
      toLocalisedString(""),
      None,
      None,
      null,
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
        NonEmptyList.of(toLocalisedString("One"), toLocalisedString("Two"), toLocalisedString("Three")),
        Vertical,
        Nil,
        None))

  def createRadio(id: String): FormComponent =
    createFormComponent(
      id,
      Choice(
        Radio,
        NonEmptyList.of(toLocalisedString("One"), toLocalisedString("Two"), toLocalisedString("Three")),
        Vertical,
        Nil,
        None))

  def createDate(id: String): FormComponent =
    createFormComponent(id, Date(AnyDate, Offset(0), None))

  def createRevealingChoice(id: String, selectedFields: FormComponent*): FormComponent =
    createFormComponent(
      id,
      RevealingChoice(NonEmptyList.of(RevealingChoiceElement(toLocalisedString("Foo"), selectedFields.toList, true))))

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
