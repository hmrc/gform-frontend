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

import cats.Id
import cats.data.NonEmptyList
import cats.instances.either._
import cats.syntax.either._
import org.scalatest.{ Assertion, FlatSpec, Matchers }
import org.scalactic.source.Position
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.graph.FormTemplateBuilder._
import uk.gov.hmrc.gform.lookup._
import uk.gov.hmrc.gform.models.{ FormModelBuilder, FormModelSupport, SectionSelectorType, VariadicFormDataSupport }
import uk.gov.hmrc.gform.models.optics.{ DataOrigin, FormModelVisibilityOptics }
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, VariadicFormData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.{ Destination, DestinationId, Destinations }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.destinations.Destinations.DestinationList
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, SourceOrigin }
import uk.gov.hmrc.gform.sharedmodel.form._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.structuredform._
import uk.gov.hmrc.gform.typeclasses.identityThrowableMonadError

class StructuredFormDataBuilderSpec extends FlatSpec with Matchers with FormModelSupport with VariadicFormDataSupport {
  private implicit val l: LangADT = LangADT.En

  type EitherEffect[A] = Either[Throwable, A]

  /* |                                      | text | hmrc tax period | file | sort code | address | date | choice | lookup |
   * |--------------------------------------+------+-----------------+------+-----------+---------+------+--------+--------|
   * | standard                             | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * | group                                | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * | repeated section                     | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * | add to list                          | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * | revealing choice in group            | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * | revealing choice in repeated section | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * | revealing choice in add to list      | x    | todo            | todo | todo      | todo    | todo | todo   | x      |
   * |--------------------------------------+------+-----------------+------+-----------+---------+------+--------+--------|
   */

  "lookup" must "create the correct JSON in all possible contexts" in {
    validate(
      createFormModelVisibilityOptics(
        List(
          createNonRepeatingSection(
            createLookupField("standardLookup")
          ),
          createNonRepeatingSection(
            createRevealingChoice(
              "standardRevealingChoice",
              true,
              createRevealingChoiceElement(createLookupField("lookupInRevealingChoice"))
            )
          ),
          createNonRepeatingSection(
            createGroup(createLookupField("lookupInGroup"))
          ),
          createRepeatingSection(3)(
            createLookupField("lookupInRepeatedSection")
          ),
          createAddToListSection(
            "addAnotherQuestionWithText",
            List(
              createLookupField("lookupInAddToList")
            )
          ),
          createRepeatingSection(3)(
            createRevealingChoice(
              "revealingChoiceInRepeatedSection",
              true,
              createRevealingChoiceElement(createLookupField("lookupInRevealingChoiceInRepeatedSection"))
            )
          ),
          createAddToListSection(
            "addAnotherQuestionWithRevealingChoiceWithText",
            List(
              createRevealingChoice(
                "revealingChoiceInAddToList",
                true,
                createRevealingChoiceElement(createLookupField("lookupInRevealingChoiceInAddToList"))
              )
            )
          )
        ),
        variadicFormDataMany(
          "standardRevealingChoice"                         -> List(0),
          "1_revealingChoiceInAddToList"                    -> List(0),
          "2_revealingChoiceInAddToList"                    -> List(0),
          "3_revealingChoiceInAddToList"                    -> List(0),
          "1_addAnotherQuestionWithText"                    -> List(0),
          "2_addAnotherQuestionWithText"                    -> List(0),
          "3_addAnotherQuestionWithText"                    -> List(1),
          "1_addAnotherQuestionWithRevealingChoiceWithText" -> List(0),
          "2_addAnotherQuestionWithRevealingChoiceWithText" -> List(0),
          "3_addAnotherQuestionWithRevealingChoiceWithText" -> List(1),
          "1_revealingChoiceInRepeatedSection"              -> List(0),
          "2_revealingChoiceInRepeatedSection"              -> List(0),
          "3_revealingChoiceInRepeatedSection"              -> List(0)
        ) ++
          variadicFormData(
            "standardLookup"                             -> "fieldValue",
            "lookupInRevealingChoice"                    -> "fieldValue",
            "1_lookupInGroup"                            -> "fieldValue",
            "2_lookupInGroup"                            -> "fieldValue",
            "3_lookupInGroup"                            -> "fieldValue",
            "1_lookupInRepeatedSection"                  -> "fieldValue",
            "2_lookupInRepeatedSection"                  -> "fieldValue",
            "3_lookupInRepeatedSection"                  -> "fieldValue",
            "1_lookupInAddToList"                        -> "fieldValue",
            "2_lookupInAddToList"                        -> "fieldValue",
            "3_lookupInAddToList"                        -> "fieldValue",
            "1_lookupInRevealingChoiceInRepeatedSection" -> "fieldValue",
            "2_lookupInRevealingChoiceInRepeatedSection" -> "fieldValue",
            "3_lookupInRevealingChoiceInRepeatedSection" -> "fieldValue",
            "1_lookupInRevealingChoiceInAddToList"       -> "fieldValue",
            "2_lookupInRevealingChoiceInAddToList"       -> "fieldValue",
            "3_lookupInRevealingChoiceInAddToList"       -> "fieldValue",
            "1_a_group"                                  -> "",
            "2_a_group"                                  -> "",
            "3_a_group"                                  -> ""
          )
      ),
      objectStructure(
        field(
          "addAnotherQuestionWithRevealingChoiceWithText",
          arrayNode(
            objectStructure(
              field("addAnotherQuestionWithRevealingChoiceWithText", arrayNode(textNode("0"))),
              field(
                "revealingChoiceInAddToList",
                objectStructure(
                  field("choices", arrayNode(textNode("0"))),
                  field("revealed", objectStructure(field("lookupInRevealingChoiceInAddToList", textNode("field_id"))))
                )
              )
            ),
            objectStructure(
              field("addAnotherQuestionWithRevealingChoiceWithText", arrayNode(textNode("0"))),
              field(
                "revealingChoiceInAddToList",
                objectStructure(
                  field("choices", arrayNode(textNode("0"))),
                  field("revealed", objectStructure(field("lookupInRevealingChoiceInAddToList", textNode("field_id"))))
                )
              )
            ),
            objectStructure(
              field("addAnotherQuestionWithRevealingChoiceWithText", arrayNode(textNode("1"))),
              field(
                "revealingChoiceInAddToList",
                objectStructure(
                  field("choices", arrayNode(textNode("0"))),
                  field("revealed", objectStructure(field("lookupInRevealingChoiceInAddToList", textNode("field_id"))))
                )
              )
            )
          )
        ),
        field(
          "addAnotherQuestionWithText",
          arrayNode(
            objectStructure(
              field("addAnotherQuestionWithText", arrayNode(textNode("0"))),
              field("lookupInAddToList", textNode("field_id"))),
            objectStructure(
              field("addAnotherQuestionWithText", arrayNode(textNode("0"))),
              field("lookupInAddToList", textNode("field_id"))
            ),
            objectStructure(
              field("addAnotherQuestionWithText", arrayNode(textNode("1"))),
              field("lookupInAddToList", textNode("field_id"))
            )
          )
        ),
        field(
          "standardRevealingChoice",
          objectStructure(
            field("choices", arrayNode(textNode("0"))),
            field("revealed", objectStructure(field("lookupInRevealingChoice", textNode("field_id"))))
          )
        ),
        field(
          "revealingChoiceInRepeatedSection",
          arrayNode(
            objectStructure(
              field("choices", arrayNode(textNode("0"))),
              field(
                "revealed",
                objectStructure(field("lookupInRevealingChoiceInRepeatedSection", arrayNode(textNode("field_id")))))
            ),
            objectStructure(
              field("choices", arrayNode(textNode("0"))),
              field(
                "revealed",
                objectStructure(field("lookupInRevealingChoiceInRepeatedSection", arrayNode(textNode("field_id")))))
            ),
            objectStructure(
              field("choices", arrayNode(textNode("0"))),
              field(
                "revealed",
                objectStructure(field("lookupInRevealingChoiceInRepeatedSection", arrayNode(textNode("field_id")))))
            )
          )
        ),
        field("standardLookup", textNode("field_id")),
        field("lookupInGroup", arrayNode(textNode("field_id"), textNode("field_id"), textNode("field_id"))),
        field("lookupInRepeatedSection", arrayNode(textNode("field_id"), textNode("field_id"), textNode("field_id"))),
        field("a_group", arrayNode(textNode(""), textNode(""), textNode("")))
      ).asRight
    )
  }
  "text" must "create the correct JSON in all possible contexts" in {
    validate(
      createFormModelVisibilityOptics(
        List(
          createNonRepeatingSection(
            createNonGroupField("standardText")
          ),
          createNonRepeatingSection(
            createRevealingChoice(
              "standardRevealingChoice",
              true,
              createRevealingChoiceElement(createNonGroupField("textInRevealingChoice"))
            )
          ),
          createNonRepeatingSection(
            createGroup(createNonGroupField("textInGroup"))
          ),
          createRepeatingSection(3)(
            createNonGroupField("textInRepeatedSection")
          ),
          createAddToListSection(
            "addAnotherQuestionWithText",
            List(
              createNonGroupField("textInAddToList")
            )
          ),
          createRepeatingSection(3)(
            createRevealingChoice(
              "revealingChoiceInRepeatedSection",
              true,
              createRevealingChoiceElement(createNonGroupField("textInRevealingChoiceInRepeatedSection"))
            )
          ),
          createAddToListSection(
            "addAnotherQuestionWithRevealingChoiceWithText",
            List(
              createRevealingChoice(
                "revealingChoiceInAddToList",
                true,
                createRevealingChoiceElement(createNonGroupField("textInRevealingChoiceInAddToList"))
              )
            )
          )
        ),
        variadicFormDataMany(
          "standardRevealingChoice"                         -> List(0),
          "1_revealingChoiceInAddToList"                    -> List(0),
          "2_revealingChoiceInAddToList"                    -> List(0),
          "3_revealingChoiceInAddToList"                    -> List(0),
          "1_addAnotherQuestionWithText"                    -> List(0),
          "2_addAnotherQuestionWithText"                    -> List(0),
          "3_addAnotherQuestionWithText"                    -> List(1),
          "1_addAnotherQuestionWithRevealingChoiceWithText" -> List(0),
          "2_addAnotherQuestionWithRevealingChoiceWithText" -> List(0),
          "3_addAnotherQuestionWithRevealingChoiceWithText" -> List(1),
          "1_revealingChoiceInRepeatedSection"              -> List(0),
          "2_revealingChoiceInRepeatedSection"              -> List(0),
          "3_revealingChoiceInRepeatedSection"              -> List(0)
        ) ++
          variadicFormData(
            "standardText"                             -> "Standard text",
            "textInRevealingChoice"                    -> "Standard revealing choice text",
            "1_textInGroup"                            -> "Group text 1",
            "2_textInGroup"                            -> "Group text 2",
            "3_textInGroup"                            -> "Group text 3",
            "1_textInRepeatedSection"                  -> "Repeated section text 1",
            "2_textInRepeatedSection"                  -> "Repeated section text 2",
            "3_textInRepeatedSection"                  -> "Repeated section text 3",
            "1_textInAddToList"                        -> "Add to list text 1",
            "2_textInAddToList"                        -> "Add to list text 2",
            "3_textInAddToList"                        -> "Add to list text 3",
            "1_textInRevealingChoiceInRepeatedSection" -> "Repeated section with revealing choice text 1",
            "2_textInRevealingChoiceInRepeatedSection" -> "Repeated section with revealing choice text 2",
            "3_textInRevealingChoiceInRepeatedSection" -> "Repeated section with revealing choice text 3",
            "1_textInRevealingChoiceInAddToList"       -> "Add to list with revealing choice text 1",
            "2_textInRevealingChoiceInAddToList"       -> "Add to list with revealing choice text 2",
            "3_textInRevealingChoiceInAddToList"       -> "Add to list with revealing choice text 3",
            "1_a_group"                                -> "",
            "2_a_group"                                -> "",
            "3_a_group"                                -> ""
          )
      ),
      objectStructure(
        field(
          "addAnotherQuestionWithRevealingChoiceWithText",
          arrayNode(
            objectStructure(
              field("addAnotherQuestionWithRevealingChoiceWithText", arrayNode(textNode("0"))),
              field(
                "revealingChoiceInAddToList",
                objectStructure(
                  field("choices", arrayNode(textNode("0"))),
                  field(
                    "revealed",
                    objectStructure(
                      field("textInRevealingChoiceInAddToList", textNode("Add to list with revealing choice text 1"))))
                )
              )
            ),
            objectStructure(
              field("addAnotherQuestionWithRevealingChoiceWithText", arrayNode(textNode("0"))),
              field(
                "revealingChoiceInAddToList",
                objectStructure(
                  field("choices", arrayNode(textNode("0"))),
                  field(
                    "revealed",
                    objectStructure(
                      field("textInRevealingChoiceInAddToList", textNode("Add to list with revealing choice text 2"))))
                )
              )
            ),
            objectStructure(
              field("addAnotherQuestionWithRevealingChoiceWithText", arrayNode(textNode("1"))),
              field(
                "revealingChoiceInAddToList",
                objectStructure(
                  field("choices", arrayNode(textNode("0"))),
                  field(
                    "revealed",
                    objectStructure(
                      field("textInRevealingChoiceInAddToList", textNode("Add to list with revealing choice text 3"))))
                )
              )
            )
          )
        ),
        field(
          "addAnotherQuestionWithText",
          arrayNode(
            objectStructure(
              field("textInAddToList", textNode("Add to list text 1")),
              field("addAnotherQuestionWithText", arrayNode(textNode("0")))),
            objectStructure(
              field("textInAddToList", textNode("Add to list text 2")),
              field("addAnotherQuestionWithText", arrayNode(textNode("0")))),
            objectStructure(
              field("textInAddToList", textNode("Add to list text 3")),
              field("addAnotherQuestionWithText", arrayNode(textNode("1"))))
          )
        ),
        field(
          "standardRevealingChoice",
          objectStructure(
            field("choices", arrayNode(textNode("0"))),
            field(
              "revealed",
              objectStructure(field("textInRevealingChoice", textNode("Standard revealing choice text"))))
          )
        ),
        field(
          "revealingChoiceInRepeatedSection",
          arrayNode(
            objectStructure(
              field("choices", arrayNode(textNode("0"))),
              field(
                "revealed",
                objectStructure(
                  field(
                    "textInRevealingChoiceInRepeatedSection",
                    arrayNode(textNode("Repeated section with revealing choice text 1")))))
            ),
            objectStructure(
              field("choices", arrayNode(textNode("0"))),
              field(
                "revealed",
                objectStructure(
                  field(
                    "textInRevealingChoiceInRepeatedSection",
                    arrayNode(textNode("Repeated section with revealing choice text 2")))))
            ),
            objectStructure(
              field("choices", arrayNode(textNode("0"))),
              field(
                "revealed",
                objectStructure(
                  field(
                    "textInRevealingChoiceInRepeatedSection",
                    arrayNode(textNode("Repeated section with revealing choice text 3")))))
            )
          )
        ),
        field("standardText", textNode("Standard text")),
        field(
          "textInRepeatedSection",
          arrayNode(
            textNode("Repeated section text 1"),
            textNode("Repeated section text 2"),
            textNode("Repeated section text 3"))),
        field("textInGroup", arrayNode(textNode("Group text 1"), textNode("Group text 2"), textNode("Group text 3"))),
        field("a_group", arrayNode(textNode(""), textNode(""), textNode("")))
      ).asRight
    )
  }

  "apply(Form, FormTemplate)" must "create the correct JSON for simple fields in non-repeating sections/groups" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createNonGroupField("field")
        ) :: Nil,
        variadicFormData("field" -> "fieldValue")
      ),
      objectStructure(field("field", textNode("fieldValue"))).asRight
    )
  }

  it must "create the correct JSON for multivalue fields in non-repeating sections/groups" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createMultiChoice("field")
        ) :: Nil,
        variadicFormDataMany("field" -> List(1, 2))
      ),
      objectStructure(field("field", arrayNode(textNode("1"), textNode("2")))).asRight
    )
  }

  it must "create the correct JSON for MultiField fields in non-repeating sections/groups" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createDate("field")
        ) :: Nil,
        variadicFormData(
          "field"       -> "",
          "field-day"   -> "1",
          "field-month" -> "2",
          "field-year"  -> "3"
        )
      ),
      objectStructure(
        field(
          "field",
          objectStructure(field("day", textNode("1")), field("month", textNode("2")), field("year", textNode("3"))))).asRight
    )
  }

  it must "create the correct JSON for simple fields in groups in non-repeating sections" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createGroup(createNonGroupField("field"))
        ) :: Nil,
        variadicFormData(
          "1_a_group" -> "",
          "1_field"   -> "fieldValue1",
          "2_a_group" -> "",
          "2_field"   -> "fieldValue2"
        )
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            textNode("fieldValue1"),
            textNode("fieldValue2")
          )),
        field("a_group", arrayNode(textNode(""), textNode("")))
      ).asRight
    )
  }

  it must "create the correct JSON for multivalue fields in groups in non-repeating sections" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createGroup(createMultiChoice("field"))
        ) :: Nil,
        variadicFormDataMany(
          "1_field" -> List(1, 2),
          "2_field" -> List(1, 3, 4)
        ) ++
          variadicFormData(
            "1_a_group" -> "",
            "2_a_group" -> ""
          )
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            arrayNode(textNode("1"), textNode("2")),
            arrayNode(textNode("1"), textNode("3"), textNode("4"))
          )),
        field("a_group", arrayNode(textNode(""), textNode("")))
      ).asRight
    )
  }

  it must "create the correct JSON for MultiField fields in groups in non-repeating sections" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createGroup(createDate("field"))
        ) :: Nil,
        variadicFormData(
          "1_a_group"     -> "",
          "2_a_group"     -> "",
          "3_a_group"     -> "",
          "1_field-day"   -> "1",
          "1_field-month" -> "2",
          "1_field-year"  -> "3",
          "2_field-day"   -> "4",
          "2_field-month" -> "5",
          "2_field-year"  -> "",
          "3_field-day"   -> "",
          "3_field-month" -> "",
          "3_field-year"  -> "6"
        )
      ),
      objectStructure(
        field("a_group", arrayNode(textNode(""), textNode(""), textNode(""))),
        field(
          "field",
          arrayNode(
            objectStructure(field("day", textNode("1")), field("month", textNode("2")), field("year", textNode("3"))),
            objectStructure(field("day", textNode("4")), field("month", textNode("5")), field("year", textNode(""))),
            objectStructure(field("day", textNode("")), field("month", textNode("")), field("year", textNode("6")))
          )
        )
      ).asRight
    )
  }

  it must "create the correct JSON for simple fields in repeating sections" in {
    validate(
      createFormModelVisibilityOptics(
        createRepeatingSection(2)(
          createNonGroupField("field")
        ) :: Nil,
        variadicFormData(
          "1_field" -> "fieldValue1",
          "2_field" -> "fieldValue2"
        )
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
      createFormModelVisibilityOptics(
        createRepeatingSection(2)(
          createMultiChoice("field")
        ) :: Nil,
        variadicFormDataMany(
          "1_field" -> List(1, 2),
          "2_field" -> List(1, 3, 4)
        )
      ),
      objectStructure(
        field(
          "field",
          arrayNode(
            arrayNode(textNode("1"), textNode("2")),
            arrayNode(textNode("1"), textNode("3"), textNode("4"))
          )
        )
      ).asRight
    )
  }

  it must "create the correct JSON for MultiField fields in repeating sections" in {
    validate(
      createFormModelVisibilityOptics(
        createRepeatingSection(2)(
          createDate("field")
        ) :: Nil,
        variadicFormData(
          "1_field"       -> "",
          "1_field-day"   -> "1",
          "1_field-month" -> "2",
          "1_field-year"  -> "3",
          "2_field"       -> "",
          "2_field-day"   -> "4",
          "2_field-month" -> "5",
          "2_field-year"  -> "6"
        )
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
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createRevealingChoice(
            "revealYourSecrets",
            false,
            createRevealingChoiceElement(createNonGroupField("revealedField1"), createDate("revealedDate")),
            createRevealingChoiceElement(createNonGroupField("unrevealedField1"))
          )
        ) :: Nil,
        variadicFormDataMany(
          "revealYourSecrets" -> List(0)
        ) ++
          variadicFormData(
            "revealedField1"     -> "revealedField1Value",
            "revealedDate-day"   -> "1",
            "revealedDate-month" -> "2",
            "revealedDate-year"  -> "3",
            "unrevealedField1"   -> "unrevealedField1Value"
          )
      ),
      objectStructure(
        field(
          "revealYourSecrets",
          objectStructure(
            field("choice", textNode("0")),
            field(
              "revealed",
              objectStructure(
                field("revealedField1", textNode("revealedField1Value")),
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
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createRevealingChoice(
            "revealYourSecrets",
            true,
            createRevealingChoiceElement(createNonGroupField("revealedField1"), createDate("revealedDate")),
            createRevealingChoiceElement(createNonGroupField("unrevealedField1")),
            createRevealingChoiceElement(createNonGroupField("revealedField2"))
          )
        ) :: Nil,
        variadicFormDataMany(
          "revealYourSecrets" -> List(0, 2)
        ) ++
          variadicFormData(
            "revealedField1"     -> "revealedField1Value",
            "revealedDate-day"   -> "1",
            "revealedDate-month" -> "2",
            "revealedDate-year"  -> "3",
            "unrevealedField1"   -> "unrevealedField1Value",
            "revealedField2"     -> "revealedField2Value"
          )
      ),
      objectStructure(
        field(
          "revealYourSecrets",
          objectStructure(
            field("choices", arrayNode(textNode("0"), textNode("2"))),
            field(
              "revealed",
              objectStructure(
                field("revealedField2", textNode("revealedField2Value")),
                field("revealedField1", textNode("revealedField1Value")),
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

  it must "create the correct JSON for multi value RevealingChoice components in add-to-list sections" in {
    validate(
      createFormModelVisibilityOptics(
        createAddToListSection(
          "addToListAnotherQuestion",
          List(
            createNonGroupField("cakeName"),
            createDate("deliveryDate"),
            createRevealingChoice(
              "cake",
              true,
              createRevealingChoiceElement(createNonGroupField("chocolatePieces")),
              createRevealingChoiceElement(createNonGroupField("lemonPieces"))
            )
          )
        ) :: Nil,
        variadicFormDataMany(
          "1_cake"                     -> List(0, 1),
          "2_cake"                     -> List(0, 1),
          "1_addToListAnotherQuestion" -> List(0),
          "2_addToListAnotherQuestion" -> List(1)
        ) ++
          variadicFormData(
            "1_cakeName"           -> "lemonCake",
            "1_lemonPieces"        -> "1",
            "1_chocolatePieces"    -> "2",
            "1_deliveryDate-day"   -> "1",
            "1_deliveryDate-month" -> "2",
            "1_deliveryDate-year"  -> "2003",
            "2_cakeName"           -> "chocolateCake",
            "2_lemonPieces"        -> "3",
            "2_chocolatePieces"    -> "4",
            "2_deliveryDate-day"   -> "4",
            "2_deliveryDate-month" -> "5",
            "2_deliveryDate-year"  -> "2006"
          )
      ),
      objectStructure(
        field(
          "addToListAnotherQuestion",
          arrayNode(
            objectStructure(
              field("cakeName", textNode("lemonCake")),
              field("addToListAnotherQuestion", arrayNode(textNode("0"))),
              field(
                "deliveryDate",
                objectStructure(
                  field("day", textNode("1")),
                  field("month", textNode("2")),
                  field("year", textNode("2003")))),
              field(
                "cake",
                objectStructure(
                  field("choices", arrayNode(textNode("0"), textNode("1"))),
                  field(
                    "revealed",
                    objectStructure(
                      field("lemonPieces", textNode("1")),
                      field("chocolatePieces", textNode("2"))
                    )
                  )
                )
              )
            ),
            objectStructure(
              field("cakeName", textNode("chocolateCake")),
              field("addToListAnotherQuestion", arrayNode(textNode("1"))),
              field(
                "deliveryDate",
                objectStructure(
                  field("day", textNode("4")),
                  field("month", textNode("5")),
                  field("year", textNode("2006")))),
              field(
                "cake",
                objectStructure(
                  field("choices", arrayNode(textNode("0"), textNode("1"))),
                  field(
                    "revealed",
                    objectStructure(
                      field("lemonPieces", textNode("3")),
                      field("chocolatePieces", textNode("4"))
                    )
                  )
                )
              )
            )
          )
        )
      ).asRight
    )
  }

  it must "include the declaration sections" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createNonGroupField("field")
        ) :: Nil,
        variadicFormData(
          "field"    -> "fieldValue",
          "decField" -> "decFieldValue"
        ),
        Some(DeclarationSection(toSmartString("Declaration"), None, None, List(createNonGroupField("decField"))))
      ),
      objectStructure(
        field("field", textNode("fieldValue")),
        field("decField", textNode("decFieldValue"))
      ).asRight
    )
  }

  it must "provide the correct alternative field names for robotic XML" in {
    def roboticsXmlPurposeMap(fieldName: String) = createStructuredPurposeMap(RoboticsXml, FieldName(fieldName))

    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createAddress("field")
        ) :: Nil,
        variadicFormData(
          "field-street1"  -> "1",
          "field-street2"  -> "2",
          "field-street3"  -> "3",
          "field-street4"  -> "4",
          "field-uk"       -> "5",
          "field-postcode" -> "6",
          "field-country"  -> "7"
        )
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

  it must "provide the correct alternative field names for robotic XML for repeated field" in {
    def roboticsXmlPurposeMap(fieldName: String) = createStructuredPurposeMap(RoboticsXml, FieldName(fieldName))
    validate(
      createFormModelVisibilityOptics(
        createAddToListSection(
          "addToListAnotherQuestion",
          List(
            createAddress("field")
          )
        ) :: Nil,
        variadicFormDataMany(
          "1_addToListAnotherQuestion" -> List(0),
          "2_addToListAnotherQuestion" -> List(1)
        ) ++
          variadicFormData(
            "1_field-street1"  -> "1",
            "1_field-street2"  -> "2",
            "1_field-street3"  -> "3",
            "1_field-street4"  -> "4",
            "1_field-uk"       -> "5",
            "1_field-postcode" -> "6",
            "1_field-country"  -> "7",
            "2_field-street1"  -> "11",
            "2_field-street2"  -> "12",
            "2_field-street3"  -> "13",
            "2_field-street4"  -> "14",
            "2_field-uk"       -> "15",
            "2_field-postcode" -> "16",
            "2_field-country"  -> "17"
          )
      ),
      objectStructure(
        field(
          "addToListAnotherQuestion",
          arrayNode(
            objectStructure(
              field("addToListAnotherQuestion", arrayNode(textNode("0"))),
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
            ),
            objectStructure(
              field("addToListAnotherQuestion", arrayNode(textNode("1"))),
              field(
                "field",
                objectStructure(
                  field("street1", textNode("11"), roboticsXmlPurposeMap("line1")),
                  field("street2", textNode("12"), roboticsXmlPurposeMap("line2")),
                  field("street3", textNode("13"), roboticsXmlPurposeMap("line3")),
                  field("street4", textNode("14"), roboticsXmlPurposeMap("line4")),
                  field("uk", textNode("15"), roboticsXmlPurposeMap("uk")),
                  field("postcode", textNode("16"), roboticsXmlPurposeMap("postcode")),
                  field("country", textNode("17"), roboticsXmlPurposeMap("country"))
                )
              )
            )
          )
        )).asRight
    )
  }

  it must "translate label from lookup component to its id" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createLookupField("field")
        ) :: Nil,
        variadicFormData(
          "field" -> "fieldValue"
        )
      ),
      objectStructure(
        field("field", textNode("field_id"))
      ).asRight
    )
  }

  it must "fail when label doesn't exist in lookup component" in {
    validate(
      createFormModelVisibilityOptics(
        createNonRepeatingSection(
          createLookupField("field")
        ) :: Nil,
        variadicFormData("field" -> "non existent label")
      ),
      StructuredFormDataBuilderException("Cannot find 'non existent label' in register Origin").asLeft
    )
  }

  private val lookupRegistry = new LookupRegistry(
    Map(
      Register.Origin -> RadioLookup(LocalisedLookupOptions(
        Map(LangADT.En -> LookupOptions(Map(LookupLabel("fieldValue") -> LookupInfo(LookupId("field_id"), 1))))))))

  private def validate[A](formModelVisibilityOptics: FormModelVisibilityOptics[DataOrigin.Mongo], expected: A)(
    implicit position: Position,
    l: LangADT): Assertion = {
    val sss: EitherEffect[StructuredFormValue.ObjectStructure] =
      StructuredFormDataBuilder[DataOrigin.Mongo, EitherEffect](
        formModelVisibilityOptics,
        destinationList,
        lookupRegistry)
    sss shouldBe expected
  }

  def createFormModelVisibilityOptics(
    sections: List[Section],
    data: VariadicFormData[SourceOrigin.OutOfDate],
    declarationSection: Option[DeclarationSection] = None
  ) = {

    val formModelOptics: FormModelOptics[DataOrigin.Mongo] =
      FormModelOptics.mkFormModelOptics[DataOrigin.Mongo, Id, SectionSelectorType.WithDeclaration](
        data,
        mkAuthCacheWithForm(mkFormTemplate(sections, declarationSection)),
        recalculation)

    formModelOptics.formModelVisibilityOptics
  }

  def destinationList: Destinations = DestinationList(
    NonEmptyList.one(Destination.Log(DestinationId("destination-id"))),
    ExampleData.acknowledgementSection,
    ExampleData.declarationSection
  )

  def createNonRepeatingSection(fields: FormComponent*): Section =
    Section.NonRepeatingPage(
      Page(
        toSmartString("non-repeating-page"),
        None,
        None,
        None,
        None,
        None,
        fields.toList,
        None,
        None
      ))

  def createRepeatingSection(repeats: Int)(fields: FormComponent*): Section =
    Section.RepeatingPage(
      Page(toSmartString("repeating-page"), None, None, None, None, None, fields.toList, None, None),
      Constant(repeats.toString)
    )

  def createAddToListSection(addAnotherQuestionName: String, fields: List[FormComponent]*): Section =
    mkAddToListSection(addAnotherQuestionName, fields: _*)

  def createNonGroupField(id: String): FormComponent =
    createFormComponent(id, Text(BasicText, Value))

  def createLookupField(id: String): FormComponent =
    createFormComponent(id, Text(Lookup(Register.Origin), Value))

  def createGroup(fields: FormComponent*): FormComponent =
    createFormComponent("a_group", Group(fields.toList, Some(5)))

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
    createFormComponent(id, RevealingChoice(element1 :: elements.toList, multiValue))

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
