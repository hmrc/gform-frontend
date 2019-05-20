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

package uk.gov.hmrc.gform.sharedmodel.formtemplate

import cats.data.NonEmptyList
import org.scalactic.source.Position
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen

class FormComponentSpec extends Spec {
  private val exprText = Text(AnyText, Add(Constant("1"), FormCtx("other-field-id")))
  private val exprTextArea = TextArea(AnyText, Value)
  private val exprAddress = Address(international = false)
  private val exprUKSortCode = UkSortCode(Value)
  private val exprDate = Date(AnyDate, Offset(0), None)
  private val exprChoice =
    Choice(Checkbox, NonEmptyList(toLocalisedString("Natural gas"), Nil), Vertical, List.empty[Int], None)
  private val exprInformationMessage = InformationMessage(StandardInfo, toLocalisedString("Info text"))
  private val exprFileUpload = FileUpload()

  private val labelNoCounter = "Label no counter"
  private val shortNameNoCounter = "Short name no counter."

  "FormComponent" should "round trip derived JSON" in {
    forAll(FormComponentGen.formComponentGen()) { value =>
      FormComponent.format.reads(FormComponent.format.writes(value)) should beJsSuccess(value)
    }
  }

  it should "not expand Text, TextArea, UkSortCode, Data, Address, Choice, InformationMessage, FileUpload" in {
    notExpand(exprText)
    notExpand(exprTextArea)
    notExpand(exprUKSortCode)
    notExpand(exprDate)
    notExpand(exprAddress)
    notExpand(exprChoice)
    notExpand(exprInformationMessage)
    notExpand(exprFileUpload)
  }

  it should "expand Group with one field" in {
    val max = 5

    val fc =
      mkFormComponent(
        "group-id",
        Group(
          List(mkFormComponent("text-id", exprText, "$n. Some label", "Short name for $n.")),
          Vertical,
          Some(max),
          None,
          None,
          None),
        "$n. Some label",
        "Short name for $n."
      )

    val result = fc.expandFormComponentFull.formComponents

    val expected = List(
      mkFormComponent("text-id", exprText, "1. Some label", "Short name for 1."),
      mkFormComponent("1_text-id", exprText, "2. Some label", "Short name for 2."),
      mkFormComponent("2_text-id", exprText, "3. Some label", "Short name for 3."),
      mkFormComponent("3_text-id", exprText, "4. Some label", "Short name for 4."),
      mkFormComponent("4_text-id", exprText, "5. Some label", "Short name for 5.")
    )

    result.size should be(5)
    result should be(expected)

  }

  it should "be able to recognise when a text component needs to be capitalised" in {
    val toBeCapitalised =
      mkFormComponent("anything", Text(BasicText, Value, DisplayWidth.DEFAULT, IsUpperCase), "anything", "anything")
    val toNotBeCapitalised =
      mkFormComponent("anything", Text(BasicText, Value, DisplayWidth.DEFAULT, IsNotUpperCase), "anything", "anything")
    val upperCaseresult = IsCapitalised.unapply(toBeCapitalised)
    val lowerCaseResult = IsCapitalised.unapply(toNotBeCapitalised)

    upperCaseresult shouldBe true
    lowerCaseResult shouldBe false
  }

  it should "expand Group with multiple fields" in {
    val max = 3

    val fc = mkFormComponent(
      "group-id",
      Group(
        List(
          mkFormComponent("text-id", exprText, "$n. Some label", "Short name for $n."),
          mkFormComponent("text-area-id", exprTextArea, "Some label $n", shortNameNoCounter),
          mkFormComponent("address-id", exprAddress, labelNoCounter, "Short name $n."),
          mkFormComponent("uk-sort-code", exprUKSortCode, labelNoCounter, shortNameNoCounter),
          mkFormComponent("date-id", exprDate, labelNoCounter, shortNameNoCounter),
          mkFormComponent("choice-id", exprChoice, labelNoCounter, shortNameNoCounter),
          mkFormComponent("info-message", exprInformationMessage, labelNoCounter, shortNameNoCounter),
          mkFormComponent("file-upload", exprFileUpload, labelNoCounter, shortNameNoCounter)
        ),
        Vertical,
        Some(max),
        None,
        None,
        None
      ),
      "Group label",
      "Group short name"
    )
    val result = fc.expandFormComponentFull.formComponents

    val expected = List(
      mkFormComponent("text-id", exprText, "1. Some label", "Short name for 1."),
      mkFormComponent("text-area-id", exprTextArea, "Some label 1", shortNameNoCounter),
      mkFormComponent("address-id", exprAddress, labelNoCounter, "Short name 1."),
      mkFormComponent("uk-sort-code", exprUKSortCode, labelNoCounter, shortNameNoCounter),
      mkFormComponent("date-id", exprDate, labelNoCounter, shortNameNoCounter),
      mkFormComponent("choice-id", exprChoice, labelNoCounter, shortNameNoCounter),
      mkFormComponent("info-message", exprInformationMessage, labelNoCounter, shortNameNoCounter),
      mkFormComponent("file-upload", exprFileUpload, labelNoCounter, shortNameNoCounter),
      mkFormComponent("1_text-id", exprText, "2. Some label", "Short name for 2."),
      mkFormComponent("1_text-area-id", exprTextArea, "Some label 2", shortNameNoCounter),
      mkFormComponent("1_address-id", exprAddress, labelNoCounter, "Short name 2."),
      mkFormComponent("1_uk-sort-code", exprUKSortCode, labelNoCounter, shortNameNoCounter),
      mkFormComponent("1_date-id", exprDate, labelNoCounter, shortNameNoCounter),
      mkFormComponent("1_choice-id", exprChoice, labelNoCounter, shortNameNoCounter),
      mkFormComponent("1_info-message", exprInformationMessage, labelNoCounter, shortNameNoCounter),
      mkFormComponent("1_file-upload", exprFileUpload, labelNoCounter, shortNameNoCounter),
      mkFormComponent("2_text-id", exprText, "3. Some label", "Short name for 3."),
      mkFormComponent("2_text-area-id", exprTextArea, "Some label 3", shortNameNoCounter),
      mkFormComponent("2_address-id", exprAddress, labelNoCounter, "Short name 3."),
      mkFormComponent("2_uk-sort-code", exprUKSortCode, labelNoCounter, shortNameNoCounter),
      mkFormComponent("2_date-id", exprDate, labelNoCounter, shortNameNoCounter),
      mkFormComponent("2_choice-id", exprChoice, labelNoCounter, shortNameNoCounter),
      mkFormComponent("2_info-message", exprInformationMessage, labelNoCounter, shortNameNoCounter),
      mkFormComponent("2_file-upload", exprFileUpload, labelNoCounter, shortNameNoCounter)
    )

    result.size should be(24)
    result should be(expected)
  }

  private def notExpand(ct: ComponentType)(implicit position: Position) = {
    val fc = mkFormComponent("some-component", ct, "$n. Some label", "Short name for $n.")
    fc.expandFormComponentFull.formComponents should be(fc :: Nil)
  }

  private def mkFormComponent(fcId: String, ct: ComponentType, label: String, shortName: String) =
    FormComponent(
      FormComponentId(fcId),
      ct,
      toLocalisedString(label),
      None,
      Some(toLocalisedString(shortName)),
      None,
      true,
      false,
      true,
      false,
      false,
      None,
      None
    )
}
