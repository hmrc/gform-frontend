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

package uk.gov.hmrc.gform.views.summary

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FieldOk

class TextFormatterSpec extends Spec {

  def getComponent(text: Text) = FormComponent(
    `fieldId - firstName`,
    text, "First Name", None, None, mandatory = true, editable = true, submissible = true, derived = true,
    None, None
  )

  def getValidationResult(component: FormComponent, value: String) = Some(FieldOk(component, value))

  def testValuesLessThan5Digits(text: Text) = {
    val component = getComponent(text)

    TextFormatter.formatText(text, getValidationResult(component, "1000")) shouldBe "1000"
    TextFormatter.formatText(text, getValidationResult(component, "1000.00")) shouldBe "1000.00"
  }

  def testValuesGreaterThan5Digits(text: Text) = {
    val component = getComponent(text)

    TextFormatter.formatText(text, getValidationResult(component, "10000")) shouldBe "10,000"
    TextFormatter.formatText(text, getValidationResult(component, "10000.00")) shouldBe "10,000.00"
    TextFormatter.formatText(text, getValidationResult(component, "100000")) shouldBe "100,000"
    TextFormatter.formatText(text, getValidationResult(component, "100000.00")) shouldBe "100,000.00"
  }

  def testValuesWithPoundSignsAndCommas(text: Text) = {
    val component = getComponent(text)

    TextFormatter.formatText(text, getValidationResult(component, "£100,00.00")) shouldBe "10,000.00"
    TextFormatter.formatText(text, getValidationResult(component, "£10,0.00")) shouldBe "100.00"
    TextFormatter.formatText(text, getValidationResult(component, "£88666,564.59")) shouldBe "88,666,564.59"
  }

  "formatText" should "not add commas for Text component with Sterling constraint and current value's length less than 5" in {
    testValuesLessThan5Digits(Text(Sterling, Constant("")))
  }

  it should "add commas for Text component with Sterling constraint and current value's length equal or greater 5" in {
    testValuesGreaterThan5Digits(Text(Sterling, Constant("")))
  }

  it should "remove pound signs and commas input for Text component with Sterling constraint" in {
    testValuesWithPoundSignsAndCommas(Text(Sterling, Constant("")))
  }

  "formatText" should "not add commas for Text component with Number constraint and current value's length less than 5" in {
    testValuesLessThan5Digits(Text(Number(), Constant("")))
  }

  it should "add commas for Text component with Number constraint and current value's length equal or greater 5" in {
    testValuesGreaterThan5Digits(Text(Number(), Constant("")))
  }

  it should "remove pound signs and commas input for Text component with Number constraint" in {
    testValuesWithPoundSignsAndCommas(Text(Number(), Constant("")))
  }

  "formatText" should "not add commas for Text component with PositiveNumber constraint and current value's length less than 5" in {
    testValuesLessThan5Digits(Text(PositiveNumber(), Constant("")))
  }

  it should "add commas for Text component with PositiveNumber constraint and current value's length equal or greater 5" in {
    testValuesGreaterThan5Digits(Text(PositiveNumber(), Constant("")))
  }

  it should "remove pound signs and commas input for Text component with PositiveNumber constraint " in {
    testValuesWithPoundSignsAndCommas(Text(PositiveNumber(), Constant("")))
  }

  "formatText" should "add a space to a bank account number" in {
    val text = Text(UkBankAccountNumber, Constant(""))
    val component = getComponent(text)

    TextFormatter.formatText(text, getValidationResult(component, "12345678")) shouldBe "1234 5678"
  }
}
