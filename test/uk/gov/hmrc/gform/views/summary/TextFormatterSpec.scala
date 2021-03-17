/*
 * Copyright 2021 HM Revenue & Customs
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

import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Messages
import play.api.test.Helpers
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.eval.smartstring.SmartStringEvaluator
import uk.gov.hmrc.gform.fileupload.EnvelopeWithMapping
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString, SmartString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FieldOk

class TextFormatterSpec extends Spec with TableDrivenPropertyChecks {

  implicit val messages: Messages = Helpers.stubMessages()

  private val positiveNumber = PositiveNumber()
  private val simpleNumber = Number()
  private val sterling = Sterling(RoundingMode.defaultRoundingMode, false)
  private val shortText = ShortText(3, 20)
  private val unit = LocalisedString(Map(LangADT.En -> "litres", LangADT.Cy -> "litr"))
  private val positiveNumberWithUnit = positiveNumber.copy(unit = Some(unit))
  private val simpleNumberWithUnit = simpleNumber.copy(unit = Some(unit))
  private val prefixSmartString = Some(
    SmartString(LocalisedString(Map(LangADT.En -> "enPrefix", LangADT.Cy -> "cyPrefix")), Nil)
  )
  private val suffixSmartString = Some(
    SmartString(LocalisedString(Map(LangADT.En -> "enSuffix", LangADT.Cy -> "cySuffix")), Nil)
  )

  private val langADTForEn: LangADT = LangADT.En
  private val langADTForCy: LangADT = LangADT.Cy
  private val smartStringEvaluatorForCy: SmartStringEvaluator = new SmartStringEvaluator {
    override def apply(s: SmartString, markDown: Boolean): String = s.rawValue(LangADT.Cy)
  }

  def getComponent(constraint: TextConstraint) = FormComponent(
    `fieldId - firstName`,
    Text(constraint, Value),
    toSmartString("First Name"),
    None,
    None,
    None,
    None,
    mandatory = true,
    editable = true,
    submissible = true,
    derived = true,
    onlyShowOnSummary = false,
    None,
    None
  )

  val equalsCombinations = TableDrivenPropertyChecks.Table(
    // format: off
    ("value",     "sterlingResult",  "numberResult"),
    ("1",         "£1.00",           "1"),
    ("12",        "£12.00",          "12"),
    ("123",       "£123.00",         "123"),
    ("1234",      "£1,234.00",       "1,234"),
    ("12345",     "£12,345.00",      "12,345"),
    ("123456",    "£123,456.00",     "123,456"),
    ("1234567",   "£1,234,567.00",   "1,234,567"),
    ("12345678",  "£12,345,678.00",  "12,345,678"),
    ("123456789", "£123,456,789.00", "123,456,789"),
    ("-1",         "-£1.00",           "-1"),
    ("-12",        "-£12.00",          "-12"),
    ("-123",       "-£123.00",         "-123"),
    ("-1234",      "-£1,234.00",       "-1,234"),
    ("-12345",     "-£12,345.00",      "-12,345"),
    ("-123456",    "-£123,456.00",     "-123,456"),
    ("-1234567",   "-£1,234,567.00",   "-1,234,567"),
    ("-12345678",  "-£12,345,678.00",  "-12,345,678"),
    ("-123456789", "-£123,456,789.00", "-123,456,789"),
    ("1.12",         "£1.12",           "1.12"),
    ("12.12",        "£12.12",          "12.12"),
    ("123.12",       "£123.12",         "123.12"),
    ("1234.12",      "£1,234.12",       "1,234.12"),
    ("12345.12",     "£12,345.12",      "12,345.12"),
    ("123456.12",    "£123,456.12",     "123,456.12"),
    ("1234567.12",   "£1,234,567.12",   "1,234,567.12"),
    ("12345678.12",  "£12,345,678.12",  "12,345,678.12"),
    ("123456789.12", "£123,456,789.12", "123,456,789.12"),
    ("-1.12",         "-£1.12",           "-1.12"),
    ("-12.12",        "-£12.12",          "-12.12"),
    ("-123.12",       "-£123.12",         "-123.12"),
    ("-1234.12",      "-£1,234.12",       "-1,234.12"),
    ("-12345.12",     "-£12,345.12",      "-12,345.12"),
    ("-123456.12",    "-£123,456.12",     "-123,456.12"),
    ("-1234567.12",   "-£1,234,567.12",   "-1,234,567.12"),
    ("-12345678.12",  "-£12,345,678.12",  "-12,345,678.12"),
    ("-123456789.12", "-£123,456,789.12", "-123,456,789.12"),
    ("-1,2,3,4,5,6,7,8,9.12", "-£123,456,789.12", "-123,456,789.12"), // ignore spurious commas
    ("-1,234,5678,9.12",      "-£123,456,789.12", "-123,456,789.12"),
    ("bogus",                 "bogus",            "bogus")            // unknown values are rendered unaltered
    // format: on
  )

  forAll(equalsCombinations) { (input, expectedSterling, expectedNumber) =>
    implicit val l: LangADT = LangADT.En
    def formatForConstraint(constraint: TextConstraint) =
      TextFormatter.formatText(FieldOk(getComponent(constraint), input), EnvelopeWithMapping.empty)

    formatForConstraint(Sterling(RoundingMode.defaultRoundingMode, false)) shouldBe expectedSterling
    formatForConstraint(Number()) shouldBe expectedNumber
    formatForConstraint(PositiveNumber()) shouldBe expectedNumber
  }

  "componentTextReadonly for positiveNumber" should "return correct string for prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", positiveNumber, prefixSmartString, suffixSmartString)
    result shouldBe "enPrefix value enSuffix"
  }

  it should "return correct string for prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", positiveNumber, prefixSmartString, suffixSmartString)
    result shouldBe "cyPrefix value cySuffix"
  }

  it should "return correct string for only prefix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", positiveNumberWithUnit, prefixSmartString, None)
    result shouldBe "enPrefix value litres"
  }

  it should "return correct string for only prefix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", positiveNumberWithUnit, prefixSmartString, None)
    result shouldBe "cyPrefix value litr"
  }

  it should "return correct string for only suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", positiveNumberWithUnit, None, suffixSmartString)
    result shouldBe "value enSuffix"
  }

  it should "return correct string for only suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", positiveNumberWithUnit, None, suffixSmartString)
    result shouldBe "value cySuffix"
  }

  it should "return correct string for no prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", positiveNumberWithUnit, None, None)
    result shouldBe "value litres"
  }

  it should "return correct string for no prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", positiveNumberWithUnit, None, None)
    result shouldBe "value litr"
  }

  "componentTextReadonly for Number" should "return correct string for prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", simpleNumber, prefixSmartString, suffixSmartString)
    result shouldBe "enPrefix value enSuffix"
  }

  it should "return correct string for prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", simpleNumber, prefixSmartString, suffixSmartString)
    result shouldBe "cyPrefix value cySuffix"
  }

  it should "return correct string for only prefix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", simpleNumberWithUnit, prefixSmartString, None)
    result shouldBe "enPrefix value litres"
  }

  it should "return correct string for only prefix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", simpleNumberWithUnit, prefixSmartString, None)
    result shouldBe "cyPrefix value litr"
  }

  it should "return correct string for only suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", simpleNumberWithUnit, None, suffixSmartString)
    result shouldBe "value enSuffix"
  }

  it should "return correct string for only suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", simpleNumberWithUnit, None, suffixSmartString)
    result shouldBe "value cySuffix"
  }

  it should "return correct string for no prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", simpleNumberWithUnit, None, None)
    result shouldBe "value litres"
  }

  it should "return correct string for no prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", simpleNumberWithUnit, None, None)
    result shouldBe "value litr"
  }

  "componentTextReadonly for Sterling" should "return correct string for prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("10000", sterling, prefixSmartString, suffixSmartString)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("10000", sterling, prefixSmartString, suffixSmartString)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for only prefix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("10000", sterling, prefixSmartString, None)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for only prefix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("10000", sterling, prefixSmartString, None)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for only suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("10000", sterling, None, suffixSmartString)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for only suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("10000", sterling, None, suffixSmartString)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for no prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("10000", sterling, None, None)
    result shouldBe "£10,000.00"
  }

  it should "return correct string for no prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("10000", sterling, None, None)
    result shouldBe "£10,000.00"
  }

  "componentTextReadonly for ShortText" should "return correct string for prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", shortText, prefixSmartString, suffixSmartString)
    result shouldBe "enPrefix value enSuffix"
  }

  it should "return correct string for prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", shortText, prefixSmartString, suffixSmartString)
    result shouldBe "cyPrefix value cySuffix"
  }

  it should "return correct string for only prefix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", shortText, prefixSmartString, None)
    result shouldBe "enPrefix value"
  }

  it should "return correct string for only prefix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", shortText, prefixSmartString, None)
    result shouldBe "cyPrefix value"
  }

  it should "return correct string for only suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", shortText, None, suffixSmartString)
    result shouldBe "value enSuffix"
  }

  it should "return correct string for only suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", shortText, None, suffixSmartString)
    result shouldBe "value cySuffix"
  }

  it should "return correct string for no prefix and suffix for language En" in {
    implicit val l: LangADT = langADTForEn
    val result = TextFormatter.componentTextForSummary("value", shortText, None, None)
    result shouldBe "value"
  }

  it should "return correct string for no prefix and suffix for language Cy" in {
    implicit val l: LangADT = langADTForCy
    implicit val smartStringEvaluator: SmartStringEvaluator = smartStringEvaluatorForCy

    val result = TextFormatter.componentTextForSummary("value", shortText, None, None)
    result shouldBe "value"
  }
}
