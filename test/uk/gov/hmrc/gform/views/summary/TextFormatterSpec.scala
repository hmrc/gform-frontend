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

package uk.gov.hmrc.gform.views.summary

import play.api.i18n.Messages
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.FieldOk

class TextFormatterSpec(implicit messages: Messages) extends Spec {

  def getComponent(constraint: TextConstraint) = FormComponent(
    `fieldId - firstName`,
    Text(constraint, Value),
    toLocalisedString("First Name"),
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

  val equalsCombinations = Table(
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
    ("£-1,234,£56£78,9.12",   "-£123,456,789.12", "-123,456,789.12"), // ignore spurious £
    ("bogus",                 "bogus",            "bogus")            // unknown values are rendered unaltered
    // format: on
  )

  forAll(equalsCombinations) { (input, expectedSterling, expectedNumber) =>
    def formatForConstraint(constraint: TextConstraint) =
      TextFormatter.formatText(Some(FieldOk(getComponent(constraint), input)))

    formatForConstraint(Sterling.defaultRounding) shouldBe expectedSterling
    formatForConstraint(Number()) shouldBe expectedNumber
    formatForConstraint(PositiveNumber()) shouldBe expectedNumber
  }
}
