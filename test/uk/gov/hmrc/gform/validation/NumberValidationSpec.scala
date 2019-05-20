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

package uk.gov.hmrc.gform.validation

import cats.implicits._
import org.scalatest.mockito.MockitoSugar.mock
import org.scalatest.prop.TableDrivenPropertyChecks
import play.api.i18n.Messages
import uk.gov.hmrc.gform.Helpers.toLocalisedString
import uk.gov.hmrc.gform.auth.models.MaterialisedRetrievals
import uk.gov.hmrc.gform.fileupload.FileUploadService
import uk.gov.hmrc.gform.lookup.LookupRegistry
import uk.gov.hmrc.gform.sharedmodel.{ ExampleData, LangADT, LocalisedString }
import uk.gov.hmrc.gform.sharedmodel.form.{ EnvelopeId, FormField, ThirdPartyData }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.http.HeaderCarrier

class NumberValidationSpec(implicit messages: Messages, l: LangADT)
    extends Spec with TableDrivenPropertyChecks with GraphSpec {

  trait Test extends ExampleData {
    def value: String

    private val lookupRegistry = new LookupRegistry(Map.empty)

    override def `formField - number` = FormField(`fieldId - number`, value)

    override def `fieldValue - number` = FormComponent(
      `fieldId - number`,
      Text(Number(), Value),
      toLocalisedString("sample label"),
      None,
      None,
      None,
      true,
      false,
      false,
      false,
      false,
      None
    )

    override def data = Map(
      `fieldId - number` -> `formField - number`
    )

    def validate(fieldValue: FormComponent, fieldValues: List[FormComponent], data: Map[FormComponentId, Seq[String]]) =
      new ComponentsValidator(
        mkFormDataRecalculated(data),
        mock[FileUploadService],
        EnvelopeId("whatever"),
        retrievals,
        booleanExprEval,
        ThirdPartyData.empty,
        ExampleData.formTemplate,
        lookupRegistry
      ).validate(fieldValue, fieldValues)

    implicit lazy val hc: HeaderCarrier = HeaderCarrier()
  }

  val retrievals: MaterialisedRetrievals = mock[MaterialisedRetrievals]

  "A number which satisfies the whole shape and fractional shape pattern" should "be accepted as a valid number" in {
    val numbers =
      Table(
        "number",
        "+32432",
        "123",
        "+32,432",
        "+32,432.",
        "-32432",
        "-32,432",
        "-32,432.",
        "32432",
        "32,432",
        "32,432.",
        "+.23",
        "+232.23",
        "+2,232.23",
        "-.23",
        "-232.23",
        "-2,232.23",
        ".23",
        "232.23",
        "2,232.23",
        "1,234,567,890",
        "1,234,567,890.12",
        "+1,234,567,890",
        "-1,234,567,890.12",
        "£1,234"
      )

    forAll(numbers) { number =>
      new Test {
        override val value = number
        validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe ().valid
      }
    }
  }

  "A number which does not satisfy the whole shape and fractional shape pattern" should "not be accepted as a valid number" in {

    val numbers =
      Table(
        "number",
        "+3,2432",
        "+3,2432.",
        "-3,2432",
        "-3,2432.",
        "3,2432",
        "3,2432.",
        "+2,32.23",
        "-2,32.23",
        "2,32.23",
        "12,34,567,890",
        "12,34,567,890.12",
        "+12,34,567,890",
        "12,34,567,890.12",
        "£12,34",
        "THX1138"
      )

    forAll(numbers) { number =>
      new Test {
        override val value = number
        val expectedError = Map(`fieldValue - number`.id -> Set("sample label must be a number")).invalid[Unit]
        validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe expectedError
      }
    }
  }

  "A number which does not satisfy the fractional part rules" should "not be accepted as a valid number" in {
    val numbers =
      Table(
        ("number", "expected"),
        (
          "1234567890123456789.87654321",
          Map(
            `fieldValue - number`.id -> Set(
              "sample label must be at most 11 whole digits and decimal fraction must be at most 2 digits"))
            .invalid[Unit]),
        (
          "1234567890123456789.87",
          Map(`fieldValue - number`.id -> Set("sample label must be at most 11 whole digits"))
            .invalid[Unit]),
        (
          "9.87654321",
          Map(`fieldValue - number`.id -> Set("sample label must be at most 2 digits"))
            .invalid[Unit])
      )

    forAll(numbers) { (number, expected) =>
      new Test {
        override val value = number
        validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe expected
      }
    }
  }

  "A number with applied constraints" should "be invalid if it does not satisfy those constraints" in {
    val numbers =
      Table(
        ("number", "constraint", "expected"),
        (
          "123.21",
          Text(Number(2, 1), Value),
          Map(
            `fieldValue - number`.id -> Set(
              "sample label must be at most 2 whole digits and decimal fraction must be at most 1 digits"))
            .invalid[Unit]),
        //return invalid for too many whole digits
        (
          "1234567890123456789",
          Text(Number(maxFractionalDigits = 0), Value),
          Map(`fieldValue - number`.id -> Set("sample label must be at most 11 digits"))
            .invalid[Unit]),
        (
          "-123",
          Text(PositiveNumber(), Value),
          Map(`fieldValue - number`.id -> Set("sample label must be a positive number"))
            .invalid[Unit]),
        ("123", Text(PositiveNumber(), Value), ().valid),
        //return invalid for decimal fractions
        (
          "123.4",
          Text(PositiveNumber(maxFractionalDigits = 0), Value),
          Map(`fieldValue - number`.id -> Set("sample label must be a whole number"))
            .invalid[Unit])
      )

    forAll(numbers) { (number, constraint, expected) =>
      new Test {
        override val value = number
        override def `fieldValue - number` = super.`fieldValue - number`.copy(`type` = constraint)
        validate(`fieldValue - number`, List(`fieldValue - number`), rawDataFromBrowser).futureValue shouldBe expected
      }
    }
  }
}
