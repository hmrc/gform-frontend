/*
 * Copyright 2025 HM Revenue & Customs
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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import uk.gov.hmrc.gform.gform.DataRetrieveResponseValidator._
import uk.gov.hmrc.gform.sharedmodel._

class DataRetrieveResponseValidatorSpec extends AnyWordSpec with Matchers {

  // Helper to create a DataRetrieve object with test instructions
  def createDataRetrieve(
    tpe: String,
    instructions: List[AttributeInstruction]
  ): DataRetrieve = DataRetrieve(
    DataRetrieve.Type(tpe),
    DataRetrieveId("test"),
    Attr.FromObject(instructions),
    Map.empty,
    List.empty,
    None,
    None,
    None
  )

  val validateBankDetailsInstructions = List(
    AttributeInstruction(
      DataRetrieve.Attribute("accountNumberIsWellFormatted"),
      ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted"))),
      Some(AllowedValues(List("yes", "no", "indeterminate"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
      ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs"))),
      Some(AllowedValues(List("yes", "no", "inapplicable"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeBankName"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeBankName"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("iban"),
      ConstructAttribute.AsIs(Fetch(List("iban"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    )
  )

  val businessBankAccountExistenceInstructions = List(
    AttributeInstruction(
      DataRetrieve.Attribute("accountNumberIsWellFormatted"),
      ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted"))),
      Some(AllowedValues(List("yes", "no", "indeterminate"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeBankName"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeBankName"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
      ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs"))),
      Some(AllowedValues(List("yes", "no", "inapplicable"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("accountExists"),
      ConstructAttribute.AsIs(Fetch(List("accountExists"))),
      Some(AllowedValues(List("yes", "no", "inapplicable", "indeterminate", "error"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("nameMatches"),
      ConstructAttribute.AsIs(Fetch(List("nameMatches"))),
      Some(
        AllowedValues(
          List("yes", "partial", "no", "inapplicable", "indeterminate", "error"),
          AllowedValueType.JsStringType
        )
      )
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("accountName"),
      ConstructAttribute.AsIs(Fetch(List("accountName"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("iban"),
      ConstructAttribute.AsIs(Fetch(List("iban"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
    )
  )

  "validateBankDetailsResponse" should {
    val dataRetrieve = createDataRetrieve("validateBankDetails", validateBankDetailsInstructions)

    "return ValidationSuccess for valid response" in {
      val validJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "yes",
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "sortCodeIsPresentOnEISCD": "yes",
          "sortCodeBankName": "Test Bank",
          "sortCodeSupportsDirectDebit": "yes",
          "sortCodeSupportsDirectCredit": "yes",
          "iban": "GB82WEST12345698765432"
        }
      """)

      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "return ValidationFailure for unexpected key" in {
      val invalidJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "yes",
          "unexpectedKey": "value",
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "sortCodeIsPresentOnEISCD": "yes"
        }
      """)

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("unexpected key in JSON response 'unexpectedKey'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "return ValidationFailure for unexpected value" in {
      val invalidJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "maybe",
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "sortCodeIsPresentOnEISCD": "yes"
        }
      """)

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("unexpected value for 'accountNumberIsWellFormatted': 'maybe'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "return ValidationFailure for missing required field" in {
      val invalidJson = Json.parse("""
        {
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "sortCodeIsPresentOnEISCD": "yes"
        }
      """)

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("missing required field 'accountNumberIsWellFormatted'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "allow empty optional fields" in {
      val validJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "yes",
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "sortCodeIsPresentOnEISCD": "yes",
          "sortCodeBankName": "",
          "iban": ""
        }
      """)

      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }
  }

  "validateBusinessBankAccountExistenceResponse" should {
    val dataRetrieve = createDataRetrieve("businessBankAccountExistence", businessBankAccountExistenceInstructions)

    "return ValidationSuccess for valid response" in {
      val validJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "yes",
          "sortCodeIsPresentOnEISCD": "yes",
          "sortCodeBankName": "Test Bank",
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "accountExists": "yes",
          "nameMatches": "yes",
          "accountName": "Test Account",
          "sortCodeSupportsDirectDebit": "yes",
          "sortCodeSupportsDirectCredit": "yes",
          "iban": "GB82WEST12345698765432"
        }
      """)

      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }
  }

  "validatePersonalBankAccountExistenceResponse" should {
    val dataRetrieve = createDataRetrieve("personalBankAccountExistence", businessBankAccountExistenceInstructions)

    "return ValidationSuccess for valid response" in {
      val validJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "yes",
          "accountExists": "yes",
          "nameMatches": "partial",
          "accountName": "Test Account",
          "nonStandardAccountDetailsRequiredForBacs": "no",
          "sortCodeIsPresentOnEISCD": "yes",
          "sortCodeSupportsDirectDebit": "yes",
          "sortCodeSupportsDirectCredit": "yes",
          "sortCodeBankName": "Test Bank",
          "iban": ""
        }
      """)

      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }
  }

  "validateDataRetrieveResponse" should {
    "handle wildcard allowed values correctly" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("anyField"),
          ConstructAttribute.AsIs(Fetch(List("anyField"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"anyField": "any value here"}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "reject 'indeterminate' for nonStandardAccountDetailsRequiredForBacs" in {
      val dataRetrieve = createDataRetrieve("validateBankDetails", validateBankDetailsInstructions)

      val invalidJson = Json.parse("""
        {
          "accountNumberIsWellFormatted": "yes",
          "nonStandardAccountDetailsRequiredForBacs": "indeterminate",
          "sortCodeIsPresentOnEISCD": "yes"
        }
      """)

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("unexpected value for 'nonStandardAccountDetailsRequiredForBacs': 'indeterminate'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "handle nested paths correctly" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("addressLine1"),
          ConstructAttribute.AsIs(Fetch(List("address", "address_line_1"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("postcode"),
          ConstructAttribute.AsIs(Fetch(List("address", "postal_code"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""
        {
          "address": {
            "address_line_1": "123 Main St",
            "postal_code": "SW1A 1AA"
          }
        }
      """)

      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "validate missing nested fields" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("addressLine1"),
          ConstructAttribute.AsIs(Fetch(List("address", "address_line_1"))),
          Some(AllowedValues(List("test1", "test2"), AllowedValueType.JsStringType))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val invalidJson = Json.parse("""{"address": {}}""")

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("missing required field 'address.address_line_1'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "handle Concat construct type" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("fullName"),
          ConstructAttribute.Concat(List(Fetch(List("firstName")), Fetch(List("lastName")))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"firstName": "John", "lastName": "Doe"}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "handle Combine construct type" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("combinedAddress"),
          ConstructAttribute.Combine(
            List(
              DataRetrieve.Attribute("line1") -> Fetch(List("address", "line1")),
              DataRetrieve.Attribute("city")  -> Fetch(List("address", "city"))
            )
          ),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"address": {"line1": "123 Main St", "city": "London"}}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "handle ExtractAtIndex construct type" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("firstCode"),
          ConstructAttribute.ExtractAtIndex(Fetch(List("sic_codes")), 0),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"sic_codes": ["1234", "5678"]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }
  }

  "FromArray validation" should {
    "validate array responses successfully" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("name"),
          ConstructAttribute.AsIs(Fetch(List("name"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValue))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("role"),
          ConstructAttribute.AsIs(Fetch(List("role"))),
          Some(AllowedValues(List("director", "secretary", "llp-member"), AllowedValueType.JsStringType))
        )
      )
      val dataRetrieve = DataRetrieve(
        DataRetrieve.Type("test"),
        DataRetrieveId("testId"),
        Attr.FromArray(instructions),
        Map.empty,
        List.empty,
        None,
        None,
        None
      )

      val validJson = Json.parse("""[
        {"name": "John Doe", "role": "director"},
        {"name": "Jane Smith", "role": "llp-member"}
      ]""")

      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "return failure for invalid array element" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("role"),
          ConstructAttribute.AsIs(Fetch(List("role"))),
          Some(AllowedValues(List("director", "secretary"), AllowedValueType.JsStringType))
        )
      )
      val dataRetrieve = DataRetrieve(
        DataRetrieve.Type("test"),
        DataRetrieveId("testId"),
        Attr.FromArray(instructions),
        Map.empty,
        List.empty,
        None,
        None,
        None
      )

      val invalidJson = Json.parse("""[
        {"role": "director"},
        {"role": "invalid-role"}
      ]""")

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("[index 1] unexpected value for 'role': 'invalid-role'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "return failure when expecting array but receiving object" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("field"),
          ConstructAttribute.AsIs(Fetch(List("field"))),
          None
        )
      )
      val dataRetrieve = DataRetrieve(
        DataRetrieve.Type("test"),
        DataRetrieveId("testId"),
        Attr.FromArray(instructions),
        Map.empty,
        List.empty,
        None,
        None,
        None
      )

      val invalidJson = Json.parse("""{"field": "value"}""")

      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("response must be a JSON array")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }
  }
}
