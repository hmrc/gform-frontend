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
      Some(AllowedValues(List("yes", "no", "indeterminate"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
      ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs"))),
      Some(AllowedValues(List("yes", "no", "inapplicable"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeBankName"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeBankName"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("iban"),
      ConstructAttribute.AsIs(Fetch(List("iban"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
    )
  )

  val businessBankAccountExistenceInstructions = List(
    AttributeInstruction(
      DataRetrieve.Attribute("accountNumberIsWellFormatted"),
      ConstructAttribute.AsIs(Fetch(List("accountNumberIsWellFormatted"))),
      Some(AllowedValues(List("yes", "no", "indeterminate"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeIsPresentOnEISCD"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeIsPresentOnEISCD"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeBankName"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeBankName"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("nonStandardAccountDetailsRequiredForBacs"),
      ConstructAttribute.AsIs(Fetch(List("nonStandardAccountDetailsRequiredForBacs"))),
      Some(AllowedValues(List("yes", "no", "inapplicable"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("accountExists"),
      ConstructAttribute.AsIs(Fetch(List("accountExists"))),
      Some(
        AllowedValues(
          List("yes", "no", "inapplicable", "indeterminate", "error"),
          AllowedValueType.JsStringType,
          isRequired = true
        )
      )
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("nameMatches"),
      ConstructAttribute.AsIs(Fetch(List("nameMatches"))),
      Some(
        AllowedValues(
          List("yes", "partial", "no", "inapplicable", "indeterminate", "error"),
          AllowedValueType.JsStringType,
          isRequired = true
        )
      )
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectDebit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectDebit"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("sortCodeSupportsDirectCredit"),
      ConstructAttribute.AsIs(Fetch(List("sortCodeSupportsDirectCredit"))),
      Some(AllowedValues(List("yes", "no", "error"), AllowedValueType.JsStringType, isRequired = true))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("accountName"),
      ConstructAttribute.AsIs(Fetch(List("accountName"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
    ),
    AttributeInstruction(
      DataRetrieve.Attribute("iban"),
      ConstructAttribute.AsIs(Fetch(List("iban"))),
      Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
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
          Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
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
          Some(AllowedValues(List("123 Main St"), AllowedValueType.JsStringType, isRequired = true))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("postcode"),
          ConstructAttribute.AsIs(Fetch(List("address", "postal_code"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
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
          Some(AllowedValues(List("test1", "test2"), AllowedValueType.JsStringType, isRequired = true))
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
          Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
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
          Some(AllowedValues(List("123 Main St", "London"), AllowedValueType.JsStringType, isRequired = true))
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
          Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"sic_codes": ["1234", "5678"]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "accept any number when AllowedValues has wildcard with JsNumberType and isRequired = true" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("amount"),
          ConstructAttribute.AsIs(Fetch(List("amount"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"amount": 123.45}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "reject missing required field with wildcard JsNumberType" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("amount"),
          ConstructAttribute.AsIs(Fetch(List("amount"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val invalidJson = Json.parse("""{}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("missing required field 'amount'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "reject string value when expecting any number with JsNumberType" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("amount"),
          ConstructAttribute.AsIs(Fetch(List("amount"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val invalidJson = Json.parse("""{"amount": "not a number"}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("field 'amount' must be a number")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "accept any string when AllowedValues has wildcard with JsStringType and isRequired = true" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("description"),
          ConstructAttribute.AsIs(Fetch(List("description"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsStringType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"description": "any value works here"}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "reject empty string for required wildcard JsStringType" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("description"),
          ConstructAttribute.AsIs(Fetch(List("description"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsStringType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val invalidJson = Json.parse("""{"description": ""}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("required field 'description' cannot be empty")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "accept any boolean when AllowedValues has wildcard with JsBooleanType and isRequired = true" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("isActive"),
          ConstructAttribute.AsIs(Fetch(List("isActive"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsBooleanType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJsonTrue = Json.parse("""{"isActive": true}""")
      validateDataRetrieveResponse(validJsonTrue, dataRetrieve) shouldBe ValidationSuccess

      val validJsonFalse = Json.parse("""{"isActive": false}""")
      validateDataRetrieveResponse(validJsonFalse, dataRetrieve) shouldBe ValidationSuccess
    }

    "reject string value when expecting boolean with JsBooleanType" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("isActive"),
          ConstructAttribute.AsIs(Fetch(List("isActive"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsBooleanType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val invalidJson = Json.parse("""{"isActive": "true"}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("field 'isActive' must be a boolean")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "allow empty optional field with wildcard and specific type" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("optionalNumber"),
          ConstructAttribute.AsIs(Fetch(List("optionalNumber"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsNumberType, isRequired = false))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "validate specific number values when not using wildcard" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("status"),
          ConstructAttribute.AsIs(Fetch(List("status"))),
          Some(AllowedValues(List("200", "201", "204"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"status": 200}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess

      val invalidJson = Json.parse("""{"status": 404}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("unexpected value for 'status': '404'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "validate specific boolean values when not using wildcard" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("isEnabled"),
          ConstructAttribute.AsIs(Fetch(List("isEnabled"))),
          Some(AllowedValues(List("true"), AllowedValueType.JsBooleanType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"isEnabled": true}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess

      val invalidJson = Json.parse("""{"isEnabled": false}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors should contain("unexpected value for 'isEnabled': 'false'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "validate string array with wildcard allowedValues" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("tags"),
          ConstructAttribute.AsIs(Fetch(List("tags"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsStringType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"tags": ["alpha", "beta", "gamma"]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "validate string array with specific allowed values" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("roles"),
          ConstructAttribute.AsIs(Fetch(List("roles"))),
          Some(AllowedValues(List("admin", "user", "guest"), AllowedValueType.JsStringType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"roles": ["admin", "user"]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess

      val invalidJson = Json.parse("""{"roles": ["admin", "superuser"]}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors.head should include("array element at index 1")
          errors.head should include("superuser")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "validate number array with wildcard allowedValues" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("scores"),
          ConstructAttribute.AsIs(Fetch(List("scores"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"scores": [100, 95.5, 87]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "validate number array with specific allowed values" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("statusCodes"),
          ConstructAttribute.AsIs(Fetch(List("statusCodes"))),
          Some(AllowedValues(List("200", "201", "204"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"statusCodes": [200, 201]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess

      val invalidJson = Json.parse("""{"statusCodes": [200, 404]}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors.head should include("array element at index 1")
          errors.head should include("404")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "validate boolean array with wildcard allowedValues" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("flags"),
          ConstructAttribute.AsIs(Fetch(List("flags"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsBooleanType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"flags": [true, false, true]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "validate boolean array with specific allowed values" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("settings"),
          ConstructAttribute.AsIs(Fetch(List("settings"))),
          Some(AllowedValues(List("true"), AllowedValueType.JsBooleanType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"settings": [true, true]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess

      val invalidJson = Json.parse("""{"settings": [true, false]}""")
      validateDataRetrieveResponse(invalidJson, dataRetrieve) match {
        case ValidationFailure(errors) =>
          errors.head should include("array element at index 1")
          errors.head should include("false")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }

    "reject array with wrong type elements" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("codes"),
          ConstructAttribute.AsIs(Fetch(List("codes"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsNumberType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val invalidJson = Json.parse("""{"codes": [100, "not a number", 200]}""")
      val result = validateDataRetrieveResponse(invalidJson, dataRetrieve)
      result match {
        case ValidationFailure(errors) =>
          errors.head should include("array element at index 1")
          errors.head should include("must be a number")
        case ValidationSuccess => fail(s"Expected validation failure but got: $result")
      }
    }

    "validate empty array" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("items"),
          ConstructAttribute.AsIs(Fetch(List("items"))),
          Some(AllowedValues(List("*"), AllowedValueType.JsStringType, isRequired = false))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"items": []}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }

    "validate array with AnyValueType accepts mixed primitive types" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("mixedData"),
          ConstructAttribute.AsIs(Fetch(List("mixedData"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = true))
        )
      )
      val dataRetrieve = createDataRetrieve("test", instructions)

      val validJson = Json.parse("""{"mixedData": ["text", 123, true]}""")
      validateDataRetrieveResponse(validJson, dataRetrieve) shouldBe ValidationSuccess
    }
  }

  "FromArray validation" should {
    "validate array responses successfully" in {
      val instructions = List(
        AttributeInstruction(
          DataRetrieve.Attribute("name"),
          ConstructAttribute.AsIs(Fetch(List("name"))),
          Some(AllowedValues(List("*"), AllowedValueType.AnyValueType, isRequired = false))
        ),
        AttributeInstruction(
          DataRetrieve.Attribute("role"),
          ConstructAttribute.AsIs(Fetch(List("role"))),
          Some(
            AllowedValues(List("director", "secretary", "llp-member"), AllowedValueType.JsStringType, isRequired = true)
          )
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
          Some(AllowedValues(List("director", "secretary"), AllowedValueType.JsStringType, isRequired = true))
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
