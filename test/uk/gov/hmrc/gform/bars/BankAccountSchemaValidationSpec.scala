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

package uk.gov.hmrc.gform.bars

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import uk.gov.hmrc.gform.bars.BankAccountSchemaValidation._

class BankAccountSchemaValidationSpec extends AnyWordSpec with Matchers {

  "validateBankDetailsResponse" should {
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

      validateBankDetailsResponse(validJson) shouldBe ValidationSuccess
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

      validateBankDetailsResponse(invalidJson) match {
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

      validateBankDetailsResponse(invalidJson) match {
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

      validateBankDetailsResponse(invalidJson) match {
        case ValidationFailure(errors) =>
          errors should contain("missing required field 'accountNumberIsWellFormatted'")
        case ValidationSuccess => fail("Expected validation failure")
      }
    }
  }

  "validateBusinessBankAccountExistenceResponse" should {
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

      validateBusinessBankAccountExistenceResponse(validJson) shouldBe ValidationSuccess
    }
  }

  "validatePersonalBankAccountExistenceResponse" should {
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
          "sortCodeBankName": "Test Bank"
        }
      """)

      validatePersonalBankAccountExistenceResponse(validJson) shouldBe ValidationSuccess
    }
  }
}
