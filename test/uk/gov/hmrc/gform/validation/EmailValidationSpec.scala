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

package uk.gov.hmrc.gform.validation

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.validation.generators.EmailAddressGen

class EmailValidationSpec extends Spec with EmailAddressGen {
  "Validate an email address" should "return true for a valid email address" in {
    forAll(validEmailAddresses()) { address =>
      EmailAddress.isValid(address) shouldBe true
    }
  }

  it should "return false for invalid email addresses" in {
    forAll(validEmailAddresses()) { address =>
      EmailAddress.isValid("§" + address) shouldBe false
    }
  }
}
