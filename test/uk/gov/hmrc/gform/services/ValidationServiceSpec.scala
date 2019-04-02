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
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormatExprGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

class ValidationServiceSpec extends Spec {

  val numberWithPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.International)
  val numberWithoutPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.UK)

  val telephoneConstraint = Text(TelephoneNumber, Value)
  val telephoneNumber = TelephoneNumber
  val formComponent = FormComponent(
    FormComponentId("formComponent"),
    telephoneConstraint,
    "formComponentLabel",
    None,
    None,
    None,
    true,
    true,
    false,
    true,
    false,
    None)

  "validatePhoneNumber" should "return valid when character count is less than 4 and contains a special character" in {
    val lessThan4WithPlus = numberWithPlus.map(string => string.substring(0, 4))
    forAll(lessThan4WithPlus) { phoneNumber =>
      val result = ComponentsValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }

  it should "return invalid when a string contains a '$' symbol" in {
    forAll(numberWithPlus) { phoneNumber =>
      val result = ComponentsValidator.validatePhoneNumber(
        formComponent,
        phoneNumber + "$"
      )
      result.isInvalid shouldBe true
    }
  }

  it should "return valid when character count is between 4-25" in {
    forAll(numberWithoutPlus) { phoneNumber =>
      val result = ComponentsValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }

  it should "return invalid when character count is less than 4" in {
    val invalidNumber = numberWithoutPlus.map(string => string.substring(0, 3))
    forAll(invalidNumber) { phoneNumber =>
      val result = ComponentsValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isInvalid shouldBe true
    }
  }

  "it" should "return valid when character count is less than 4 and contains a special character" in {
    val lessThan4WithPlus = numberWithPlus.map(string => string.substring(0, 4))
    forAll(lessThan4WithPlus) { phoneNumber =>
      val result = ComponentsValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }
}
