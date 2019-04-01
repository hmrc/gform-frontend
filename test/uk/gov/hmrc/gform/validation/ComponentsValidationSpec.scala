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
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{ FormComponent, FormComponentId, TelephoneNumber, Text, Value }

class ComponentsValidationSpec extends Spec {

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

  "validateHelper" should "return invalid when character count is less than 4, not including the + symbol" in {
    val lessThan4WithPlus = numberWithPlus.map(string => string.substring(0, 4))
    forAll(lessThan4WithPlus) { phoneNumber =>
      {
        val result = ComponentsValidator.validatorHelper(
          phoneNumber.replace("+", "").length,
          formComponent,
          phoneNumber,
          telephoneNumber.minimumLength,
          telephoneNumber.maximumLength
        )
        result.isInvalid shouldBe true
      }
    }
  }

  it should "return invalid when character count is greater than 25, not including the + symbol" in {
    val greaterThan25WithPlus = "+123456768901234567889012345"

    val result = ComponentsValidator.validatorHelper(
      greaterThan25WithPlus.replace("+", "").length,
      formComponent,
      greaterThan25WithPlus,
      telephoneNumber.minimumLength,
      telephoneNumber.maximumLength)

    result.isInvalid shouldBe true
  }

  it should "return valid when character count is between 0-25 and starts with a number" in {
    forAll(numberWithoutPlus) { phoneNumber =>
      {
        val result = ComponentsValidator.validatorHelper(
          phoneNumber.length,
          formComponent,
          phoneNumber,
          telephoneNumber.minimumLength,
          telephoneNumber.maximumLength)
        result.isValid shouldBe true
      }
    }

  }
}
