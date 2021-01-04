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

package uk.gov.hmrc.gform.validation

import org.scalatest.Matchers
import play.api.i18n.{ Lang, Messages }
import play.api.test.Helpers
import uk.gov.hmrc.gform.Helpers.toSmartString
import uk.gov.hmrc.gform.sharedmodel.LangADT
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import org.scalatestplus.mockito._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormatExprGen

class ComponentValidatorSpec extends Spec with Matchers with GraphSpec with MockitoSugar {

  implicit val langADT: LangADT = LangADT.En
  val lang = Lang(langADT.langADTToString)
  val messagesApi = Helpers.stubMessagesApi(Map("en" -> Map("helper.order" -> "{0} {1}")))
  implicit val messages: Messages = Helpers.stubMessages(messagesApi)

  private val numberWithPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.International)
  private val numberWithoutPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.UK)
  private val telephoneConstraint = Text(TelephoneNumber, Value)
  private val testFormComponent = FormComponent(
    FormComponentId("testFormComponent"),
    telephoneConstraint,
    toSmartString("formComponentLabel"),
    None,
    None,
    None,
    true,
    true,
    false,
    true,
    false,
    None
  )

  "validatePhoneNumber" should "return valid when character count is less than 7 and contains a special character" in {
    val lessThan7WithPlus = numberWithPlus.map(string => string.substring(0, 7))
    forAll(lessThan7WithPlus) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(testFormComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }

  it should "return invalid when a string contains a '$' symbol" in {
    forAll(numberWithPlus) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(
        testFormComponent,
        phoneNumber + "$"
      )
      result.isInvalid shouldBe true
    }
  }

  it should "return valid when character count is between 7-25" in {
    forAll(numberWithoutPlus) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(testFormComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }

  it should "return invalid when character count is less than 7" in {
    val invalidNumber = numberWithoutPlus.map(string => string.substring(0, 6))
    forAll(invalidNumber) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(testFormComponent, phoneNumber)
      result.isInvalid shouldBe true
    }
  }

  private val shortTextComponent = FormComponent(
    FormComponentId("testFormComponent"),
    Text(ShortText(3, 5), Value),
    toSmartString("formComponentLabel"),
    None,
    None,
    None,
    true,
    true,
    false,
    true,
    false,
    None
  )

  "validateShortText" should "return invalid if character count is too big" in {
    val shortTextTooLong = "abcdefghij"
    val result = ComponentValidator.shortTextValidation(shortTextComponent, shortTextTooLong, 3, 5)
    result.isInvalid shouldBe true
  }

  it should "return invalid if character count is too small" in {
    val shortTextTooShort = "a"
    val result = ComponentValidator.shortTextValidation(shortTextComponent, shortTextTooShort, 3, 5)
    result.isInvalid shouldBe true
  }

  it should "return valid if the character count is within range" in {
    val shortTextWithinRange = "abcd"
    val result = ComponentValidator.shortTextValidation(shortTextComponent, shortTextWithinRange, 3, 5)
    result.isValid shouldBe true
  }

  it should "return invalid if incorrect character are entered" in {
    val shortTextIncorrectChars = "a[]*"
    val result =
      ComponentValidator.shortTextValidation(shortTextComponent, shortTextIncorrectChars, 3, 5)
    result.isInvalid shouldBe true
  }

  private val textComponent = FormComponent(
    FormComponentId("testFormComponent"),
    TextArea(ShortText.default, Value),
    toSmartString("formComponentLabel"),
    None,
    None,
    None,
    true,
    true,
    false,
    true,
    false,
    None
  )

  "textValidationWithConstraints" should "return valid if there are all valid characters" in {
    val textWithAllValidCharacters =
      "This would make my 80 percent of your average trading profits for 3 months £730.60."
    val result = ComponentValidator.textValidationWithConstraints(textComponent, textWithAllValidCharacters, 3, 100)
    result.isValid shouldBe true
  }

  it should "return invalid if there are some invalid characters" in {
    val textWithInvalidCharacters =
      "This would ^ make my 80 percent of your ^ average | trading profits for  | 3 months £730.60."

    val result = ComponentValidator.textValidationWithConstraints(textComponent, textWithInvalidCharacters, 3, 100)
    result.isInvalid shouldBe true
  }
}
