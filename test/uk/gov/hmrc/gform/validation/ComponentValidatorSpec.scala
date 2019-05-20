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
import org.scalatest.Matchers
import play.api.i18n.Messages
import uk.gov.hmrc.gform.sharedmodel.{ LangADT, LocalisedString }
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{ FormComponentGen, FormatExprGen }
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.ComponentTypeGen._

class ComponentValidatorSpec(implicit messages: Messages, l: LangADT) extends Spec with Matchers with GraphSpec {

  "validateChoice" should "be invalid when form component is mandatory + the data associated with the id is empty" in {
    forAll(FormComponentGen.formComponentGen(), choiceGen) { (formComponent, choice) =>
      {
        val choiceComponent = convertFormComponent(formComponent, choice)
        whenever(isFormComponentMandatory(formComponent, true)) {
          val res = ComponentValidator.validateChoice(choiceComponent)(choiceData(true, formComponent))
          res.isInvalid shouldBe true
        }
      }
    }
  }

  it should "be valid when the form component is not mandatory and the data associated with the id is empty" in {
    forAll(FormComponentGen.formComponentGen(), choiceGen) { (formComponent, choice) =>
      {
        val choiceComponent = convertFormComponent(formComponent, choice)
        whenever(isFormComponentMandatory(formComponent, false)) {
          val res = ComponentValidator.validateChoice(choiceComponent)(choiceData(false, formComponent))
          res.isValid shouldBe true
        }
      }
    }
  }

  it should "be valid when the form component is mandatory and the data associated with the id is empty" in {
    forAll(FormComponentGen.formComponentGen(), choiceGen) { (formComponent, choice) =>
      {
        val choiceComponent = convertFormComponent(formComponent, choice)
        whenever(isFormComponentMandatory(formComponent, true)) {
          val res = ComponentValidator.validateChoice(choiceComponent)(choiceData(false, formComponent))
          res.isValid shouldBe true
        }
      }
    }
  }

  private val numberWithPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.International)
  private val numberWithoutPlus = FormatExprGen.telephoneNumberGen(FormatExprGen.UK)
  private val telephoneConstraint = Text(TelephoneNumber, Value)
  private val formComponent = FormComponent(
    FormComponentId("formComponent"),
    telephoneConstraint,
    LocalisedString(Map(LangADT.En -> "formComponentLabel")),
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
      val result = ComponentValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }

  it should "return invalid when a string contains a '$' symbol" in {
    forAll(numberWithPlus) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(
        formComponent,
        phoneNumber + "$"
      )
      result.isInvalid shouldBe true
    }
  }

  it should "return valid when character count is between 7-25" in {
    forAll(numberWithoutPlus) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isValid shouldBe true
    }
  }

  it should "return invalid when character count is less than 7" in {
    val invalidNumber = numberWithoutPlus.map(string => string.substring(0, 6))
    forAll(invalidNumber) { phoneNumber =>
      val result = ComponentValidator.validatePhoneNumber(formComponent, phoneNumber)
      result.isInvalid shouldBe true
    }
  }

  val shortTextComponent = FormComponent(
    FormComponentId("formComponent"),
    Text(ShortText(3, 5), Value),
    LocalisedString(Map(LangADT.En -> "formComponentLabel")),
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

  private val convertFormComponent: (FormComponent, Choice) => FormComponent = (formComponent, choice) =>
    formComponent.copy(`type` = choice)

  private def choiceData(isEmpty: Boolean, formComponent: FormComponent) =
    if (isEmpty) mkFormDataRecalculated(Map(FormComponentId(formComponent.id.value) -> Seq()))
    else mkFormDataRecalculated(Map(FormComponentId(formComponent.id.value)         -> Seq("bing")))

  private def isFormComponentMandatory(fieldValue: FormComponent, isMandatory: Boolean) =
    fieldValue.mandatory == isMandatory
}
