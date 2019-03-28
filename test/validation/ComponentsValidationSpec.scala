package validation
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate.{FormComponent, FormComponentId, TelephoneNumber, Text, Value}
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.{FormComponentGen, FormatExprGen}
import uk.gov.hmrc.gform.validation.ComponentsValidator

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
