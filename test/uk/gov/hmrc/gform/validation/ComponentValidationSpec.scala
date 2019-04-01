package uk.gov.hmrc.gform.validation
import org.scalatest.Matchers
import uk.gov.hmrc.gform.{ GraphSpec, Spec }
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.FormComponentGen
import uk.gov.hmrc.gform.sharedmodel.formtemplate.generators.ComponentTypeGen._

class ComponentValidationSpec extends Spec with Matchers with GraphSpec {

  "validateChoice" should "be invalid when a form component is mandatory" in {
    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("checkbox1") -> Seq("bing"),
        FormComponentId("checkbox2") -> Seq("bong")
      ))

    forAll(FormComponentGen.formComponentGen(), choiceGen) { (formComponent, choice) =>
      {
        val choiceComponent = formComponent.copy(`type` = choice)
        whenever(isFormComponentMandatory(formComponent, true)) {
          val res = ComponentValidator.validateChoice(choiceComponent)(data)
          res.isInvalid shouldBe true
        }
      }
    }
  }

  "validateChoice" should "be valid" in {
    val data = mkFormDataRecalculated(
      Map(
        FormComponentId("checkbox1") -> Seq("bing"),
        FormComponentId("checkbox2") -> Seq("bong")
      ))

    forAll(FormComponentGen.formComponentGen(), choiceGen) { (formComponent, choice) =>
      {
        val choiceComponent = formComponent.copy(`type` = choice)
        whenever(isFormComponentMandatory(formComponent, false)) {
          val res = ComponentValidator.validateChoice(choiceComponent)(data)
          res.isValid shouldBe true
        }
      }
    }
  }

  private def isFormComponentMandatory(fieldValue: FormComponent, yesOrNo: Boolean) = fieldValue.mandatory == yesOrNo

}
