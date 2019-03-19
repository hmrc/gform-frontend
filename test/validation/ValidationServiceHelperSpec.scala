package validation

import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.validation.ValidationServiceHelper._

class ValidationServiceHelperSpec extends Spec {

  "when given a data and a List of field components that is corespondes to the refrence in date" should "return the form component from the List" in
  {
    val formComponentId = FormComponentId("1")
    val dateConstraints = DateConstraints(List(DateConstraint(After,DateField(formComponentId),OffsetDate(1))))
    val date = Date(dateConstraints,Offset(1),None)
    val formComponent = FormComponent(formComponentId, date,"",None,None,None,false,false,false,false,false,None,None)
   getCompanionFieldComponent(date, List(formComponent)) shouldBe(Some(formComponent))
  }
  val beforeFormComponentId = FormComponentId("1")
  val afterFormComponentId = FormComponentId("2")
  val beforeDateConstraints = DateConstraints(List(DateConstraint(After,DateField(afterFormComponentId),OffsetDate(1))))
  val afterDateConstraints = DateConstraints(List(DateConstraint(After,DateField(beforeFormComponentId),OffsetDate(1))))
  val beforeDate = Date(beforeDateConstraints,Offset(1),None)
  val afterDate = Date(afterDateConstraints,Offset(1),None)
  val beforeFormComponent = FormComponent(beforeFormComponentId, beforeDate,"",None,None,None,false,false,false,false,false,None,None)
  val afterFormComponent = FormComponent(afterFormComponentId, afterDate,"",None,None,None,false,false,false,false,false,None,None)
  "when given a before form component" should "return refrence of after form component" in {
      getCompanionFieldComponent(beforeDate,List(beforeFormComponent, afterFormComponent) ) shouldBe(Some(afterFormComponent))
    }
  "when given a after form component" should "return refrence of before form component" in {
    getCompanionFieldComponent(afterDate, List(beforeFormComponent, afterFormComponent)) shouldBe(Some(beforeFormComponent))
  }
  "when given a date value and an List that doesn't contain a refrence to any of the Ids" should "return None" in {
    val formComponentId = FormComponentId("3")
    getCompanionFieldComponent(beforeDate, List(FormComponent(formComponentId, beforeDate,"",None,None,None,false,false,false,false,false,None,None))) shouldBe(None)
  }
}