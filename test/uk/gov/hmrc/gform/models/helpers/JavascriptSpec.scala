package uk.gov.hmrc.gform.models.helpers

import cats.implicits._
import uk.gov.hmrc.gform.Spec
import uk.gov.hmrc.gform.sharedmodel.formtemplate._

import scala.concurrent.Future

class JavascriptSpec extends Spec {

  private val c = Constant("")

  def formComponent(id: String, value : Expr = c) = FormComponent(
    FormComponentId(id),
    Text(AnyText, value),
    "",
    None,
    None,
    None,
    false,
    true,
    true,
    true,
    false,
    None)

  private def fieldJavascript(field: FormComponent) = {
    val fields = List(formComponent("thisSection"), field)
    Javascript.fieldJavascript(
      sectionFields = fields,
      allFields = formComponent("otherSection") :: fields,
      groupList = List[List[List[FormComponent]]]().pure[Future])
  }

  "if calculation references only a constant" should "not generate Javascript for the static calculation" in{
    val result = fieldJavascript(formComponent("staticExpr"))
    result.futureValue should not include("addstaticExpr")
  }

  "if calculation references only a field in this section" should "not generate Javascript for the static calculation" in{
    val result = fieldJavascript(formComponent("staticExpr", FormCtx("thisSection")))
    result.futureValue should not include("addstaticExpr")
  }

  "if calculation references only a group in this section" should "generate Javascript for the dynamic calculation" in{
    val result = fieldJavascript(formComponent("dynamicExpr", Sum(FormCtx("thisSection"))))
    result.futureValue should include("sumthisSection")
  }

  "if calculation adds a field in this section" should "generate Javascript for the dynamic calculation" in{
    val result = fieldJavascript(formComponent("dynamicExpr", Add(FormCtx("thisSection"), c)))
    result.futureValue should include("adddynamicExpr")
  }

  "if calculation deep inside uses a field in this section" should "generate Javascript for the dynamic calculation" in{
    val result = fieldJavascript(formComponent("dynamicExpr",
      Add(c, Add(
        Subtraction(c, Subtraction(
          Multiply( c, Multiply(FormCtx("thisSection"), c)),
          c)),
        c))
    ))
    result.futureValue should include("adddynamicExpr")
  }

}
