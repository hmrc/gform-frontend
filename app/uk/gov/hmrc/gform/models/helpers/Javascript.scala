/*
 * Copyright 2018 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models.helpers

import uk.gov.hmrc.gform.models.Dependecies
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import FormComponentHelper.roundTo

case class JsFunction(name: String) extends AnyVal {
  override def toString = name
}

case class RepeatFormComponentIds(op: FormComponentId => List[FormComponentId]) extends AnyVal

object Javascript {

  def fieldJavascript(
    sectionFields: List[FormComponent],
    allFields: List[FormComponent],
    repeatFormComponentIds: RepeatFormComponentIds,
    dependencies: Dependecies): String = {

    val sectionFieldIds = sectionFields.map(_.id).toSet

    def isDynamic(expr: Expr): Boolean = expr match {
      case f @ FormCtx(_)              => sectionFieldIds.contains(f.toFieldId)
      case Sum(f)                      => isDynamic(f)
      case Add(field1, field2)         => isDynamic(field1) || isDynamic(field2)
      case Subtraction(field1, field2) => isDynamic(field1) || isDynamic(field2)
      case Multiply(field1, field2)    => isDynamic(field1) || isDynamic(field2)
      case otherwise                   => false
    }

    val fieldIdWithExpr: List[(FormComponent, Expr)] =
      sectionFields.collect {
        case formComponent @ HasExpr(SingleExpr(expr)) if isDynamic(expr) => (formComponent, expr)
      }

    fieldIdWithExpr
      .map(x => toJavascriptFn(x._1, x._2, repeatFormComponentIds, dependencies.toLookup))
      .mkString("\n") +
      """|function getValue(elementId, identity) {
         |   var el = document.getElementById(elementId);
         |   if (el) {
         |     return getNumber(el.value.replace(/[£,]/g,''), identity);
         |   } else {
         |     return identity;
         |   };
         |};
         |
         |function getNumber(value, identity) {
         |  if (value == ""){
         |    return identity;
         |  } else {
         |   return value.replace(",", "");
         |  }
         |};
         |
         |function add(a, b) {
         |  return new Big(a).add(new Big(b));
         |};
         |
         |function subtract(a, b) {
         |  return new Big(a).minus(new Big(b));
         |};
         |
         |function multiply(a, b) {
         |  return new Big(a).times(new Big(b));
         |};
         |""".stripMargin
  }

  private def toJavascriptFn(
    field: FormComponent,
    expr: Expr,
    repeatFormComponentIds: RepeatFormComponentIds,
    dependenciesLookup: Map[FormComponentId, List[FormComponentId]]): String = {

    import Expr._

    def computeExpr(expr: Expr, opIdentity: Int): String = {

      def sum(id: String) = {
        val groupFcIds: List[FormComponentId] = repeatFormComponentIds.op(FormComponentId(id))
        val sumExpr = groupFcIds.map(x => FormCtx(x.value)).foldLeft(additionIdentityExpr)(Add)
        computeExpr(sumExpr, additionIdentity)
      }

      def compute(operation: String, left: Expr, right: Expr, id: Int) =
        s"$operation(${computeExpr(left, id)}, ${computeExpr(right, id)})"

      expr match {
        case FormCtx(id)       => s"""getValue("$id", $opIdentity)"""
        case Constant(amount)  => amount
        case Add(a, b)         => compute("add", a, b, additionIdentity)
        case Subtraction(a, b) => compute("subtract", a, b, additionIdentity)
        case Multiply(a, b)    => compute("multiply", a, b, multiplicationIdentity)
        case Sum(FormCtx(id))  => sum(id)
        case otherwise         => ""
      }
    }

    def listeners(functionName: JsFunction) = {
      val windowEl = s"""window.addEventListener("load", $functionName);"""

      val componentEls =
        dependenciesLookup.get(field.id) match {
          case None => ""
          case Some(deps) =>
            deps
              .map { id =>
                s"""|var element$id = document.getElementById("$id")
                    |if (element$id) {
                    |  element$id.addEventListener("change",$functionName);
                    |  element$id.addEventListener("keyup",$functionName);
                    |}
                    |""".stripMargin
              }
              .mkString("\n")
        }
      componentEls + windowEl
    }

    val elementId = field.id
    val functionName = JsFunction("compute" + elementId)

    s"""|function $functionName() {
        |  var result = ${computeExpr(expr, additionIdentity)}.toFixed(${roundTo(field)}, 0);
        |  document.getElementById("$elementId").value = result;
        |  var total = document.getElementById("$elementId-total");
        |  if(total) total.innerHTML = result;
        |}
        |${listeners(functionName)}
        |""".stripMargin

  }

  def collapsingGroupJavascript(fieldId: FormComponentId, group: Group) =
    s"""|function removeOnClick$fieldId() {
        |${group.fields.map(fv => s"""  document.getElementById("${fv.id}").value = '';""").mkString("\n")}
        |}
        |""".stripMargin
}
