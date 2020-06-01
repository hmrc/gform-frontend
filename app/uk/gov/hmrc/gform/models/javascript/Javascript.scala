/*
 * Copyright 2020 HM Revenue & Customs
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

package uk.gov.hmrc.gform.models.javascript

import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.models.helpers.FormComponentHelper.extractMaxFractionalDigits
import uk.gov.hmrc.gform.ops._

private case class JsFunction(name: String) extends AnyVal {
  override def toString = name
}

case class RepeatFormComponentIds(op: FormComponentId => List[FormComponentId]) extends AnyVal

object Javascript {

  def fieldJavascript(
    jsFormComponentModels: List[JsFormComponentModel],
    allFields: List[FormComponent],
    repeatFormComponentIds: RepeatFormComponentIds,
    dependencies: Dependencies): String = {

    val (jsRevealingChoiceModels, sectionFields) =
      jsFormComponentModels.foldLeft((List.empty[JsRevealingChoiceModel], List.empty[FormComponentWithCtx])) {
        case ((rcModel, fcWithCtx), next) =>
          next match {
            case x: JsRevealingChoiceModel => (x :: rcModel, fcWithCtx)
            case x: JsFormComponentWithCtx => (rcModel, x.fcWithCtx :: fcWithCtx)
          }
      }

    val sectionFieldIds = sectionFields.map(_.id).toSet
    val dynamicFcIds = sectionFieldIds ++ jsRevealingChoiceModels.map(_.fc.id).toSet

    def isDynamic(expr: Expr): Boolean = expr match {
      case f @ FormCtx(_)              => dynamicFcIds.contains(f.toFieldId)
      case Sum(f)                      => isDynamic(f)
      case Add(field1, field2)         => isDynamic(field1) || isDynamic(field2)
      case Subtraction(field1, field2) => isDynamic(field1) || isDynamic(field2)
      case Multiply(field1, field2)    => isDynamic(field1) || isDynamic(field2)
      case otherwise                   => false
    }

    val previousSectionIds: List[FormComponentId] = dependencies.all.filterNot(sectionFieldIds.contains)

    val isHiddenScripts = jsRevealingChoiceModels.map {
      case JsRevealingChoiceModel(_, fc) =>
        val id = fc.id.value
        s"""|var isHidden$id = function () {
            |  return document.getElementById("$id").parentNode.parentNode.parentNode.classList.contains("govuk-checkboxes__conditional--hidden");
            |};""".stripMargin
    }
    val isHiddenScripts2 = (previousSectionIds ++ sectionFields.map(_.id)).map { fcId =>
      s"""|var isHidden${fcId.value} = function () {
          |  return false;
          |};""".stripMargin
    }

    val radiosAndCheckboxes = jsRevealingChoiceModels.map(_.fcId)

    val mkEvents: JsFunction => List[String] = functionName =>
      radiosAndCheckboxes.distinct.map { fcId =>
        s"""|document.getElementsByName("${fcId.value}").forEach(function(element, index) {
            |  element.addEventListener("change",$functionName);
            |});
            |""".stripMargin
    }

    val fieldIdWithExpr: List[(FormComponentWithCtx, Expr)] =
      sectionFields.collect {
        case formComponent @ HasExprCtx(SingleExpr(expr)) if isDynamic(expr) =>
          (formComponent, expr)
      }

    fieldIdWithExpr
      .map {
        case (formComponentWithCtx, expr) =>
          toJavascriptFn(
            formComponentWithCtx,
            expr,
            repeatFormComponentIds,
            dependencies.toLookup,
            mkEvents,
            jsRevealingChoiceModels)
      }
      .mkString("\n") +
      isHiddenScripts2.mkString("\n") +
      isHiddenScripts.mkString("\n") +
      """|function getValue(elementId, identity, isHidden) {
         |   var el = document.getElementById(elementId);
         |   var isVisible = !isHidden();
         |   if (el && isVisible) {
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
         |    return value.replace(",", "");
         |  }
         |};
         |
         |function add(a, b) {
         |  return BigNumber(a).plus(BigNumber(b));
         |};
         |
         |function subtract(a, b) {
         |  return BigNumber(a).minus(BigNumber(b));
         |};
         |
         |function multiply(a, b) {
         |  return BigNumber(a).times(BigNumber(b));
         |};
         |function displaySterling(result, precision, rounding) {
         |  var r = BigNumber(result).toFormat(precision, rounding);
         |  return result < 0 ? r.replace("-", "-£") : '£' + r;
         |};
         |""".stripMargin
  }

  private def toJavascriptFn(
    field: FormComponentWithCtx,
    expr: Expr,
    repeatFormComponentIds: RepeatFormComponentIds,
    dependenciesLookup: Map[FormComponentId, List[FormComponentId]],
    radioAndCheckboxes: JsFunction => List[String],
    jsRevealingChoiceModel: List[JsRevealingChoiceModel]
  ): String = {

    def getRoundingMode(fc: FormComponent) =
      fc.`type` match {
        case Text(Number(_, _, rm, _), _, _, _)          => Some(rm)
        case TextArea(Number(_, _, rm, _), _, _)         => Some(rm)
        case Text(PositiveNumber(_, _, rm, _), _, _, _)  => Some(rm)
        case TextArea(PositiveNumber(_, _, rm, _), _, _) => Some(rm)
        case Text(s: Sterling, _, _, _)                  => Some(s.roundingMode)
        case TextArea(s: Sterling, _, _)                 => Some(s.roundingMode)
        case _                                           => None
      }

    val roundingMode = field match {
      case FormComponentWithGroup(fc, _) => getRoundingMode(fc)
      case FormComponentSimple(fc)       => getRoundingMode(fc)
    }

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
        case FormCtx(id)       => s"""getValue("$id", $opIdentity, isHidden$id)"""
        case Constant(amount)  => amount
        case Add(a, b)         => compute("add", a, b, additionIdentity)
        case Subtraction(a, b) => compute("subtract", a, b, additionIdentity)
        case Multiply(a, b)    => compute("multiply", a, b, additionIdentity)
        case Sum(FormCtx(id))  => sum(id)
        case otherwise         => ""
      }
    }

    def listeners(functionName: JsFunction) = {

      val components = dependenciesLookup.get(field.id).getOrElse(Nil) ++ jsRevealingChoiceModel.map(_.fc.id)

      val componentEls = components.map { fcId =>
        val id = fcId.value
        s"""|var element$id = document.getElementById("$id");
            |if (element$id) {
            |  element$id.addEventListener("change",$functionName);
            |  element$id.addEventListener("keyup",$functionName);
            |}""".stripMargin
      }

      s"""|window.addEventListener("load", $functionName);
          |${componentEls.mkString("\n")}
          |""".stripMargin
    }

    val elementRoundingMode = roundingMode match {
      case Some(RoundingMode.Up)       => "ROUND_UP"
      case Some(RoundingMode.Down)     => "ROUND_DOWN"
      case Some(RoundingMode.Ceiling)  => "ROUND_CEIL"
      case Some(RoundingMode.Floor)    => "ROUND_FLOOR"
      case Some(RoundingMode.HalfUp)   => "ROUND_HALF_UP"
      case Some(RoundingMode.HalfDown) => "ROUND_HALF_DOWN"
      case Some(RoundingMode.HalfEven) => "ROUND_HALF_EVEN"
      case _                           => "ROUND_DOWN"
    }
    val elementId = field.id
    val functionName = JsFunction("compute" + elementId)

    // format: off
    s"""|function $functionName() {
     |  var numberOfDecimalPlaces = ${roundToCtx(field)};
     |  var roundingMode = BigNumber.$elementRoundingMode;
     |  var result = BigNumber(${computeExpr(expr, additionIdentity)}).decimalPlaces(numberOfDecimalPlaces, roundingMode);
     |  var element = document.getElementById("$elementId")
     |  if(element) {
     |    element.value = result;
     |  }
     |  var total = document.getElementById("$elementId-total");
     |  if(total) {
     |    total.innerHTML = ${if (field.isSterling) "displaySterling(result, numberOfDecimalPlaces, roundingMode)" else "result"};
     |  }
     |}
     |${listeners(functionName)}
     |${radioAndCheckboxes(functionName).mkString("\n")}
     |""".stripMargin
    // format: on

  }

  def roundToCtx(fc: FormComponentWithCtx): Int = fc match {
    case FormComponentWithGroup(fc, _) => extractMaxFractionalDigits(fc).maxDigits
    case FormComponentSimple(fc)       => extractMaxFractionalDigits(fc).maxDigits
  }

}
