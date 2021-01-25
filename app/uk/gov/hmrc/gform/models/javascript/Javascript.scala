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

package uk.gov.hmrc.gform.models.javascript

import cats.syntax.eq._
import uk.gov.hmrc.gform.models._
import uk.gov.hmrc.gform.models.helpers.FormComponentHelper.extractMaxFractionalDigits
import uk.gov.hmrc.gform.models.ids.{ BaseComponentId, ModelComponentId }
import uk.gov.hmrc.gform.models.optics.DataOrigin
import uk.gov.hmrc.gform.sharedmodel.form.FormModelOptics
import uk.gov.hmrc.gform.sharedmodel.formtemplate._
import uk.gov.hmrc.gform.ops._

private case class JsFunction(name: String) extends AnyVal {
  override def toString = name
}

object Javascript {

  def fieldJavascript(
    pageModel: PageModel[DataExpanded],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): String = {

    val fcWithSuccessors: List[(FormComponent, Set[ModelComponentId])] = pageModel.allFormComponents
      .flatMap { formComponent =>
        val maybeExpr: Option[Expr] = HasValueExpr.unapply(formComponent)
        maybeExpr
          .map { expr =>
            val leafs: Set[BaseComponentId] = expr.leafs.collect {
              case FormCtx(fcId) => fcId.baseComponentId
            }.toSet
            val modelComponentIds =
              pageModel.allModelComponentIds.filter(modelComponentId => leafs(modelComponentId.baseComponentId))

            formComponent -> modelComponentIds
          }
      }
      .filter { case (fc, set) => set.nonEmpty } // Ignore if there are no successors (for example ${form.submissionReference})

    val successorLookup
      : Map[FormComponentId, Set[ModelComponentId]] = fcWithSuccessors.map { case (k, v) => k.id -> v }.toMap

    val fcWithExprs: List[(FormComponent, Expr)] = fcWithSuccessors.map(_._1).collect {
      case fc @ HasExpr(expr) if fc.editable || fc.submissible => fc -> expr
    }

    fcWithExprs
      .map {
        case (formComponent, expr) =>
          toJavascriptFn(formComponent, expr, successorLookup, formModelOptics)
      }
      .mkString("\n") +
      """|function getValue(elementId, isHidden) {
         |   var el = document.getElementById(elementId);
         |   var isVisible = !isHidden();
         |   if (el && isVisible) {
         |     return getNumber(el.value.replace(/[£,]/g,''));
         |   } else {
         |     return 0;
         |   };
         |};
         |
         |function getNumber(value) {
         |  if (value == ""){
         |    return 0;
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
         |function isElementHidden(elementId) {
         |  var classList = document.getElementById(elementId).parentNode.parentNode.parentNode.parentNode.classList;
         |  return classList.contains("govuk-radios__conditional--hidden") || classList.contains("govuk-checkboxes__conditional--hidden");
         |}
         |""".stripMargin
  }

  private def toJavascriptFn(
    field: FormComponent,
    expr: Expr,
    successorLookup: Map[FormComponentId, Set[ModelComponentId]],
    formModelOptics: FormModelOptics[DataOrigin.Mongo]
  ): String = {

    val elementId = field.id
    val functionName = JsFunction("compute" + elementId)

    val isDependent: Set[ModelComponentId] = successorLookup.get(field.id).getOrElse(Set.empty)

    def getRoundingMode(fc: FormComponent) =
      fc.`type` match {
        case Text(Number(_, _, rm, _), _, _, _, _, _)         => Some(rm)
        case TextArea(Number(_, _, rm, _), _, _, _)           => Some(rm)
        case Text(PositiveNumber(_, _, rm, _), _, _, _, _, _) => Some(rm)
        case TextArea(PositiveNumber(_, _, rm, _), _, _, _)   => Some(rm)
        case Text(s: Sterling, _, _, _, _, _)                 => Some(s.roundingMode)
        case TextArea(s: Sterling, _, _, _)                   => Some(s.roundingMode)
        case _                                                => None
      }

    val roundingMode = getRoundingMode(field)

    def computeExpr(expr: Expr): String = {
      def compute(operation: String, left: Expr, right: Expr) =
        s"$operation(${computeExpr(left)}, ${computeExpr(right)})"

      def sumCalc(id: FormComponentId, sum: Sum) = {
        val sumFcIds: Set[ModelComponentId] = successorLookup.values.flatten.toList
          .filter(_.baseComponentId === id.baseComponentId)
          .toSet // Remove duplicates
        if (sumFcIds.isEmpty) {
          formModelOptics.formModelVisibilityOptics
            .evalAndApplyTypeInfoFirst(sum)
            .numberRepresentation
            .map(_.toInt)
            .getOrElse(0)
            .toString
        } else {
          // This make sense only for sum over a Group field
          val sumExpr = sumFcIds.map(x => FormCtx(x.toFormComponentId)).foldLeft(Expr.additionIdentity)(Add)
          computeExpr(sumExpr)
        }
      }

      expr match {
        case FormCtx(id) =>
          val modelComponentId = id.modelComponentId
          if (isDependent(modelComponentId)) {
            s"""getValue("$id", isHidden$id)"""
          } else {
            formModelOptics.formModelVisibilityOptics.data.one(modelComponentId).getOrElse("0")
          }
        case Constant(amount)       => amount
        case Add(a, b)              => compute("add", a, b)
        case Subtraction(a, b)      => compute("subtract", a, b)
        case Multiply(a, b)         => compute("multiply", a, b)
        case sum @ Sum(FormCtx(id)) => sumCalc(id, sum)
        case otherwise              => ""
      }
    }

    def listeners(functionName: JsFunction) = {

      val componentEls = isDependent.map { modelComponentId =>
        val id = modelComponentId.toMongoIdentifier

        val isChild =
          formModelOptics.formModelRenderPageOptics.rcLookup.isRevealingChoiceChild(modelComponentId.toFormComponentId)

        val isHidden =
          if (isChild) {
            s"""|var isHidden$id = function () {
                |  return isElementHidden("$id");
                |};
                |
                |var element = document.getElementById("$id");
                |if (element) {
                |  var check = element.dataset.checkbox;
                |  if(check) {
                |    var checkbox = document.getElementById(check);
                |    if (checkbox) {
                |      checkbox.addEventListener("change",$functionName);
                |    }
                |  }
                |}
                |""".stripMargin
          } else {
            s"""|var isHidden$id = function () {
                |  return false;
                |};""".stripMargin
          }

        s"""|var element$id = document.getElementById("$id");
            |if (element$id) {
            |  element$id.addEventListener("change",$functionName);
            |  element$id.addEventListener("keyup",$functionName);
            |}
            |$isHidden
            |""".stripMargin
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

    // format: off
    s"""|function $functionName() {
     |  var numberOfDecimalPlaces = ${roundToCtx(field)};
     |  var roundingMode = BigNumber.$elementRoundingMode;
     |  var result = BigNumber(${computeExpr(expr)}).decimalPlaces(numberOfDecimalPlaces, roundingMode);
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
     |""".stripMargin
    // format: on
  }

  def roundToCtx(fc: FormComponent): Int = extractMaxFractionalDigits(fc).maxDigits

}
